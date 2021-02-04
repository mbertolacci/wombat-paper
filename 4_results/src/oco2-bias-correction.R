source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_DISPLAY_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)
library(tidyr, warn.conflicts = FALSE)
library(wombat)

parser <- ArgumentParser()
parser$add_argument('--mcmc-samples-lg-online-corrected')
parser$add_argument('--mcmc-samples-ln-online-corrected')
parser$add_argument('--mcmc-samples-lg-offline-online-corrected')
parser$add_argument('--mcmc-samples-ln-offline-online-corrected')
parser$add_argument('--output')
args <- parser$parse_args()

read_samples <- function(filename, mode_i, raw_i) {
  window(readRDS(filename), start = 1001)$beta %>%
    as.data.frame() %>%
    pivot_longer(everything()) %>%
    mutate(
      mode = mode_i,
      raw = raw_i
    )
}

beta_samples_df <- bind_rows(
  read_samples(
    args$mcmc_samples_lg_online_corrected,
    'WOMBAT LG',
    'Uncorrected retrievals'
  ),
  read_samples(
    args$mcmc_samples_ln_online_corrected,
    'WOMBAT LN',
    'Uncorrected retrievals'
  ),
  read_samples(
    args$mcmc_samples_lg_offline_online_corrected,
    'WOMBAT LG',
    'TCCON-corrected retrievals'
  ),
  read_samples(
    args$mcmc_samples_ln_offline_online_corrected,
    'WOMBAT LN',
    'TCCON-corrected retrievals'
  )
) %>%
  mutate(
    parameter = factor(c(
      'is_oco2:oco2_operation_modeLG' = '(Intercept)',
      'is_oco2:oco2_operation_modeLG:oco2_dp' = 'dp',
      'is_oco2:oco2_operation_modeLG:oco2_co2_grad_del' = 'co2_grad_del',
      'is_oco2:oco2_operation_modeLG:oco2_log_dws' = 'logDWS',
      'is_oco2:oco2_operation_modeLN' = '(Intercept)',
      'is_oco2:oco2_operation_modeLN:oco2_dp' = 'dp',
      'is_oco2:oco2_operation_modeLN:oco2_co2_grad_del' = 'co2_grad_del',
      'is_oco2:oco2_operation_modeLN:oco2_log_dws' = 'logDWS'
    )[name], levels = c('dp', 'co2_grad_del', 'logDWS')),
    mode = factor(
      mode,
      levels = c(
        'WOMBAT LG',
        'WOMBAT LN',
        'TCCON-based offline correction'
      )
    ),
    value = -value
  ) %>%
  filter(parameter != '(Intercept)') %>%
  mutate(
    value = ifelse(
      mode == 'WOMBAT LG',
      value / c(
        'dp' = 3.057625,
        'co2_grad_del' = 21.348496,
        'logDWS' = 1.135891
      )[parameter],
      ifelse(
        mode == 'WOMBAT LN',
        value / c(
          'dp' = 2.758201,
          'co2_grad_del' = 19.829637,
          'logDWS' = 1.144667
        )[parameter],
        value
      )
    )
  )

output1 <- ggplot() +
  geom_vline(
    data = tibble(
      parameter = factor(
        c('dp', 'co2_grad_del', 'logDWS'),
        levels = c('dp', 'co2_grad_del', 'logDWS')
      ),
      value = c(0.3, 0.028, 0.6)
    ),
    mapping = aes(xintercept = value),
    colour = 'blue',
    size = 1
  ) +
  geom_density(
    data = beta_samples_df %>% filter(raw == 'Uncorrected retrievals'),
    mapping = aes(value, colour = mode)
  ) +
  scale_colour_manual(
    values = c(
      'WOMBAT LG' = get_colour('wombat_lg'),
      'WOMBAT LN' = get_colour('wombat_ln'),
      'TCCON-based offline correction' = 'blue'
    ),
    drop = FALSE,
    guide = guide_legend(override.aes = list(size = 1))
  ) +
  labs(x = 'Bias correction coefficient', y = 'Density', colour = NULL) +
  facet_wrap(~ parameter, scales = 'free') +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  ggtitle('Uncorrected retrievals') +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = 'bottom'
  )

output2 <- ggplot() +
  geom_vline(
    xintercept = 0,
    colour = '#999999',
    linetype = 2,
    size = 1
  ) +
  geom_density(
    data = beta_samples_df %>% filter(raw == 'TCCON-corrected retrievals'),
    mapping = aes(value, colour = mode)
  ) +
  scale_colour_manual(
    values = c(
      'WOMBAT LG' = get_colour('wombat_lg'),
      'WOMBAT LN' = get_colour('wombat_ln'),
      'TCCON-based offline correction' = 'blue'
    ),
    drop = FALSE,
    guide = guide_legend(override.aes = list(size = 1))
  ) +
  labs(x = 'Bias correction coefficient', y = 'Density', colour = NULL) +
  facet_wrap(~ parameter, scales = 'free') +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  ggtitle('TCCON-corrected retrievals') +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = 'bottom'
  )

output <- gridExtra::arrangeGrob(
  output1 + theme(legend.position = 'none'),
  output2 + theme(legend.position = 'none'),
  get_legend(output1),
  heights = c(5.5, 5.5, 1)
)

ggsave(
  args$output,
  plot = output,
  width = DISPLAY_SETTINGS$full_width,
  height = 12,
  units = 'cm'
)
