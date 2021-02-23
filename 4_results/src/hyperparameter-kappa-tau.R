source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_DISPLAY_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)
library(patchwork)

N_REGIONS_TO_INCLUDE <- 11

parser <- ArgumentParser()
parser$add_argument('--mcmc-samples-lg')
parser$add_argument('--mcmc-samples-ln')
parser$add_argument('--output')
args <- parser$parse_args()


as_df <- function(samples) {
  n_iterations <- nrow(samples$beta)
  bind_rows(
    data.frame(
      variable = 'kappa',
      region = rep(seq_len(N_REGIONS_TO_INCLUDE), each = n_iterations),
      value = as.vector(samples$a[, seq_len(N_REGIONS_TO_INCLUDE)])
    ),
    data.frame(
      variable = 'tau_w',
      region = rep(seq_len(N_REGIONS_TO_INCLUDE), each = n_iterations),
      value = as.vector(
        (samples$w / (1 - samples$a ^ 2))[, seq_len(N_REGIONS_TO_INCLUDE)]
      )
    )
  )
}

samples_summary_df <- bind_rows(
  readRDS(args$mcmc_samples_lg) %>%
    as_df() %>%
    mutate(group = 'LG'),
  readRDS(args$mcmc_samples_ln) %>%
    as_df() %>%
    mutate(group = 'LN')
) %>%
  group_by(group, variable, region) %>%
  summarise(
    mean = mean(value),
    q025 = quantile(value, probs = 0.025),
    q500 = quantile(value, probs = 0.500),
    q975 = quantile(value, probs = 0.975)
  ) %>%
  mutate(
    region_name = sprintf('T%02d', region)
  )

plot_summary <- function(df) {
  ggplot(
    df,
    aes(region_name, colour = group)
  ) +
    geom_pointrange(
      mapping = aes(
        y = mean,
        ymin = q025,
        ymax = q975
      ),
      shape = 16,
      position = position_dodge(width = 0.5)
    ) +
    scale_colour_manual(
      values = c(
        'LG' = get_colour('wombat_lg'),
        'LN' = get_colour('wombat_ln')
      )
    ) +
    labs(x = NULL, colour = NULL)
}

kappa_plot <- samples_summary_df %>%
  filter(variable == 'kappa') %>%
  plot_summary() +
    labs(y = expression(kappa[j])) +
    theme(axis.text.x = element_blank())

tau_plot <- samples_summary_df %>%
  filter(variable == 'tau_w') %>%
  plot_summary() +
    scale_y_log10() +
    annotation_logticks(sides = 'l') +
    labs(y = expression(tau['w,j'])) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

output <- kappa_plot /
  tau_plot +
  plot_layout(guides = 'collect')

ggsave(
  args$output,
  plot = output,
  width = DISPLAY_SETTINGS$full_width,
  height = 7,
  units = 'cm'
)
