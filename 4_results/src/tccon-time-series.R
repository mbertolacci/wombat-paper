source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_DISPLAY_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)
library(tidyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--tccon-samples')
parser$add_argument('--output')
args <- parser$parse_args()

NAME_COLOURS <- c(
  'Observed' = 'black',
  'WOMBAT Prior (mean)' = get_colour('wombat_prior'),
  'WOMBAT LG (mean, 95% cred. int.)' = get_colour('wombat_lg'),
  'WOMBAT LN (mean, 95% cred. int.)' = get_colour('wombat_ln')
)

tccon_samples <- readRDS(args$tccon_samples) %>%
  filter(variant == 'Correlated') %>%
  mutate(month = lubridate::floor_date(time, 'month'))

tccon_standard_deviations <- tccon_samples %>%
  group_by(observation_group, station) %>%
  summarise(
    # NOTE(mgnb): we assume perfect correlation
    Y2_tilde_sd = sqrt(mean(co2_error ^ 2))
  ) %>%
  ungroup()

tccon_time_series_monthly <- tccon_samples %>%
  group_by(observation_group, station, month) %>%
  summarise(
    co2 = mean(co2),
    Y2_prior = mean(Y2_prior),
    Y2_tilde_samples = t(colMeans(Y2_tilde_samples))
  ) %>%
  ungroup() %>%
  left_join(
    tccon_standard_deviations,
    by = c('observation_group', 'station')
  ) %>%
  mutate(
    Y2 = Y2_prior + rowMeans(Y2_tilde_samples),
    Z2_tilde_samples = Y2_tilde_samples + rnorm(matrix(
      rnorm(n() * ncol(Y2_tilde_samples), sd = Y2_tilde_sd),
      nrow = n()
    )),
    Z2_lower = Y2_prior + matrixStats::rowQuantiles(Z2_tilde_samples, probs = 0.025),
    Z2_upper = Y2_prior + matrixStats::rowQuantiles(Z2_tilde_samples, probs = 0.975)
  ) %>%
  select(-Z2_tilde_samples)

df_long <- bind_rows(
  tccon_time_series_monthly %>%
    filter(observation_group == 'LG') %>%
    select(station, month, value = co2) %>%
    mutate(name = 'Observed', lower = NA, upper = NA),
  tccon_time_series_monthly %>%
    filter(observation_group == 'LG') %>%
    select(station, month, value = Y2_prior) %>%
    mutate(name = 'WOMBAT Prior (mean)', lower = NA, upper = NA),
  tccon_time_series_monthly %>%
    mutate(name = sprintf('WOMBAT %s (mean, 95%% cred. int.)', observation_group)) %>%
    select(name, station, month, value = Y2, lower = Z2_lower, upper = Z2_upper)
)

df_complete <- expand.grid(
  name = names(NAME_COLOURS),
  month = sort(unique(df_long$month)),
  station = sort(unique(df_long$station))
) %>%
  left_join(df_long, by = c('name', 'month', 'station'))

output <- df_complete %>%
  mutate(
    name = factor(name, levels = names(NAME_COLOURS)),
    # NOTE(mgnb): reverse the order of the factors to control the order of
    # panels
    station = factor(station, levels = rev(TCCON_ORDER))
  ) %>%
  ggplot(aes(month)) +
    geom_ribbon(
      mapping = aes(ymin = lower, ymax = upper, fill = name, colour = name),
      alpha = 0.2,
      size = 0.1
    ) +
    geom_line(
      mapping = aes(y = value, colour = name),
      size = 0.3
    ) +
    scale_colour_manual(values = NAME_COLOURS) +
    scale_fill_manual(values = NAME_COLOURS) +
    facet_wrap(~ station, scales = 'free_y', ncol = 4) +
    labs(
      x = 'Month',
      y = 'Mole fraction [ppm]',
      colour = NULL,
      fill = NULL
    ) +
    guides(
      colour = guide_legend(ncol = 3),
      fill = guide_legend(ncol = 3)
    ) +
    theme(
      legend.position = 'bottom',
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width,
  height = DISPLAY_SETTINGS$full_height - 6
)
