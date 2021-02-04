source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)

parser <- ArgumentParser()
parser$add_argument('--tccon-samples-lg')
parser$add_argument('--tccon-samples-ln')
parser$add_argument('--tccon-samples-lg-uncorrelated')
parser$add_argument('--tccon-samples-ln-uncorrelated')
parser$add_argument('--output')
args <- parser$parse_args()

tccon_time_series <- bind_rows(
  readRDS(args$tccon_samples_lg) %>%
    mutate(observation_group = 'LG', variant = 'Correlated'),
  readRDS(args$tccon_samples_ln) %>%
    mutate(observation_group = 'LN', variant = 'Correlated'),
  readRDS(args$tccon_samples_lg_uncorrelated) %>%
    mutate(observation_group = 'LG', variant = 'Uncorrelated'),
  readRDS(args$tccon_samples_ln_uncorrelated) %>%
    mutate(observation_group = 'LN', variant = 'Uncorrelated')
) %>%
  filter(
    time >= '2015-01-01',
    time < '2017-05-01'
  ) %>%
  mutate(
    Y2 = Y2_prior + rowMeans(Y2_tilde_samples),
    station = factor(
      TCCON_CODE_TO_NAME[station],
      levels = TCCON_ORDER
    )
  ) %>%
  filter(
    station %in% MIP_TCCON_INCLUDED_STATIONS
  ) %>%
  select(
    observation_group,
    variant,
    observation_id,
    time,
    co2,
    co2_error,
    station,
    Y2_prior,
    Y2,
    Y2_tilde_samples
  )

saveRDS(tccon_time_series, args$output)
