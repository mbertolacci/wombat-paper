source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)

parser <- ArgumentParser()
parser$add_argument('--tccon-samples')
parser$add_argument('--mip-tccon')
parser$add_argument('--output')
args <- parser$parse_args()

tccon_samples <- readRDS(args$tccon_samples) %>%
  filter(variant == 'Correlated') %>%
  select(-Y2_tilde_samples)

mip_tccon <- fst::read_fst(args$mip_tccon)

metric_df <- bind_rows(
  tccon_samples %>%
    select(
      station = station,
      variant,
      time = time,
      case = observation_group,
      model = Y2,
      observed = co2
    ) %>%
    mutate(
      group = 'WOMBAT Post.'
    ) %>%
    select(-variant),
  tccon_samples %>%
    select(
      station = station,
      time = time,
      case = observation_group,
      model = Y2_prior,
      observed = co2
    ) %>%
    mutate(
      group = 'WOMBAT Prior'
    ),
  mip_tccon
) %>%
  group_by(case, group, station) %>%
  summarise(
    bias = mean(model - observed, na.rm = TRUE),
    stdev = sd(model - observed, na.rm = TRUE),
    mse = mean((model - observed) ^ 2, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(group = factor(group, MODEL_NAME_ORDER))

fst::write_fst(metric_df, args$output)
