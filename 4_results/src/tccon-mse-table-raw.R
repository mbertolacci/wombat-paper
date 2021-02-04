source(Sys.getenv('RESULTS_BASE_PARTIAL'))
library(argparse)

parser <- ArgumentParser()
parser$add_argument('--tccon-samples')
parser$add_argument('--mip-tccon')
parser$add_argument('--output')
args <- parser$parse_args()

tccon_samples <- readRDS(args$tccon_samples) %>%
  select(-Y2_tilde_samples)

mip_tccon <- fst::read_fst(args$mip_tccon)

sink(args$output)
bind_rows(
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
      group = sprintf('WOMBAT Post. (%s)', variant)
    ) %>%
    select(-variant),
  tccon_samples %>%
    filter(variant == 'Uncorrelated') %>%
    select(
      station = station,
      time = time,
      case = observation_group,
      model = Y2_prior,
      observed = co2
    ) %>%
    mutate(
      group = 'WOMBAT Prior'
    )
) %>%
  group_by(case, group, station) %>%
  summarise(
    bias = mean(model - observed, na.rm = TRUE),
    stdev = sd(model - observed, na.rm = TRUE),
    mse = mean((model - observed) ^ 2, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(case, group) %>%
  summarise(
    avg_mse = mean(mse)
  ) %>%
  knitr::kable(digits = 3)
sink(NULL)
