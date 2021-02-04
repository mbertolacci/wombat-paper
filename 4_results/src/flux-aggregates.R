source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_DISPLAY_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)
library(lubridate, warn.conflicts = FALSE)
library(patchwork)

options(dplyr.summarise.inform = FALSE)

parser <- ArgumentParser()
parser$add_argument('--region', nargs = '+')
parser$add_argument('--height', type = 'double')
parser$add_argument('--mip-fluxes')
parser$add_argument('--show-mip-fluxes', action = 'store_true', default = FALSE)
parser$add_argument('--flux-samples-lg')
parser$add_argument('--flux-samples-ln')
parser$add_argument('--show-prior-uncertainty', action = 'store_true', default = FALSE)
parser$add_argument('--output')
args <- parser$parse_args()

flux_samples <- bind_rows(
  readRDS(args$flux_samples_lg) %>%
    mutate(
      is_prior = estimate == 'Prior',
      observation_group = ifelse(
        is_prior,
        'WOMBAT Prior',
        'WOMBAT LG'
      ),
    ),
  readRDS(args$flux_samples_ln) %>%
    filter(
      estimate != 'Prior'
    ) %>%
    mutate(
      is_prior = FALSE,
      observation_group = 'WOMBAT LN'
    )
)

legend_n_columns <- 3
show_prior_uncertainty <- FALSE
show_mip_fluxes <- args$show_mip_fluxes
source(Sys.getenv('RESULTS_FLUX_AGGREGATES_PARTIAL'))
