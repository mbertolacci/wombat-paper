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
parser$add_argument('--flux-samples-lg')
parser$add_argument('--flux-samples-ln')
parser$add_argument('--flux-samples-lg-online-corrected')
parser$add_argument('--flux-samples-ln-online-corrected')
parser$add_argument('--output')
args <- parser$parse_args()

flux_samples <- bind_rows(
  readRDS(args$flux_samples_lg) %>%
    mutate(
      is_prior = estimate == 'Prior',
      observation_group = ifelse(
        is_prior,
        'Prior',
        'LG, offline-corr.'
      )
    ),
  readRDS(args$flux_samples_ln) %>%
    filter(estimate != 'Prior') %>%
    mutate(
      is_prior = FALSE,
      observation_group = 'LN, offline-corr.'
    ),
  readRDS(args$flux_samples_lg_online_corrected) %>%
    filter(estimate != 'Prior') %>%
    mutate(
      is_prior = FALSE,
      observation_group = 'LG, online-corr.'
    ),
  readRDS(args$flux_samples_ln_online_corrected) %>%
    filter(estimate != 'Prior') %>%
    mutate(
      is_prior = FALSE,
      observation_group = 'LN, online-corr.'
    ),
)

show_mip_fluxes <- FALSE
show_prior_uncertainty <- FALSE
legend_n_columns <- 2
small_y_axes <- FALSE
source(Sys.getenv('RESULTS_FLUX_AGGREGATES_PARTIAL'))
