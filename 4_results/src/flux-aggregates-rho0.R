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
parser$add_argument('--categories', nargs = '+')
parser$add_argument('--flux-samples-lg')
parser$add_argument('--flux-samples-ln')
parser$add_argument('--flux-samples-lg-uncorrelated')
parser$add_argument('--flux-samples-ln-uncorrelated')
parser$add_argument('--output')
args <- parser$parse_args()


flux_samples <- bind_rows(
  readRDS(args$flux_samples_lg) %>%
    mutate(
      is_prior = estimate == 'Prior',
      observation_group = ifelse(
        is_prior,
        'Prior',
        'LG, correlated errors'
      )
    ),
  readRDS(args$flux_samples_ln) %>%
    filter(estimate != 'Prior') %>%
    mutate(
      is_prior = FALSE,
      observation_group = 'LN, correlated errors'
    ),
  readRDS(args$flux_samples_lg_uncorrelated) %>%
    filter(estimate != 'Prior') %>%
    mutate(
      is_prior = FALSE,
      observation_group = 'LG, uncorrelated errors'
    ),
  readRDS(args$flux_samples_ln_uncorrelated) %>%
    filter(estimate != 'Prior') %>%
    mutate(
      is_prior = FALSE,
      observation_group = 'LN, uncorrelated errors'
    ),
)

legend_n_columns <- 3
show_prior_uncertainty <- FALSE
categories <- args$categories
small_y_axes <- FALSE
source(Sys.getenv('RESULTS_FLUX_AGGREGATES_PARTIAL'))
