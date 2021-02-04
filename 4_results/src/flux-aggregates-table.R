source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)

parser <- ArgumentParser()
parser$add_argument('--flux-samples-lg')
parser$add_argument('--flux-samples-ln')
parser$add_argument('--mip-fluxes')
parser$add_argument('--output')
args <- parser$parse_args()

log_info('Loading flux samples')
flux_samples <- bind_rows(
  readRDS(args$flux_samples_lg) %>%
    mutate(observation_group = 'LG'),
  readRDS(args$flux_samples_ln) %>%
    mutate(observation_group = 'LN') %>%
    filter(estimate != 'Prior')
) %>%
  mutate(
    year = year(month_start),
    estimate = ifelse(
      estimate == 'Prior',
      'WOMBAT Prior',
      sprintf('WOMBAT %s', observation_group)
    )
  ) %>%
  select(-observation_group)

log_info('Loading MIP fluxes')
mip_fluxes <- fst::read_fst(args$mip_fluxes)

mip_fluxes_modified <- mip_fluxes %>%
  mutate(
    name = MIP_REGION_TO_REGION[region_name]
  ) %>%
  filter(
    month_start >= '2015-01-01',
    month_start < '2017-01-01',
    type != 'fossil',
    case %in% c('Prior', 'LG', 'LN')
  ) %>%
  select(name, which = case, everything())

mip_fluxes_modified <- bind_rows(c(list(mip_fluxes_modified), lapply(
  REGION_AGGREGATES,
  function(region_aggregate) {
    mip_fluxes_modified %>%
      filter(
        name %in% region_aggregate$parts,
        type %in% region_aggregate$types
      ) %>%
      group_by(group, which, type, month_start) %>%
      summarise(
        flux = sum(flux)
      ) %>%
      ungroup() %>%
      mutate(name = region_aggregate$name)
  }
))) %>%
  mutate(
    year = year(month_start),
    estimate = sprintf('MIP %s', which)
  ) %>%
  select(-which)

log_info('Calculating')
annual_fluxes <- flux_samples %>%
  group_by(estimate, name, year) %>%
  summarise(
    flux_mean = sum(flux_mean),
    flux_lower = quantile(colSums(flux_samples), probs = 0.025, na.rm = TRUE),
    flux_upper = quantile(colSums(flux_samples), probs = 0.975, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(
    name %in% MIP_REGION_TO_REGION,
    year %in% c(2015, 2016)
  )

annual_mip_fluxes <- mip_fluxes_modified %>%
  group_by(group, estimate, name, year) %>%
  summarise(
    flux = sum(flux)
  ) %>%
  ungroup() %>%
  group_by(estimate, name, year) %>%
  summarise(
    flux_mean = mean(flux, na.rm = TRUE),
    flux_lower = min(flux, na.rm = TRUE),
    flux_upper = max(flux, na.rm = TRUE)
  ) %>%
  filter(
    year %in% c(2015, 2016)
  )

monthly_fluxes <- flux_samples %>%
  filter(
    name %in% MIP_REGION_TO_REGION,
    month_start >= '2015-01-01',
    month_start < '2017-01-01'
  ) %>%
  ungroup() %>%
  mutate(
    flux_lower = matrixStats::rowQuantiles(flux_samples, probs = 0.025),
    flux_upper = matrixStats::rowQuantiles(flux_samples, probs = 0.975)
  ) %>%
  select(estimate, name, month_start, flux_mean, flux_lower, flux_upper)

monthly_mip_fluxes <- mip_fluxes_modified %>%
  group_by(group, estimate, name, month_start) %>%
  summarise(
    flux = sum(flux)
  ) %>%
  ungroup() %>%
  group_by(estimate, name, month_start) %>%
  summarise(
    flux_mean = mean(flux, na.rm = TRUE),
    flux_lower = min(flux, na.rm = TRUE),
    flux_upper = max(flux, na.rm = TRUE)
  )

sink(args$output)

cat('============ Annual fluxes\n')
bind_rows(
  annual_fluxes,
  annual_mip_fluxes
) %>%
  arrange(year, name, estimate) %>%
  knitr::kable(digits = 2)

cat('\n\n\n============ Average over the two years:\n')
flux_samples %>%
  filter(
    year %in% c(2015, 2016),
    name %in% c('Global', 'Global land'),
    estimate %in% c('WOMBAT LG', 'WOMBAT LN')
  ) %>%
  group_by(name, estimate) %>%
  summarise(
    flux_mean = mean(colSums(flux_samples) / 2),
    flux_sd = sd(colSums(flux_samples) / 2),
    flux_q025 = quantile(colSums(flux_samples) / 2, probs = 0.025),
    flux_q975 = quantile(colSums(flux_samples) / 2, probs = 0.975)
  ) %>%
  knitr::kable(digits = 2)

cat('\n\n\n============ Difference between LG and LN mean annual flux for some regions:\n')
flux_samples %>%
  filter(
    year %in% c(2015, 2016),
    name == 'N extratropics (23.5 - 90)',
    estimate %in% c('WOMBAT LG', 'WOMBAT LN')
  ) %>%
  mutate(
    estimate = c(
      'WOMBAT LG' = 'LG',
      'WOMBAT LN' = 'LN'
    )[as.character(estimate)]
  ) %>%
  group_by(name, estimate) %>%
  summarise(
    flux_samples = t(colSums(flux_samples)) / 2
  ) %>%
  pivot_wider(names_from = estimate, values_from = flux_samples) %>%
  mutate(
    difference_samples = LG - LN,
    difference_q025 = quantile(difference_samples, probs = 0.025),
    difference_q975 = quantile(difference_samples, probs = 0.975)
  ) %>%
  select(-LG, -LN, -difference_samples) %>%
  knitr::kable(digits = 2)

cat('\n\n\n============ Monthly fluxes:\n')
bind_rows(
  monthly_fluxes,
  monthly_mip_fluxes
) %>%
  arrange(month_start, name, estimate) %>%
  knitr::kable(digits = 2)

sink(NULL)
