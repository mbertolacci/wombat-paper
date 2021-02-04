source(Sys.getenv('INVERSION_BASE_PARTIAL'))
source(Sys.getenv('UTILS_PARTIAL'))
library(ncdf4)
library(parallel)

options(dplyr.summarise.inform = FALSE)

n_unique <- function(x) length(unique(x))
all_same <- function(x) all(x == x[1])

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--control', '') %>%
  add_argument('--matched-runs-directory', '') %>%
  add_argument('--cores', '', default = get_cores()) %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading {args$control}')
control <- fst::read_fst(args$control)

run_paths <- get_run_paths(args$matched_runs_directory, include_base = FALSE)
log_info('Loading {length(run_paths)} tracers and computing sensitivities')
sensitivities_parts <- mclapply(run_paths, function(run_path) {
  combined_path <- file.path(args$matched_runs_directory, run_path, 'combined.nc')
  log_debug('Loading {combined_path}')
  with_nc_file(list(fn = combined_path), {
    v <- function(...) ncvar_get(fn, ...)
    a <- function(name) as.integer(ncatt_get_quiet(fn, 0, name)$value)

    region <- a('region')
    year <- a('year')
    month <- a('month')

    tibble(
      region = region,
      from_month_start = lubridate::ymd(sprintf('%04d-%02d-01', year, month)),
      observation_id = as.vector(v('observation_id')),
      observation_type = factor(v('observation_type')),
      resolution = factor(v('resolution'), levels = c('obspack', 'hourly', 'daily')),
      co2 = as.vector(v('co2')),
    ) %>%
      inner_join(
        control %>% select(
          model_id,
          observation_id,
          observation_type,
          resolution,
          control_co2 = co2
        ),
        by = c('observation_id', 'observation_type', 'resolution')
      ) %>%
      mutate(
        co2_sensitivity = co2 - control_co2
      ) %>%
      select(
        region,
        from_month_start,
        model_id,
        resolution,
        co2_sensitivity
      ) %>%
      arrange(model_id, resolution)
  })
}, mc.cores = args$cores)

stopifnot(!any(sapply(sensitivities_parts, is.null)))
for (result in sensitivities_parts) {
  if (is(result, 'try-error')) {
    print(result)
    stop('failed')
  }
}

# HACK(mgnb): correction for stupidity
sensitivities_parts <- mclapply(sensitivities_parts, function(part) {
  region <- part$region[1]
  from_month_start <- part$from_month_start[1]
  log_debug('Correcting {region} {from_month_start}')

  correction <- NULL
  for (part_inner in sensitivities_parts) {
    region_inner <- part_inner$region[1]
    from_month_start_inner <- part_inner$from_month_start[1]

    if (
      region_inner == region
      && month(from_month_start_inner) == month(from_month_start)
      && from_month_start_inner > from_month_start
    ) {
      log_trace('Correcting with {region_inner} {from_month_start_inner}')
      correction <- part_inner
      break
    }
  }

  if (is.null(correction)) {
    return(part)
  }

  part %>%
    left_join(
      correction %>% select(model_id, resolution, co2_sensitivity_correction = co2_sensitivity),
      by = c('model_id', 'resolution')
    ) %>%
    mutate(
      co2_sensitivity_correction = ifelse(
        is.na(co2_sensitivity_correction),
        0,
        co2_sensitivity_correction
      ),
      co2_sensitivity = co2_sensitivity - co2_sensitivity_correction
    ) %>%
    select(-co2_sensitivity_correction)
}, mc.cores = args$cores)

log_info('Combining parts')
sensitivities <- bind_rows(sensitivities_parts) %>%
  arrange(region, from_month_start, model_id, resolution)

log_info('TO REMOVE: replacing NaN\'s with 0')
sensitivities$co2_sensitivity <- ifelse(
  is.nan(sensitivities$co2_sensitivity),
  0,
  sensitivities$co2_sensitivity
)

log_info('Checking internal consistency')
stopifnot(all(is.finite(sensitivities$co2_sensitivity)))

sensitivities %>%
  group_by(region) %>%
  summarise(
    n_from_month_starts = n_unique(from_month_start)
  ) %>%
  with(stopifnot(all_same(n_from_month_starts)))

sensitivities %>%
  group_by(from_month_start) %>%
  summarise(
    n_regions = n_unique(region)
  ) %>%
  with(stopifnot(all_same(n_regions)))

sensitivities %>%
  group_by(from_month_start, region) %>%
  summarise(
    n_observations = n()
  ) %>%
  ungroup() %>%
  group_by(from_month_start) %>%
  summarise(
    n_observations_equal = all_same(n_observations)
  ) %>%
  with(stopifnot(all(n_observations_equal)))

log_info('Saving to {args$output}')
fst::write_fst(sensitivities, args$output)

log_info('Done')
