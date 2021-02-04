source(Sys.getenv('INVERSION_BASE_PARTIAL'))
source(Sys.getenv('UTILS_PARTIAL'))
library(ncdf4)
library(parallel)

n_unique <- function(x) length(unique(x))
all_same <- function(x) all(x == x[1])

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--control', '') %>%
  add_argument('--matched-runs-directory', '') %>%
  add_argument('--cores', '', default = get_cores()) %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading {args$control}')
control <- fst::read_fst(args$control) %>%
  filter(resolution != 'daily')

run_paths <- get_run_paths(args$matched_runs_directory, include_base = FALSE)

log_info('Loading {length(run_paths)}')
anomalies_parts <- mclapply(run_paths, function(run_path) {
  combined_path <- file.path(run_path$full_path, 'combined.nc')
  log_debug('Loading {combined_path}')
  with_nc_file(list(fn = combined_path), {
    v <- function(...) ncvar_get(fn, ...)
    a <- function(name) as.integer(ncatt_get_quiet(fn, 0, name)$value)

    name <- stringr::str_match(
      run_path$short_path,
      r'(run\.v(\d+\.\d+\.\d+)\.(\w+)\.(\w+))'
    )[4]

    tibble(
      name,
      resolution = factor(v('resolution'), levels = c('obspack', 'hourly', 'daily')),
      observation_id = as.vector(v('observation_id')),
      observation_type = factor(v('observation_type')),
      co2 = as.vector(v('co2')),
    ) %>%
      filter(resolution != 'daily') %>%
      inner_join(
        control %>% select(
          model_id,
          observation_id,
          observation_type,
          control_co2 = co2
        ),
        by = c('observation_id', 'observation_type')
      ) %>%
      mutate(
        co2_anomaly = co2 - control_co2
      ) %>%
      select(
        name,
        model_id,
        co2_anomaly
      )
  })
}, mc.cores = args$cores)

stopifnot(!any(sapply(anomalies_parts, is.null)))
for (result in anomalies_parts) {
  if (is(result, 'try-error')) {
    print(result)
    stop('failed')
  }
}

log_info('Combining parts')
anomalies <- bind_rows(anomalies_parts) %>%
  arrange(name, model_id)

log_info('Checking internal consistency')
stopifnot(all(is.finite(anomalies$co2_anomaly)))

log_info('Saving to {args$output}')
fst::write_fst(anomalies, args$output)

log_info('Done')
