source(Sys.getenv('INVERSION_BASE_PARTIAL'))
source(Sys.getenv('UTILS_PARTIAL'))
library(ncdf4)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--matched-runs-directory', '') %>%
  add_argument('--obspack-directory', '', nargs = Inf) %>%
  add_argument('--start-date', '') %>%
  add_argument('--end-date', '') %>%
  add_argument('--cores', '', default = get_cores()) %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading ObsPack observations')
obspack_observations <- load_obspack_observations(
  args$obspack_directory,
  ymd(args$start_date),
  ymd(args$end_date),
  args$cores
) %>%
  filter(!is.na(co2_error))

combined_path <- file.path(
  args$matched_runs_directory,
  'run.v12.3.2.base/combined.nc'
)
log_info('Loading {combined_path}')
with_nc_file(list(fn = combined_path), {
  v <- function(...) ncvar_get(fn, ...)
  control_full <- tibble(
    observation_id = v('observation_id'),
    observation_type = factor(v('observation_type')),
    resolution = factor(v('resolution'), levels = c('obspack', 'hourly', 'daily')),
    time = ncvar_get_time(fn, 'time'),
    latitude = v('observation_latitude'),
    longitude = v('observation_longitude'),
    co2 = v('co2'),
  ) %>%
    filter(
      observation_type != 'obspack'
      | (observation_id %in% obspack_observations$obspack_id),
      time >= ymd(args$start_date),
      time < ymd(args$end_date)
    ) %>%
    arrange(time)


  control_not_hourly <- control_full %>%
    filter(resolution != 'hourly')
  control_not_daily <- control_full %>%
    filter(
      resolution != 'daily',
      sprintf('%s_%s', observation_id, observation_type)
        %in% sprintf('%s_%s', control_not_hourly$observation_id, control_not_hourly$observation_type)
    ) %>%
    mutate(
      model_id = 1 : n()
    )

  control <- control_full %>%
    filter(
      observation_id %in% control_not_daily$observation_id
    ) %>%
    left_join(
      control_not_daily %>% select(observation_id, model_id),
      by = 'observation_id'
    ) %>%
    arrange(model_id)
})

log_info('Checking internal consistency')
stopifnot(all(is.finite(control$co2)))

log_info('Saving to {args$output}')
fst::write_fst(control, args$output)

log_info('Done')
