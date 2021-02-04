source(Sys.getenv('INVERSION_BASE_PARTIAL'))
source(Sys.getenv('UTILS_PARTIAL'))
library(ncdf4)
library(stringr)
library(parallel)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--control', '') %>%
  add_argument('--oco2-observations', '') %>%
  add_argument('--tccon-observation-directory', '') %>%
  add_argument('--obspack-directory', '', nargs = Inf) %>%
  add_argument('--start-date', '') %>%
  add_argument('--end-date', '') %>%
  add_argument('--cores', '', default = get_cores()) %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading control')
control <- fst::read_fst(args$control)

log_info('Loading OCO-2 observations')

load_oco2 <- function(xco2_name) {
  with_nc_file(list(fn = args$oco2_observations), {
    v <- function(...) ncvar_get(fn, ...)
    sigma_levels <- v('sigma_levels')
    oco2_soundings <- tibble(
      sounding_id = v('sounding_id'),
      time = ncvar_get_time(fn, 'time'),
      longitude = v('longitude'),
      latitude = v('latitude'),
      co2 = v(xco2_name),
      co2_error = v('xco2_increased_uncert'),
      operation_mode_raw = v('operation_mode'),
      oco2_airmass = v('airmass'),
      oco2_co2_grad_del = v('co2_grad_del'),
      oco2_log_dws = v('logDWS'),
      oco2_dp = v('dp'),
      oco2_s31 = v('s31'),
    ) %>%
      filter(operation_mode_raw >= 0) %>%
      select(-operation_mode_raw)
  })
  oco2_soundings$oco2_operation_mode <- c(
    'LN', 'LG', 'LT', 'LTT', 'ON', 'OG', 'OT', 'OTT'
  )[oco2_soundings$sounding_id %% 10]

  oco2_soundings <- oco2_soundings %>%
    filter(oco2_operation_mode %in% c('LN', 'LG', 'OG'))

  oco2_soundings
}

oco2_soundings <- load_oco2('xco2_s31_corrected')
oco2_soundings_raw <- load_oco2('xco2_raw')

log_info('Loading TCCON observations')
tccon_paths <- list.files(args$tccon_observation_directory, full.names = TRUE)
tccon_soundings <- bind_rows(mclapply(tccon_paths, function(filename) {
  with_nc_file(list(fn = filename), {
    v <- function(name, ...) ncvar_get(fn, sprintf('CO2/%s', name), ...)

    time_parts <- v('cdate', collapse_degen = FALSE)
    tibble(
      sounding_id = as.vector(v('obs_num')),
      tccon_station_id = as.vector(v('station_id')),
      time = ymd_hms(sprintf(
        '%04d-%02d-%02d %02d:%02d:%02d',
        time_parts[1, ],
        time_parts[2, ],
        time_parts[3, ],
        time_parts[4, ],
        time_parts[5, ],
        time_parts[6, ]
      )),
      longitude = as.vector(v('longitude')),
      latitude = as.vector(v('latitude')),
      co2 = as.vector(v('column_mixing')),
      co2_error = as.vector(v('sigma_column_mixing')),
    )
  })
}, mc.cores = args$cores))

log_info('Loading ObsPack observations')
obspack_observations <- load_obspack_observations(
  args$obspack_directory,
  ymd(args$start_date),
  ymd(args$end_date),
  args$cores
)

log_info('Combining observations')
observations <- bind_rows(
  oco2_soundings %>%
    mutate(
      observation_id = sprintf('%f', sounding_id),
      observation_type = 'oco2'
    ) %>%
    select(-sounding_id),
  oco2_soundings_raw %>%
    mutate(
      observation_id = sprintf('%f', sounding_id),
      observation_type = 'oco2r'
    ) %>%
    select(-sounding_id),
  tccon_soundings %>%
    mutate(
      observation_id = sprintf('%f', sounding_id),
      observation_type = 'tccon'
    ) %>%
    select(-sounding_id),
  obspack_observations %>%
    mutate(
      observation_type = 'obspack'
    ) %>%
    select(
      observation_id = obspack_id,
      observation_type,
      time,
      longitude,
      latitude,
      altitude,
      co2,
      co2_error
    )
) %>%
  select(observation_id, observation_type, time, everything()) %>%
  filter(observation_id %in% control$observation_id) %>%
  mutate(
    overall_observation_mode = factor(ifelse(
      observation_type == 'oco2',
      oco2_operation_mode,
      ifelse(
        observation_type == 'oco2r',
        sprintf('%sr', oco2_operation_mode),
        c(
          'obspack' = 'IS',
          'tccon' = 'TC'
        )[observation_type]
      )
    )),
    observation_group = factor(ifelse(
      overall_observation_mode == 'IS',
      stringr::str_split(observation_id, '~', simplify = TRUE)[, 2],
      as.character(overall_observation_mode)
    )),
    observation_group_parts = stringr::str_split(observation_group, '-', simplify = TRUE),
    obspack_site_full = factor(ifelse(
      observation_type == 'obspack',
      observation_group_parts[, 1],
      NA
    )),
    obspack_site_full_parts = stringr::str_split(obspack_site_full, '_', simplify = TRUE),
    obspack_site = factor(ifelse(
      observation_type == 'obspack',
      obspack_site_full_parts[, 2],
      NA
    )),
    obspack_site_type = factor(ifelse(
      observation_type == 'obspack',
      obspack_site_full_parts[, 3],
      NA
    )),
    obspack_measurement_type = factor(ifelse(
      observation_type == 'obspack',
      stringr::str_split(observation_group_parts[, 2], '_', simplify = TRUE)[, 1],
      NA
    )),
    obspack_measurement_subtype = factor(ifelse(
      observation_type == 'obspack',
      observation_group_parts[, 3],
      NA
    ))
  ) %>%
  select(-obspack_site_full, -obspack_site_full_parts, -observation_group_parts) %>%
  filter(!is.na(co2_error))

log_info('Saving')
fst::write_fst(observations, args$output)

log_info('Done')
