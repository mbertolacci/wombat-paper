library(argparser)
library(dplyr, warn.conflicts = FALSE)
library(ncdf4)
library(parallel)
library(wombat)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('MATCHING_UTILS_PARTIAL'))

get_tccon_pressure_weights <- function(p) {
  # NOTE(mgnb): these are trapezoidal rule weights
  0.5 * abs(cbind(
    p[, 1] - p[, 2],
    matrixStats::rowDiffs(p, lag = 2),
    p[, ncol(p) - 1] - p[, ncol(p)]
  )) / p[, 1]
}

compute_xco2 <- function(df) {
  with(df, {
    vco2_at_obs <- interpolate_vco2_pce(
      vco2,
      pressure_edge,
      obs_pressure_levels
    )
    obs_xco2_apriori + rowSums(
      obs_pressure_weights * obs_averaging_kernel * (
        vco2_at_obs - obs_vco2_apriori
      ),
      na.rm = TRUE
    )
  })
}

SURFACE_PRESSURE_THRESHOLD <- 100

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--matched-runs-directory', '') %>%
  add_argument('--oco2-observations', '') %>%
  add_argument('--tccon-observation-directory', '') %>%
  add_argument('--cores', '', default = get_cores()) %>%
  parse_args()

log_info('Loading OCO-2 observations')
with_nc_file(list(fn = args$oco2_observations), {
  v <- function(...) ncvar_get(fn, ...)
  sigma_levels <- v('sigma_levels')

  oco2_soundings <- tibble(
    sounding_id = v('sounding_id'),
    pressure_surface = v('psurf'),
    pressure_weights = t(v('pressure_weight')),
    averaging_kernel = t(v('xco2_averaging_kernel')),
    xco2_apriori = v('xco2_apriori'),
    vco2_apriori = t(v('co2_profile_apriori')),
  )
})
oco2_soundings$pressure_levels <- as.matrix(oco2_soundings$pressure_surface) %*% sigma_levels

log_info('Loading TCCON observations')
tccon_paths <- list.files(args$tccon_observation_directory, full.names = TRUE)
tccon_soundings <- rbind_alt(lapply(tccon_paths, function(filename) {
  with_nc_file(list(fn = filename), {
    v <- function(name, ...) ncvar_get(fn, sprintf('CO2/%s', name), collapse_degen = FALSE, ...)
    tibble(
      sounding_id = as.vector(v('obs_num')),
      pressure_levels = 1e-2 * t(v('p_levels_prior')),
      averaging_kernel = t(v('avg_kernel')),
      vh2o_wet_apriori = t(v('prior_h2o')),
      vco2_wet_apriori = t(v('prior_mixing'))
    )
  })
})) %>%
  mutate(pressure_surface = pressure_levels[, 1])

tccon_soundings$vco2_apriori <- with(tccon_soundings, {
  vco2_wet_apriori / (1 - vh2o_wet_apriori)
})

tccon_soundings$pressure_weights <- get_tccon_pressure_weights(tccon_soundings$pressure_levels)
tccon_soundings$xco2_apriori <- with(tccon_soundings, rowSums(
  pressure_weights * vco2_apriori
))

run_paths <- get_run_paths(args$matched_runs_directory)

log_info('Processing {length(run_paths)} runs')
invisible(results <- mclapply(run_paths, function(run_path) {
  log_info('Processing {run_path$short_path}')

  process_soundings <- function(name, soundings, id_unit, id_long_name) {
    subsetted_path <- file.path(run_path$full_path, sprintf('subsetted-%s.nc', name))

    log_debug('Loading {name} subsetted values')
    with_nc_file(list(fn = subsetted_path), {
      v <- function(...) ncvar_get(fn, ...)
      subsetted_attributes <- ncatt_get_quiet(fn, 0)
      subsetted <- tibble(
        sounding_id = ncvar_get(fn, 'sounding_id'),
        observation_latitude = ncvar_get(fn, 'observation_latitude'),
        observation_longitude = ncvar_get(fn, 'observation_longitude'),
        model_latitude = ncvar_get(fn, 'model_latitude'),
        model_longitude = ncvar_get(fn, 'model_longitude'),
        time = ncvar_get_time(fn, 'time'),
        # NOTE(mgnb): convert from mol/mol to ppm
        vco2 = 1e6 * t(v('vco2')),
        pressure_edge = t(v('pressure_edge'))
      )
    })

    log_debug('Joining observed and subsetted')
    output <- subsetted %>%
      left_join(
        soundings %>%
          select(
            sounding_id,
            obs_pressure_surface = pressure_surface,
            obs_pressure_levels = pressure_levels,
            obs_pressure_weights = pressure_weights,
            obs_averaging_kernel = averaging_kernel,
            obs_xco2_apriori = xco2_apriori,
            obs_vco2_apriori = vco2_apriori
          ),
        by = 'sounding_id'
      ) %>%
      mutate(
        surface_pressure_obs_minus_gc = obs_pressure_surface - pressure_edge[, 1]
      ) %>%
      filter(abs(surface_pressure_obs_minus_gc) <= SURFACE_PRESSURE_THRESHOLD)

    log_debug('Computing XCO2')
    output$xco2 <- compute_xco2(output)

    output_attributes <- with_traceability_attributes(subsetted_attributes)

    log_debug('Saving')
    output %>%
      select(
        sounding_id,
        time,
        observation_latitude,
        observation_longitude,
        model_latitude,
        model_longitude,
        xco2
      ) %>%
      mutate(
        time = as.integer(round(as.double(
          time - lubridate::ymd_hms('2000-01-01 00:00:00'),
          units = 'hours'
        )))
      ) %>%
      write_nc(
        file.path(run_path$full_path, sprintf('xco2-%s.nc', name)),
        units = c(
          'sounding_id' = id_unit,
          'time' = 'hours since 2000-01-01 00:00:00',
          'observation_latitude' = 'degrees_north',
          'observation_longitude' = 'degrees_east',
          'model_latitude' = 'degrees_north',
          'model_longitude' = 'degrees_east',
          'xco2' = 'ppm'
        ),
        long_name = c(
          'sounding_id' = id_long_name,
          'time' = 'Time',
          'observation_latitude' = 'Observation Latitude',
          'observation_longitude' = 'Observation Longitude',
          'model_latitude' = 'Model Latitude',
          'model_longitude' = 'Model Longitude',
          'xco2' = 'Column average CO2 (dry air)'
        ),
        attributes = output_attributes
      )
  }

  process_soundings('oco2-hourly', oco2_soundings, 'YYYYMMDDhhmmso', 'OCO-2 10-sec sounding ID')
  process_soundings('oco2-daily', oco2_soundings, 'YYYYMMDDhhmmso', 'OCO-2 10-sec sounding ID')
  process_soundings('tccon-hourly', tccon_soundings, 'YYYYmmddHHMMSSII', 'TCCON MIP sounding ID')
  process_soundings('tccon-daily', tccon_soundings, 'YYYYmmddHHMMSSII', 'TCCON MIP sounding ID')

  NULL
}, mc.cores = args$cores))

any_failed <- FALSE
for (result in results) {
  if (is(result, 'try-error')) {
    print(result)
    any_failed <- TRUE
  }
}
if (any_failed) {
  stop('Failed')
}

log_info('Done')
