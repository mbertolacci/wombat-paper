library(argparser)
library(dplyr, warn.conflicts = FALSE)
library(ncdf4)
library(parallel)

library(wombat)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('MATCHING_UTILS_PARTIAL'))

get_input_attributes <- function(fn) {
  output <- ncatt_get_quiet(fn, 0)
  output <- output[base::intersect(
    names(output),
    c(
      'run_type',
      'geoschem_version',
      'git_commit',
      'year',
      'region',
      'month'
    )
  )]
  for (name in c('year', 'region', 'month')) {
    if (name %in% names(output)) {
      output[[name]] <- as.integer(output[[name]])
    }
  }
  output
}

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--matched-runs-directory', '') %>%
  add_argument('--cores', '', default = get_cores()) %>%
  parse_args()

process_run <- function(run_path) {
  log_info('Processing {run_path$short_path}')

  obspack_path <- file.path(run_path$full_path, 'subsetted-obspack.nc')

  with_nc_file(list(fn = obspack_path), {
    v <- function(...) as.vector(ncvar_get(fn, ...))
    log_trace('Opening {obspack_path}')
    obspack <- tibble(
      obspack_id = v('obspack_id'),
      resolution = 'obspack',
      time = ncvar_get_time(fn, 'time'),
      observation_latitude = v('observation_latitude'),
      observation_longitude = v('observation_longitude'),
      model_latitude = v('model_latitude'),
      model_longitude = v('model_longitude'),
      CO2 = v('CO2'),
    )
    attr(obspack, 'nc_attributes') <- get_input_attributes(fn)
  })

  read_oco2_tccon <- function(path, resolution) {
    log_trace('Opening {path}')
    with_nc_file(list(fn = path), {
      v <- function(...) as.vector(ncvar_get(fn, ...))
      output <- tibble(
        sounding_id = v('sounding_id'),
        resolution = resolution,
        time = ncvar_get_time(fn, 'time'),
        observation_latitude = v('observation_latitude'),
        observation_longitude = v('observation_longitude'),
        model_latitude = v('model_latitude'),
        model_longitude = v('model_longitude'),
        xco2 = v('xco2')
      )
      attr(output, 'nc_attributes') <- get_input_attributes(fn)
      output
    })
  }

  oco2_hourly <- read_oco2_tccon(file.path(run_path$full_path, 'xco2-oco2-hourly.nc'), 'hourly')
  oco2_daily <- read_oco2_tccon(file.path(run_path$full_path, 'xco2-oco2-daily.nc'), 'daily')
  tccon_hourly <- read_oco2_tccon(file.path(run_path$full_path, 'xco2-tccon-hourly.nc'), 'hourly')
  tccon_daily <- read_oco2_tccon(file.path(run_path$full_path, 'xco2-tccon-daily.nc'), 'daily')

  if (
    !identical(
      attr(obspack, 'nc_attributes'),
      attr(oco2_daily, 'nc_attributes')
    ) || !identical(
      attr(obspack, 'nc_attributes'),
      attr(tccon_daily, 'nc_attributes')
    )
  ) {
    warning('Traceability attributes for input files differ')
  }

  transform_oco2_tccon <- function(df, observation_type) {
    df %>%
      mutate(
        sounding_id_str = sprintf('%f', sounding_id),
        observation_type = observation_type
      ) %>%
      select(
        observation_id = sounding_id_str,
        observation_type,
        resolution,
        time,
        observation_latitude,
        observation_longitude,
        model_latitude,
        model_longitude,
        co2 = xco2
      )
  }

  output_attributes <- with_traceability_attributes(attr(
    obspack,
    'nc_attributes'
  ))

  output <- bind_rows(
    obspack %>%
      mutate(
        observation_type = 'obspack',
        co2 = 1e6 * CO2
      ) %>%
      select(
        observation_id = obspack_id,
        observation_type,
        resolution,
        time,
        observation_latitude,
        observation_longitude,
        model_latitude,
        model_longitude,
        co2
      ),
    transform_oco2_tccon(oco2_hourly, 'oco2'),
    transform_oco2_tccon(oco2_daily, 'oco2'),
    transform_oco2_tccon(tccon_hourly, 'tccon'),
    transform_oco2_tccon(tccon_daily, 'tccon')
  ) %>%
    mutate(
      time = as.integer(round(as.double(
        time - lubridate::ymd_hms('2000-01-01 00:00:00'),
        units = 'hours'
      )))
    )

  output_path <- file.path(run_path$full_path, 'combined.nc')
  log_trace('Writing {output_path}')
  output %>%
    write_nc(
      output_path,
      dimension_name = 'obs',
      units = c(
        'observation_id' = '',
        'observation_type' = '',
        'resolution' = '',
        'time' = 'hours since 2000-01-01 00:00:00',
        'observation_latitude' = 'degrees_north',
        'observation_longitude' = 'degrees_east',
        'model_latitude' = 'degrees_north',
        'model_longitude' = 'degrees_east',
        'co2' = 'ppm'
      ),
      long_name = c(
        'observation_id' = 'Observation ID',
        'observation_type' = 'Observation Type',
        'resolution' = 'Resolution',
        'time' = 'Time (model)',
        'observation_latitude' = 'Observation Latitude',
        'observation_longitude' = 'Observation Longitude',
        'model_latitude' = 'Model Latitude',
        'model_longitude' = 'Model Longitude',
        'co2' = 'CO2 mole fraction (dry air)'
      ),
      attributes = output_attributes
    )

  NULL
}

run_paths <- get_run_paths(args$matched_runs_directory)

log_info('Processing {length(run_paths)} runs')
results <- mclapply(run_paths, process_run, mc.cores = args$cores)

any_failed <- FALSE
for (result in results) {
  if (is(result, 'try-error')) {
    print(result)
    any_failed <- TRUE
  }
}
if (any_failed) {
  stop('Combining failed')
}

log_info('Done')
