library(abind)
library(argparser)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(ncdf4)
library(parallel)
library(wombat)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('MATCHING_UTILS_PARTIAL'))

process_run <- function(run_path, output_path, meteorology_run_path) {
  species_conc_paths <- list.files(file.path(run_path, 'output'), pattern = 'GEOSChem\\.SpeciesConc\\..*\\.nc4')
  date_strings <- substr(species_conc_paths, 22, 29)

  xco2_3hr_parts <- lapply(date_strings, function(date_string) {
    with_nc_file(list(
      species_conc = file.path(run_path, 'output', sprintf('GEOSChem.SpeciesConc.%s_0000z.nc4', date_string)),
      level_edge = file.path(meteorology_run_path, 'output', sprintf('GEOSChem.LevelEdgeDiags.%s_0000z.nc4', date_string))
    ), {
      log_debug('[{run_path}] Processing {date_string}')
      vco2 <- 1e6 * aperm(ncvar_get(species_conc, 'SpeciesConc_CO2'), c(1, 2, 4, 3))
      pressure_edge <- aperm(ncvar_get(level_edge, 'Met_PEDGE'), c(1, 2, 4, 3))

      n_levels <- dim(vco2)[4]

      w_vco2 <- vco2 * (
        pressure_edge[, , , seq_len(n_levels)]
        - pressure_edge[, , , 1 + seq_len(n_levels)]
      )

      xco2 <- rowSums(
        w_vco2,
        dims = 3
      ) / pressure_edge[, , , 1]

      rowMeans(aperm(array(
        xco2,
        dim = c(dim(xco2)[1 : 2], 3, 8)
      ), c(1, 2, 4, 3)), dims = 3)
    })
  })

  with_nc_file(list(fn = file.path(run_path, 'output', species_conc_paths[1])), {
    longitudes <- ncvar_get(fn, 'lon')
    latitudes <- ncvar_get(fn, 'lat')
  })

  log_debug('[{run_path}] Constructing times')
  times <- parse_date_time(sprintf(
    '%s %s',
    rep(date_strings, each = 8),
    rep(c('01:30', '04:30', '07:30', '10:30', '13:30', '16:30', '19:30', '22:30'), length(date_strings))
  ), '%Y%m%d %H:%M')

  log_debug('[{run_path}] Joining')
  xco2_3hr <- abind(xco2_3hr_parts, along = 3)

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  log_debug('[{run_path}] Writing output')
  time_dim <- ncdim_def(
    'time',
    vals = as.integer(round(as.double(
      times - lubridate::ymd_hms('2000-01-01 00:00:00'),
      units = 'mins'
    ))),
    units = 'minutes since 2000-01-01 00:00:00',
    longname = 'time',
    unlim = TRUE
  )
  longitude_dim <- ncdim_def(
    'longitude',
    vals = longitudes,
    units = 'degrees_east',
    longname = 'Longitude'
  )
  latitude_dim <- ncdim_def(
    'latitude',
    vals = latitudes,
    units = 'degrees_north',
    longname = 'Latitude'
  )

  xco2_var <- ncvar_def(
    'xco2',
    units = 'ppm',
    longname = 'Column average CO2 (dry air)',
    dim = list(longitude_dim, latitude_dim, time_dim),
    prec = 'double',
    compression = 9
  )

  nc_fn <- nc_create(file.path(output_path, 'xco2-3hr.nc'), list(xco2_var))
  on.exit(nc_close(nc_fn))
  ncvar_put(
    nc_fn,
    xco2_var,
    xco2_3hr
  )
}

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--meteorology-run', '') %>%
  add_argument('--input-runs', '') %>%
  add_argument('--output-runs', '') %>%
  add_argument('--input-run', '') %>%
  add_argument('--output-run', '') %>%
  add_argument('--cores', '', default = get_cores()) %>%
  parse_args()

if (!is.na(args$input_runs)) {
  run_paths <- get_run_paths(args$input_runs)

  log_info('Processing {length(run_paths)} runs')
  invisible(mclapply(run_paths, function(run_path) {
    log_info('[{run_path}] Processing')

    process_run(
      file.path(args$input_runs, run_path),
      file.path(args$output_runs, run_path),
      args$meteorology_run
    )
  }, mc.cores = args$cores))
} else {
  log_info('[{args$input_run}] Processing')

  process_run(
    args$input_run,
    args$output_run,
    args$meteorology_run
  )
}
