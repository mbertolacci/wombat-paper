suppressPackageStartupMessages(library(raster, warn.conflicts = FALSE))
library(argparser)
library(dplyr, warn.conflicts = FALSE)
library(wombat)
library(logger)
library(lubridate, warn.conflicts = FALSE)
library(withr)

saveRDS_gz1 <- function(x, filename) {
  saveRDS(x, gzfile(filename, compression = 1))
}

if (Sys.getenv('WOMBAT_LOG_LEVEL') != '') {
  log_threshold(list(
    'trace' = TRACE,
    'debug' = DEBUG,
    'info' = INFO,
    'warn' = WARN,
    'error' = ERROR
  )[[Sys.getenv('WOMBAT_LOG_LEVEL')]])
}

load_obspack_observations <- function(obspack_directory, start_date, end_date, cores) {
  obspack_paths <- do.call(c, lapply(obspack_directory, list.files, full.names = TRUE))
  obspack_observations <- bind_rows(parallel::mclapply(obspack_paths, function(path) {
    file_date <- ymd(stringr::str_match(
      basename(path),
      '^flask_input\\.(\\d{8})_\\d{8}\\.nc$'
    )[2])

    if (file_date < start_date || file_date >= end_date) return(NULL)

    with_nc_file(list(fn = path), {
      v <- function(...) ncdf4::ncvar_get(fn, ...)
      tibble(
        obspack_id = v('obspack_id'),
        time = ncvar_get_time(fn, 'time'),
        longitude = v('longitude'),
        latitude = v('latitude'),
        altitude = v('altitude'),
        co2 = 1e6 * v('value'),
        co2_error = v('mdm')
      ) %>%
        filter(!is.na(co2))
    })
  }, mc.cores = cores)) %>%
    distinct(obspack_id, .keep_all = TRUE)
}
