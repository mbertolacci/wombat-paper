#' @export
ncvar_get_time <- function(x, variable_name) {
  units <- x$var[[variable_name]]$units
  if (is.null(units)) {
    units <- x$dim[[variable_name]]$units
  }

  match <- stringr::str_match(
    units,
    '^(days|hours|minutes|seconds|Days|Hours|Seconds) since (.+)$'
  )[1, 2 : 3]
  lubridate::ymd_hms(match[2]) + list(
    'days' = lubridate::days,
    'hours' = lubridate::hours,
    'minutes' = lubridate::minutes,
    'seconds' = lubridate::seconds
  )[[tolower(match[1])]](ncdf4::ncvar_get(x, variable_name))
}

#' @export
with_nc_file <- function(files, code, envir = parent.frame()) {
  for (nme in names(files)) {
    assign(
      nme,
      ncdf4::nc_open(files[[nme]]),
      envir = envir
    )
  }
  on.exit({
    for (nme in names(files)) {
      ncdf4::nc_close(get(nme, envir = envir))
      rm(list = nme, envir = envir)
    }
  })
  eval(substitute(code), envir = envir)
}
