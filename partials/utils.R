library(logger)

if (Sys.getenv('WOMBAT_LOG_LEVEL') != '') {
  log_threshold(list(
    'trace' = TRACE,
    'debug' = DEBUG,
    'info' = INFO,
    'warn' = WARN,
    'error' = ERROR
  )[[Sys.getenv('WOMBAT_LOG_LEVEL')]])
}

get_cores <- function() {
  max_workers <- as.integer(Sys.getenv('WOMBAT_MAX_WORKERS'))
  if (is.na(max_workers)) {
    parallel::detectCores()
  } else {
    max_workers
  }
}

Filter2 <- function(x, f) Filter(f, x)

ncatt_get_quiet <- function(...) {
  # HACK(mgnb): this function uses `print` to give a warning; hide it
  capture.output({
    output <- ncdf4::ncatt_get(...)
  })
  output
}

list_dirs_depth <- function(path, depth = 2) {
  if (depth == 1) {
    output <- list.dirs(path, recursive = FALSE)
    print(output)
    output
  } else {
    output <- list.dirs(path, recursive = FALSE)
    print(output)
    c(output, do.call(c, sapply(output, list_dirs_depth, depth = depth - 1)))
  }
}

get_run_paths <- function(runs_directory, include_base = TRUE) {
  if (length(runs_directory) == 1 && startsWith(basename(runs_directory), 'run')) {
    return(list(list(
      short_path = basename(runs_directory),
      full_path = runs_directory
    )))
  }
  output <- list.dirs(runs_directory, full.names = FALSE) %>%
    Filter2(function(path) {
      if (!include_base && endsWith(path, 'base')) return(FALSE)
      startsWith(basename(path), 'run')
    }) %>%
    lapply(function(path) {
      list(
        short_path = path,
        full_path = file.path(runs_directory, path)
      )
    })
  names(output) <- NULL

  output
}
