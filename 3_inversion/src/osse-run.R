source(Sys.getenv('INVERSION_BASE_PARTIAL'))
source(Sys.getenv('INVERSION_OSSE_PARTIAL'))
library(ncdf4)

GEOS_CHEM_GRID <- list(
  longitude = list(
    centres = seq(-180, 177.51, 2.5),
    widths = rep(2.5, 144)
  ),
  latitude = list(
    centres = c(-89.5, seq(-88, 88.1, 2), 89.5),
    widths = c(1, rep(2, 89), 1)
  )
)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--name', '') %>%
  add_argument('--base-run', '') %>%
  add_argument('--obspack-data', '') %>%
  add_argument('--ext-data', '') %>%
  add_argument('--ct2019-b4', '') %>%
  add_argument('--odiac2018', '') %>%
  add_argument('--template', '') %>%
  add_argument('--restart-file', '') %>%
  add_argument('--code-directory', '') %>%
  add_argument('--threads', '') %>%
  add_argument('--output', '') %>%
  parse_args()

epoch <- lubridate::ymd_hms('2000-01-01 00:00:00')
times <- as.POSIXct(seq(
  as.Date('2014-09-01'),
  as.Date('2017-03-01'),
  by = 'months'
), tz = 'UTC')

write_scale_factors <- function(alpha, filename) {
  time <- ncdim_def(
    'time',
    vals = as.numeric(times - epoch, units = 'days'),
    units = sprintf('days since %s', format(epoch, '%Y-%m-%d %H:%M:%S')),
    longname = 'time'
  )
  # HACK(mgnb): this is a dummy grid, needed because HEMCO scale factors must
  # exist on a spatial grid
  latitude <- ncdim_def(
    'latitude',
    vals = GEOS_CHEM_GRID$latitude$centres,
    units = 'degrees_north',
    longname = 'latitude'
  )
  longitude <- ncdim_def(
    'longitude',
    vals = GEOS_CHEM_GRID$longitude$centres,
    units = 'degrees_east',
    longname = 'longitude'
  )
  alpha_scale_factors_vars <- lapply(seq_len(ncol(alpha)), function(region) {
    ncvar_def(
      sprintf('Region%02d', region),
      units = '1',
      longname = sprintf('alpha scale factors for region %02d', region),
      dim = list(longitude, latitude, time)
    )
  })

  n_times <- length(times)
  n_latitude <- length(GEOS_CHEM_GRID$latitude$centres)
  n_longitude <- length(GEOS_CHEM_GRID$longitude$centres)

  fn <- nc_create(filename, alpha_scale_factors_vars)
  on.exit(nc_close(fn))
  for (region in seq_len(ncol(alpha))) {
    ncvar_put(
      fn,
      alpha_scale_factors_vars[[region]],
      aperm(
        array(1 + alpha[, region], dim = c(n_times, n_latitude, n_longitude)),
        c(3, 2, 1)
      )
    )
  }
  invisible(NULL)
}

read_template_to <- function(from, to, template_variables) {
  output <- readChar(from, file.info(from)$size)
  # HACK(mgnb): should really call jinja2 via reticulate
  names(template_variables) <- sprintf('\\{\\{ %s \\}\\}', names(template_variables))
  output <- stringr::str_replace_all(
    output,
    template_variables
  )
  writeChar(output, to)
}

# NOTE(mgnb): modified from https://stackoverflow.com/questions/36726186/function-for-constructing-relative-paths-in-r
relative_path <- function(base, target) {
  base <- normalizePath(base)
  target <- normalizePath(target)
  common <- sub(
    '^([^|]*)[^|]*(?:\\|\\1[^|]*)$',
    '^\\1/?',
    paste0(base, '|', target)
  )
  paste0(
    gsub('[^/]+/?', '../', sub(common, '', base)),
    sub(common, '', target)
  )
}

relative_symlink <- function(from, to) {
  invisible(file.symlink(
    relative_path(dirname(to), from),
    to
  ))
}

run <- Find(function(run_i) {
  run_i$name == args$name
}, OSSE_CASES)

output_path <- file.path(args$output, sprintf('run.v12.3.2.osse.%s', run$name))
if (dir.exists(output_path)) {
  log_info('Exiting because {output_path} already exists')
  quit(status = 1)
}
invisible(dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE))
invisible(file.copy(args$template, dirname(output_path), recursive = TRUE))
invisible(file.rename(
  file.path(dirname(output_path), basename(args$template)),
  output_path
))

write_scale_factors(
  run$alpha,
  file.path(output_path, 'alpha_scale_factors.nc')
)

template_variables <- c(
  'ext_data' = relative_path(output_path, args$ext_data),
  'ct2019_b4_file' = relative_path(output_path, args$ct2019_b4),
  'odiac2018_file' = relative_path(output_path, args$odiac2018),
  'start_date' = '20140901',
  'end_date' = '20170401',
  'obspack_data' = relative_path(output_path, args$obspack_data),
  'threads' = args$threads,
  'slurm_job_name' = sprintf('run.v12.3.2.osse.%s', run$name)
)

for (template_name in list.files(args$template, pattern = '\\.template$')) {
  read_template_to(
    file.path(args$template, template_name),
    file.path(output_path, substring(template_name, 1, nchar(template_name) - 9)),
    template_variables
  )
  file.remove(file.path(output_path, template_name))
}

relative_symlink(
  args$code_directory,
  file.path(output_path, 'CodeDir')
)
relative_symlink(
  args$restart_file,
  file.path(output_path, basename(args$restart_file))
)
relative_symlink(
  file.path(args$base_run, 'geos.mp'),
  file.path(output_path, 'geos.mp')
)
