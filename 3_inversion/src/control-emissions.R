source(Sys.getenv('INVERSION_BASE_PARTIAL'))
library(ncdf4)
library(tidyr)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--matched-runs-directory', '') %>%
  add_argument('--start-date', '') %>%
  add_argument('--end-date', '') %>%
  add_argument('--output', '') %>%
  parse_args()

path <- file.path(
  args$matched_runs_directory,
  'run.v12.3.2.base/monthly-fluxes.nc'
)
log_info('Loading {path}')
with_nc_file(list(fn = path), {
  v <- function(...) ncvar_get(fn, ...)

  n_longitudes <- length(v('longitude'))
  n_latitudes <- length(v('latitude'))
  month_starts <- as.Date(ncvar_get_time(fn, 'month_start'))

  locations <- expand.grid(
    longitude_index = seq_len(n_longitudes),
    latitude_index = seq_len(n_latitudes),
    month_start = month_starts
  ) %>%
    mutate(
      model_id = 1 : n(),
      area = rep(as.vector(v('area')), length(month_starts))
    ) %>%
    select(month_start, model_id, everything())

  emissions <- cbind(locations, data.frame(
    # Comes in kg/m^2/s
    ocean = as.vector(v('flux_density_ocean')),
    land = as.vector(
      v('flux_density_biomass')
      + v('flux_density_biofuel')
      + v('flux_density_bal_biosph')
    ),
    fossil_fuel = as.vector(
      v('flux_density_fossil_fuel') + v('flux_density_ship')
    )
  )) %>%
    pivot_longer(
      c(land, ocean, fossil_fuel),
      names_to = 'type',
      values_to = 'flux_density'
    ) %>%
    left_join(
      data.frame(
        longitude_index = seq_len(n_longitudes),
        longitude = v('longitude'),
        cell_width = v('longitude_width')
      ),
      by = 'longitude_index'
    ) %>%
    left_join(
      data.frame(
        latitude_index = seq_len(n_latitudes),
        latitude = v('latitude'),
        cell_height = v('latitude_height')
      ),
      by = 'latitude_index'
    ) %>%
    select(-longitude_index, -latitude_index) %>%
    filter(
      month_start >= ymd(args$start_date),
      month_start < ymd(args$end_date)
    )
})

log_info('Saving to {args$output}')
fst::write_fst(emissions, args$output)

log_info('Done')
