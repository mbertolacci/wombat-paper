source(Sys.getenv('INVERSION_BASE_PARTIAL'))
source(Sys.getenv('UTILS_PARTIAL'))
library(ncdf4)
library(parallel)
library(tidyr)

n_unique <- function(x) length(unique(x))
all_same <- function(x) all(x == x[1])

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--control', '') %>%
  add_argument('--matched-runs-directory', '') %>%
  add_argument('--cores', '', default = get_cores()) %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading {args$control}')
control <- fst::read_fst(args$control)

run_paths <- get_run_paths(args$matched_runs_directory, include_base = FALSE)
log_info('Loading {length(run_paths)} flux files')
perturbations_parts <- mclapply(run_paths, function(run_path) {
  path <- file.path(args$matched_runs_directory, run_path, 'monthly-fluxes.nc')

  if (!file.exists(path)) return(NULL)
  log_debug('Loading {path}')
  with_nc_file(list(fn = path), {
    v <- function(...) ncvar_get(fn, ...)
    a <- function(name) as.integer(ncatt_get_quiet(fn, 0, name)$value)

    region <- a('region')
    year <- a('year')
    month <- a('month')

    locations <- expand.grid(
      longitude = v('longitude'),
      latitude = v('latitude'),
      month_start = as.Date(ncvar_get_time(fn, 'month_start'))
    )

    as_tibble(cbind(locations, data.frame(
      region = region,
      from_month_start = lubridate::ymd(sprintf('%04d-%02d-01', year, month)),
      ocean = as.vector(v('flux_density_ocean')),
      land = as.vector(
        v('flux_density_biomass')
        + v('flux_density_biofuel')
        + v('flux_density_bal_biosph')
      ),
      fossil_fuel = as.vector(
        v('flux_density_fossil_fuel') + v('flux_density_ship')
      )
    ))) %>%
      select(region, from_month_start, month_start, everything()) %>%
      filter(month_start == from_month_start) %>%
      pivot_longer(
        c(land, ocean, fossil_fuel),
        names_to = 'type',
        values_to = 'flux_density'
      ) %>%
      left_join(
        control %>%
          select(
            month_start,
            longitude,
            latitude,
            model_id,
            type,
            control_flux_density = flux_density
          ),
        by = c('month_start', 'longitude', 'latitude', 'type')
      ) %>%
      mutate(
        flux_density = flux_density - control_flux_density
      ) %>%
      select(-control_flux_density) %>%
      select(
        region,
        from_month_start,
        type,
        model_id,
        flux_density
      )
  })
}, mc.cores = args$cores)

# # HACK(mgnb): correction for stupidity
# perturbations_parts <- mclapply(perturbations_parts, function(part) {
#   if (is.null(part)) return(NULL)

#   region <- part$region[1]
#   from_month_start <- part$from_month_start[1]
#   log_trace('Correcting {region} {from_month_start}')

#   correction <- NULL
#   for (part_inner in perturbations_parts) {
#     if (is.null(part_inner)) next
#     region_inner <- part_inner$region[1]
#     from_month_start_inner <- part_inner$from_month_start[1]

#     if (
#       region_inner == region
#       && lubridate::month(from_month_start_inner) == lubridate::month(from_month_start)
#       && from_month_start_inner > from_month_start
#     ) {
#       log_trace('Correcting with {region_inner} {from_month_start_inner}')
#       correction <- part_inner
#       break
#     }
#   }

#   if (is.null(correction)) {
#     return(part)
#   }

#   part %>%
#     left_join(
#       correction %>% select(model_id, type, flux_density_correction = flux_density),
#       by = c('model_id', 'type')
#     ) %>%
#     mutate(
#       flux_density_correction = ifelse(
#         is.na(flux_density_correction),
#         0,
#         flux_density_correction
#       ),
#       flux_density = flux_density - flux_density_correction
#     ) %>%
#     select(-flux_density_correction)
# }, mc.cores = args$cores)

log_info('Computing perturbations')
perturbations <- bind_rows(perturbations_parts)

log_info('Checking internal consistency')
stopifnot(all(!is.na(perturbations$flux_density)))

perturbations %>%
  group_by(region) %>%
  summarise(
    n_from_month_starts = n_unique(from_month_start)
  ) %>%
  with(stopifnot(all_same(n_from_month_starts)))

perturbations %>%
  group_by(from_month_start) %>%
  summarise(
    n_regions = n_unique(region)
  ) %>%
  with(stopifnot(all_same(n_regions)))

perturbations %>%
  group_by(from_month_start, region) %>%
  summarise(
    n_observations = n()
  ) %>%
  ungroup() %>%
  group_by(from_month_start) %>%
  summarise(
    n_observations_equal = all_same(n_observations)
  ) %>%
  with(stopifnot(all(n_observations_equal)))

log_info('Saving to {args$output}')
fst::write_fst(perturbations, args$output)

log_info('Done')
