library(lubridate, warn.conflicts = FALSE)

month_starts <- as.Date(c('2016-01-01', '2016-02-01', '2016-03-01'))
regions <- 1 : 2
model_ids <- 1 : 5

control_emissions <- expand.grid(
  month_start = month_starts,
  region = regions
) %>%
  mutate(
    model_id = 1 : n(),
    area = 1,
    flux_density = 0
  )

perturbations <- expand.grid(
  from_month_start = month_starts,
  month_start = month_starts,
  region = regions
) %>%
  left_join(
    control_emissions %>%
      select(month_start, region, model_id),
    by = c('month_start', 'region')
  ) %>%
  select(-month_start) %>%
  mutate(flux_density = 0)

control_mole_fraction <- data.frame(
  model_id = model_ids,
  time = lubridate::ymd_hm(c(
    '2016-01-03 09:00',
    '2016-01-03 10:00',
    '2016-02-03 10:00',
    '2016-03-03 10:00',
    '2016-03-03 11:00'
  ))
) %>%
  mutate(
    co2 = 0,
    latitude = 0
  )

sensitivities <- expand.grid(
  from_month_start = month_starts,
  model_id = model_ids,
  region = regions
) %>%
  mutate(co2_sensitivity = 1)

observations <- data.frame(
  observation_id = 1 : 3,
  instrument_mode = c('LG', 'LG', 'LN'),
  time = lubridate::ymd_hm(c(
    '2016-01-03 09:00',
    '2016-01-03 10:00',
    '2016-02-03 10:00'
  )),
  co2_error = c(1, 2, 1)
)
