context('measurement-model')

test_that('outputs have correct dimensions', {
  process_model <- flux_process_model(
    control_emissions,
    control_mole_fraction,
    perturbations,
    sensitivities
  )
  model <- flux_measurement_model(
    observations,
    ~ instrument_mode,
    observations$observation_id,
    process_model,
    attenuation_variables = 'instrument_mode'
  )
  for (name in c('observations', 'gamma_prior')) {
    expect_false(is.null(model[[name]]))
  }
  for (name in c('beta')) {
    expect_true(is.null(model[[name]]))
  }
  n_beta <- 1
  expect_equal(dim(model$C), c(nrow(observations), nrow(control_mole_fraction)))
  expect_equal(length(model$measurement_variance), nrow(observations))
  expect_equal(dim(model$A), c(nrow(observations), n_beta))
  expect_length(model$beta_prior_mean, n_beta)
  expect_equal(dim(model$beta_prior_precision), rep(n_beta, 2))
  expect_length(model$attenuation_factor, nrow(observations))
})

test_that('samples have correct dimensions', {
  process_model <- flux_process_model(
    control_emissions,
    control_mole_fraction,
    perturbations,
    sensitivities
  )
  model <- flux_measurement_model(
    observations,
    ~ instrument_mode,
    observations$observation_id,
    process_model,
    attenuation_variables = 'instrument_mode'
  )

  measurement_sample <- generate(model, generate(process_model))

  expect_length(measurement_sample$beta, 1)
  expect_length(measurement_sample$gamma, 2)
})

test_that('update works', {
  process_model <- flux_process_model(
    control_emissions,
    control_mole_fraction,
    perturbations,
    sensitivities
  )
  model <- flux_measurement_model(
    observations,
    ~ instrument_mode,
    observations$observation_id,
    process_model,
    attenuation_variables = 'instrument_mode'
  )

  updated_model <- update(model, attenuation_variables = NULL, gamma = 1)
  expect_equal(
    updated_model$attenuation_factor,
    factor(rep(1, nrow(observations)))
  )
  expect_equal(updated_model$gamma, 1)
})
