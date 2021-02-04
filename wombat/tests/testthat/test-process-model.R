context('process-model')

test_that('components have correct dimensions', {
  model <- flux_process_model(
    control_emissions,
    control_mole_fraction,
    perturbations,
    sensitivities
  )
  for (name in c(
    'control_emissions',
    'perturbations',
    'control_mole_fraction',
    'a_prior',
    'w_prior'
  )) {
    expect_false(is.null(model[[name]]))
  }
  for (name in c('a', 'w', 'eta')) {
    expect_true(is.null(model[[name]]))
  }
  n_alpha <- length(regions) * length(month_starts)
  expect_equal(dim(model$H), c(nrow(control_mole_fraction), n_alpha))
  expect_equal(nrow(model$Psi), nrow(control_mole_fraction))
  expect_equal(dim(model$eta_prior_precision), rep(ncol(model$Psi), 2))
})

test_that('samples have correct dimensions', {
  model <- flux_process_model(
    control_emissions,
    control_mole_fraction,
    perturbations,
    sensitivities
  )
  process_sample <- generate(model)
  expect_length(process_sample$a, 2)
  expect_length(process_sample$alpha, ncol(model$H))
  expect_length(process_sample$eta, ncol(model$Psi))
})

test_that('update works', {
  model <- flux_process_model(
    control_emissions,
    control_mole_fraction,
    perturbations,
    sensitivities
  )
  updated_model <- update(
    model,
    lag = months(1),
    eta = 1,
    alpha = 1,
    sensitivities = sensitivities
  )

  expect_false(max(abs(updated_model$H - model$H)) == 0)
  expect_equal(updated_model$alpha, rep(1, ncol(model$H)))
  expect_equal(updated_model$eta, rep(1, ncol(model$Psi)))
})
