context('inversion-mcmc-gamma')

test_that('quantiles are estimated correctly', {
  process_model <- flux_process_model(
    control_emissions,
    control_mole_fraction,
    perturbations,
    sensitivities
  )
  observations$co2 <- rnorm(nrow(observations))
  measurement_model <- flux_measurement_model(
    observations,
    ~ instrument_mode,
    observations$observation_id,
    process_model,
    attenuation_variables = 'instrument_mode',
    rho = rep(0, 2),
    ell = rep(1, 2)
  )

  gamma_sampler <- .make_gamma_sampler(
    measurement_model,
    process_model
  )

  current <- list(
    gamma = rep(1, 2),
    alpha = rep(0, 6),
    eta = rep(0, 16),
    beta = rep(0, 1),
    rho = measurement_model$rho,
    ell = measurement_model$ell
  )

  log_gamma_conditional <- function(gamma) {
    gamma <- c(gamma, 1)
    current$gamma <- gamma

    output <- sum(dnorm(
      observations$co2,
      sd = (
        observations$co2_error
        / sqrt(gamma[measurement_model$attenuation_factor])
      ),
      log = TRUE
    )) + log_prior(measurement_model, current)

    if (is.nan(output)) -Inf
    else output
  }
  theoretical_quantiles <- quantiles_log_density(
    Vectorize(log_gamma_conditional),
    0,
    20,
    c(0.25, 0.5, 0.75)
  )

  n_samples <- 2000
  warm_up <- 1000
  gamma_samples_full <- coda::mcmc(matrix(0, nrow = n_samples, ncol = 2))
  for (i in 1 : n_samples) {
    current <- gamma_sampler(current, n_samples <= warm_up)
    gamma_samples_full[i, ] <- current$gamma
  }
  gamma_samples <- window(gamma_samples_full[, 1], start = warm_up + 1)

  expect_true(all(abs(
    quantile(gamma_samples, probs = c(0.25, 0.5, 0.75))
    - theoretical_quantiles
  ) < 0.1))
})
