context('inversion-mcmc-a')

test_that('quantiles are estimated correctly', {
  model <- flux_process_model(
    control_emissions,
    control_mole_fraction,
    perturbations,
    sensitivities,
    a_factor = factor(rep(1, 2)),
    w_factor = factor(rep(1, 2))
  )
  a_sampler <- .make_a_sampler(
    model,
    tuning = list(w = 0.25, min_w = 0.1, max_evaluations = 100)
  )

  current <- list(
    a = 0.5,
    alpha = rnorm(6),
    w = rep(1, 2),
    kappa = c(0, 0)
  )

  log_likelihood <- .make_alpha_log_likelihood(model)

  log_a_conditional <- function(a) {
    current$a <- a
    output <- log_likelihood(current) + log_prior(model, current)
    if (is.nan(output)) -Inf
    else output
  }
  theoretical_quantiles <- quantiles_log_density(
    Vectorize(log_a_conditional),
    0,
    1,
    c(0.25, 0.5, 0.75)
  )

  n_samples <- 1100
  warm_up <- 100
  a_samples_full <- coda::mcmc(rep(0, n_samples))
  for (i in 1 : n_samples) {
    current <- a_sampler(current, n_samples <= warm_up)
    a_samples_full[i] <- current$a
  }
  a_samples <- window(a_samples_full, start = warm_up + 1)
  expect_true(all(abs(
    quantile(a_samples, probs = c(0.25, 0.5, 0.75))
    - theoretical_quantiles
  ) < 0.1))
})
