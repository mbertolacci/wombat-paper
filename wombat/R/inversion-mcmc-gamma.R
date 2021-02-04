.make_gamma_sampler <- function(
  measurement_model,
  process_model,
  tuning = list(w = 1, max_evaluations = 100),
  omega = .make_omega(process_model, measurement_model),
  X = .make_X_omega(process_model, measurement_model),
  Sigma_epsilon = .make_Sigma_epsilon(measurement_model),
  Z2_tilde = calculate(measurement_model, 'Z2_tilde', process_model)
) {
  if (!is.null(measurement_model[['gamma']])) {
    return(function(current, ...) {
      current$gamma <- measurement_model[['gamma']]
      current
    })
  }

  attenuation_index <- as.integer(measurement_model$attenuation_factor)
  n_parts <- nlevels(measurement_model$attenuation_factor)

  part_slice <- lapply(seq_len(n_parts), function(i) {
    do.call(slice, tuning)
  })

  function(current, warming_up) {
    z <- as.vector(Z2_tilde - X %*% omega(current))

    for (i in seq_len(n_parts)) {
      z_i <- z[attenuation_index == i]

      output <- part_slice[[i]](current$gamma[i], function(param_i) {
        current2 <- current
        current2$gamma[i] <- param_i

        log_prior_value <- log_prior(measurement_model, current2)
        if (!is.finite(log_prior_value)) return(log_prior_value)

        (
          log_prior_value
          + .dmvnorm(z_i, covariance = Sigma_epsilon(
            current2,
            parts = i
          ), log = TRUE)
        )
      }, learn = warming_up, include_n_evaluations = TRUE)
      log_trace(paste0(
        'gamma[{i}] = {round(output$sample, 3)} took {output$n_evaluations}',
        ' evaluations, w = {output$w}'
      ))
      current$gamma[i] <- output$sample
    }

    current
  }
}
