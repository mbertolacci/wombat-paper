.make_rho_ell_sampler <- function(
  measurement_model,
  process_model,
  tuning,
  name = c('rho', 'ell'),
  omega = .make_omega(process_model, measurement_model),
  X = .make_X_omega(process_model, measurement_model),
  Sigma_epsilon = .make_Sigma_epsilon(measurement_model),
  Z2_tilde = calculate(measurement_model, 'Z2_tilde', process_model)
) {
  name <- match.arg(name)

  if (name == 'rho') {
    if (!is.null(measurement_model[['rho']])) {
      return(function(current, ...) {
        current$rho <- measurement_model[['rho']]
        current
      })
    }
  } else {
    if (
      !is.null(measurement_model[['ell']])
      || (
        !is.null(measurement_model[['rho']])
        && all(measurement_model[['rho']] == 0)
      )
    ) {
      return(function(current, ...) {
        current$ell <- measurement_model[['ell']]
        current
      })
    }
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

      tryCatch({
        output <- part_slice[[i]](current[[name]][i], function(param_i) {
          current2 <- current
          current2[[name]][i] <- param_i

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
      }, error = function(e) {
        log_error(paste0(
          '{name}[{i}] sampler failed (group name',
          ' {levels(measurement_model$attenuation_factor)[i]})'
        ))
        stop(e)
      })
      log_trace(paste0(
        '{name}[{i}] = {round(output$sample, 3)} took {output$n_evaluations}',
        ' evaluations, w = {output$w}'
      ))
      current[[name]][i] <- output$sample
    }

    current
  }
}
