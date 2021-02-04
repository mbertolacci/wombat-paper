.make_omega_parts <- function(
  process_model,
  measurement_model,
  variables = c('alpha', 'beta', 'eta')
) {
  n_parts <- c(
    alpha = ncol(process_model$H),
    beta = ncol(measurement_model$A),
    eta = ncol(process_model$Psi)
  )
  do.call(c, lapply(variables, function(variable) {
    rep(variable, n_parts[variable])
  }))
}

.make_omega <- function(
  process_model,
  measurement_model,
  variables = c('alpha', 'beta', 'eta')
) {
  omega_parts <- .make_omega_parts(process_model, measurement_model, variables)

  function(parameters) {
    omega <- rep(0, length(omega_parts))
    for (variable in variables) {
      omega[omega_parts == variable] <- parameters[[variable]]
    }
    omega
  }
}

.make_omega_unpack <- function(
  process_model,
  measurement_model,
  variables = c('alpha', 'beta', 'eta')
) {
  omega_parts <- .make_omega_parts(process_model, measurement_model, variables)

  function(parameters, omega) {
    for (variable in variables) {
      parameters[[variable]] <- omega[omega_parts == variable]
    }
    parameters
  }
}

.make_mu_omega <- function(
  process_model,
  measurement_model,
  variables = c('alpha', 'beta', 'eta')
) {
  function(params) {
    do.call(c, lapply(variables, function(variable) {
      if (variable == 'alpha') {
        as.vector(process_model$Gamma %*% params[['kappa']])
      } else if (variable == 'beta') {
        measurement_model$beta_prior_mean
      } else if (variable == 'eta') {
        process_model$eta_prior_mean
      }
    }))
  }
}

.make_Q_omega <- function(
  process_model,
  measurement_model,
  variables = c('alpha', 'beta', 'eta')
) {
  if ('alpha' %in% variables) {
    Q_alpha <- .make_Q_alpha(process_model)
  }

  function(params) {
    bdiag(lapply(variables, function(variable) {
      if (variable == 'alpha') {
        Q_alpha(params)
      } else if (variable == 'beta') {
        measurement_model$beta_prior_precision
      } else if (variable == 'eta') {
        process_model$eta_prior_precision
      }
    }))
  }
}

.make_X_omega <- function(
  process_model,
  measurement_model,
  variables = c('alpha', 'beta', 'eta')
) {
  do.call(cbind, lapply(variables, function(variable) {
    if (variable == 'alpha') {
      measurement_model$C %*% process_model$H
    } else if (variable == 'beta') {
      measurement_model$A
    } else if (variable == 'eta') {
      measurement_model$C %*% process_model$Psi
    }
  }))
}

.make_chol_Q_omega_conditional <- function(
  process_model,
  measurement_model,
  variables = c('alpha', 'beta', 'eta'),
  X = .make_X_omega(process_model, measurement_model, variables),
  Xt_Q_epsilon_X = .make_Xt_Q_epsilon_X(X, measurement_model),
  Q_omega = .make_Q_omega(process_model, measurement_model, variables)
) {
  function(parameters) {
    chol(Q_omega(parameters) + Xt_Q_epsilon_X(parameters))
  }
}
