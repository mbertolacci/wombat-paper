#' @export
flux_measurement_model <- function(
  observations,
  biases,
  matching,
  process_model,
  C = sparseMatrix(
    i = seq_len(nrow(observations)),
    j = matching,
    dims = c(nrow(observations), nrow(process_model$control_mole_fraction))
  ),
  attenuation_variables,
  attenuation_factor = if (!missing(attenuation_variables)) {
    interaction(
      as.list(observations[attenuation_variables]),
      sep = ':'
    )
  } else {
    factor(rep(1, nrow(observations)))
  },
  time = observations$time,
  measurement_variance = observations$co2_error ^ 2,
  A = model.matrix(biases, observations)[, -1, drop = FALSE],
  beta,
  beta_prior_mean = 0,
  beta_prior_variance = 4,
  beta_prior_precision = Diagonal(
    ncol(A),
    1 / beta_prior_variance
  ),
  gamma,
  gamma_prior = gamma_quantile_prior(0.1, 1.9),
  rho,
  rho_prior = list(lower = 0, upper = 1),
  ell,
  ell_prior = list(shape = 1, rate = 1, unit = 'secs')
) {
  if (
    is.unsorted(order(attenuation_factor, time))
  ) {
    stop('observations must be sorted by attenuation_factor then time')
  }
  if (!missing(beta)) {
    beta <- .recycle_vector_to(beta, ncol(A))
  }
  beta_prior_mean <- .recycle_vector_to(beta_prior_mean, ncol(A))
  if (!missing(gamma)) {
    gamma <- .recycle_vector_to(gamma, nlevels(attenuation_factor))
  }
  if (!missing(rho)) {
    rho <- .recycle_vector_to(rho, nlevels(attenuation_factor))
  }
  if (!missing(ell)) {
    ell <- .recycle_vector_to(ell, nlevels(attenuation_factor))
  }
  stopifnot(is.factor(attenuation_factor))
  stopifnot(nrow(C) == nrow(observations))
  stopifnot(length(attenuation_factor) == nrow(observations))
  stopifnot(length(time) == nrow(observations))
  stopifnot(length(measurement_variance) == nrow(observations))
  stopifnot(nrow(A) == nrow(observations))
  if (!missing(beta)) {
    stopifnot(length(beta) == ncol(A))
  }
  stopifnot(ncol(beta_prior_precision) == ncol(A))
  stopifnot(nrow(beta_prior_precision) == ncol(A))
  stopifnot(is.list(gamma_prior))
  stopifnot(length(gamma_prior) %in% c(2, 4))
  stopifnot(is.list(rho_prior))
  stopifnot(length(rho_prior) == 2)
  stopifnot(is.list(ell_prior))
  stopifnot(length(ell_prior) == 3)

  structure(.remove_nulls_and_missing(mget(c(
    'observations',
    'C',
    'time',
    'measurement_variance',
    'biases',
    'A',
    'beta',
    'beta_prior_mean',
    'beta_prior_precision',
    'attenuation_variables',
    'attenuation_factor',
    'gamma',
    'gamma_prior',
    'rho',
    'rho_prior',
    'ell',
    'ell_prior'
  ))), class = 'flux_measurement_model')
}

#' @export
update.flux_measurement_model <- function(model, ...) {
  current_arguments <- .remove_nulls_and_missing(model[c(
    'observations',
    'C',
    'time',
    'measurement_variance',
    'biases',
    'A',
    'beta',
    'beta_prior_mean',
    'beta_prior_precision',
    'attenuation_variables',
    'attenuation_factor',
    'gamma',
    'gamma_prior',
    'rho',
    'rho_prior',
    'ell',
    'ell_prior'
  )])
  update_arguments <- list(...)

  # Ensure that current_arguments doesn't override anything
  terminal_arguments <- list(
    A = 'biases',
    C = c('matching', 'process_model'),
    attenuation_factor = 'attenuation_variables',
    beta_prior_precision = c('biases', 'beta_prior_variance'),
    time = 'observations',
    measurement_variance = 'observations'
  )
  for (name in names(terminal_arguments)) {
    if (any(terminal_arguments[[name]] %in% names(update_arguments))) {
      current_arguments[[name]] <- NULL
    }
  }

  do.call(flux_measurement_model, .extend_list(
    current_arguments,
    update_arguments
  ))
}

#' @export
generate.flux_measurement_model <- function(model, process_model) {
  if (is.null(model[['beta']])) {
    model$beta <- (
      model$beta_prior_mean
      + .sample_normal_precision(model$beta_prior_precision)
    )
  }

  if (is.null(model[['gamma']])) {
    model$gamma <- rgamma(
      nlevels(model$attenuation_factor),
      shape = model$gamma_prior[['shape']],
      rate = model$gamma_prior[['rate']]
    )
  }

  if (is.null(model[['rho']])) {
    model$rho <- runif(
      nlevels(model$attenuation_factor),
      min = model$rho_prior[['lower']],
      max = model$rho_prior[['upper']]
    )
  }

  if (is.null(model[['ell']])) {
    model$ell <- rgamma(
      nlevels(model$attenuation_factor),
      shape = model$ell_prior[['shape']],
      rate = model$ell_prior[['rate']]
    )
  }

  if (!missing(process_model)) {
    Sigma_epsilon <- .make_Sigma_epsilon(model)(model)
    epsilon <- .rmvnorm(covariance = Sigma_epsilon)
    model$observations$co2 <- as.vector(
      model$C %*% calculate(process_model, 'Y2')
      + model$A %*% model$beta
      + epsilon
    )
  }

  model
}

#' @export
generate_posterior_predictive <- function(
  model,
  name = c('Z2_hat', 'Z2_tilde_hat'),
  process_model,
  samples
) {
  mean_part <- calculate(model, name, process_model, samples)

  Sigma_epsilon <- .make_Sigma_epsilon(model)
  epsilon <- t(sapply(seq_len(nrow(samples$gamma)), function(iteration) {
    as.vector(.rmvnorm(covariance = Sigma_epsilon(list(
      gamma = samples$gamma[iteration, ],
      rho = samples$rho[iteration, ],
      ell = samples$ell[iteration, ]
    ))))
  }))

  mean_part + epsilon
}

#' @export
filter.flux_measurement_model <- function(model, expr) {
  expr_s <- substitute(expr)
  indices <- eval(expr_s, model$observations, parent.frame())

  model$observations <- model$observations[indices, , drop = FALSE]
  model$time <- model$time[indices]
  model$measurement_variance <- model$measurement_variance[indices]
  model$C <- model$C[indices, , drop = FALSE]

  model$A <- model$A[indices, , drop = FALSE]
  if (ncol(model$A) > 0) {
    non_empty_columns <- colSums(abs(model$A)) != 0
    model$A <- model$A[, non_empty_columns, drop = FALSE]
    model$beta_prior_mean <- model$beta_prior_mean[non_empty_columns]
    model$beta_prior_precision <- model$beta_prior_precision[
      non_empty_columns,
      non_empty_columns,
      drop = FALSE
    ]
    if (!is.null(model[['beta']])) {
      model[['beta']] <- model[['beta']][non_empty_columns]
    }
  }

  original_levels <- levels(model$attenuation_factor)
  model$attenuation_factor <- droplevels(model$attenuation_factor[indices])
  for (name in c('gamma', 'rho', 'ell')) {
    if (!is.null(model[[name]])) {
      model[[name]] <- model[[name]][
        original_levels %in% levels(model$attenuation_factor)
      ]
    }
  }

  model
}

#' @export
calculate.flux_measurement_model <- function(
  x,
  name = c(
    'Z2',
    'Z2_hat',
    'Z2_debiased',
    'Z2_tilde',
    'Z2_tilde_debiased',
    'Z2_tilde_hat',
    'Y2',
    'Y2_control',
    'Y2_tilde'
  ),
  process_model,
  parameters = x
) {
  name <- match.arg(name)

  parameters <- lapply(parameters[c('beta', 'eta', 'alpha')], function(x) {
    if (is.null(x)) return(x)
    if (is.vector(x)) {
      t(x)
    } else {
      if (ncol(x) == 0) {
        x
      } else {
        as.matrix(x)
      }
    }
  })

  calculate_bias_correction <- function() {
    if (ncol(x$A) == 0) 0
    else as.matrix(t(tcrossprod(x$A, parameters$beta)))
  }

  add_rowwise <- function(x, y) {
    if (is.vector(y) && is.matrix(x)) {
      tmp <- x
      x <- y
      y <- tmp
    }
    if (is.vector(x) && is.matrix(y)) {
      # HACK(mgnb): this is the easiest way to add to each row
      t(x + t(y))
    } else {
      x + y
    }
  }

  output <- if (name == 'Z2') {
    x$observations$co2
  } else if (name == 'Z2_hat') {
    add_rowwise(
      calculate(x, 'Y2', process_model, parameters),
      calculate_bias_correction()
    )
  } else if (name == 'Z2_debiased') {
    add_rowwise(
      calculate(x, 'Z2', process_model, parameters),
      -calculate_bias_correction()
    )
  } else if (name == 'Z2_tilde') {
    add_rowwise(
      calculate(x, 'Z2', process_model, parameters),
      -calculate(x, 'Y2_control', process_model, parameters)
    )
  } else if (name == 'Z2_tilde_debiased') {
    add_rowwise(
      calculate(x, 'Z2_tilde', process_model, parameters),
      -calculate_bias_correction()
    )
  } else if (name == 'Z2_tilde_hat') {
    add_rowwise(
      calculate(x, 'Z2_hat', process_model, parameters),
      -calculate(x, 'Y2_control', process_model, parameters)
    )
  } else {
    # Anything left falls through to the process model
    rhs <- calculate(process_model, name, parameters)
    if (is.matrix(rhs)) {
      as.matrix(tcrossprod(rhs, x$C))
    } else {
      as.matrix(t(x$C %*% rhs))
    }
  }

  if (is.matrix(output) && nrow(output) == 1) {
    output[1, ]
  } else {
    output
  }
}

#' @export
log_prior.flux_measurement_model <- function(model, parameters = model) {
  output <- 0

  if (is.null(model[['gamma']])) {
    gamma <- parameters$gamma

    if ('lower' %in% names(model$gamma_prior)) {
      if (
        any(gamma <= model$gamma_prior[['lower']])
        || any(gamma >= model$gamma_prior[['upper']])
      ) {
        return(-Inf)
      }
    } else {
      if (any(gamma <= 0)) {
        return(-Inf)
      }
    }

    output <- output + sum(dgamma(
      gamma,
      shape = model$gamma_prior[['shape']],
      rate = model$gamma_prior[['rate']],
      log = TRUE
    ))
  }

  if (is.null(model[['rho']])) {
    output <- output + sum(dunif(
      parameters[['rho']],
      min = model$rho_prior[['lower']],
      max = model$rho_prior[['upper']],
      log = TRUE
    ))
  }

  if (is.null(model[['ell']])) {
    output <- output + sum(dgamma(
      parameters[['ell']],
      shape = model$ell_prior[['shape']],
      rate = model$ell_prior[['rate']],
      log = TRUE
    ))
  }

  output
}

.zero_rows <- function(A, include_rows) {
  At <- as(A, 'dgTMatrix')
  include <- (At@i + 1) %in% include_rows
  At@i <- At@i[include]
  At@j <- At@j[include]
  At@x <- At@x[include]
  as(At, 'dgCMatrix')
}

.Xt_Q_epsilon_X_parts <- function(
  X,
  model,
  Sigma_epsilon,
  has_cross_correlations
) {
  attenuation_index <- as.integer(model$attenuation_factor)
  n_gamma <- nlevels(model$attenuation_factor)
  n_per_gamma <- table(model$attenuation_factor)

  part_ij <- rbind(
    cbind(seq_len(n_gamma), seq_len(n_gamma)),
    if (has_cross_correlations && n_gamma > 1) {
      t(combn(n_gamma, 2))
    } else {
      NULL
    }
  )
  part_x <- apply(part_ij, 1, function(ij) {
    i <- ij[1]
    j <- ij[2]

    if (n_per_gamma[i] == 0 || n_per_gamma[j] == 0) {
      return(sparseMatrix(i = NULL, j = NULL, dims = c(ncol(X), ncol(X))))
    }

    Xi <- .zero_rows(X, which(attenuation_index == i))
    if (i == j) {
      forceSymmetric(crossprod(Xi, solve(Sigma_epsilon, Xi)))
    } else {
      Xj <- .zero_rows(X, which(attenuation_index == j))
      forceSymmetric(
        crossprod(Xi, solve(Sigma_epsilon, Xj))
        + crossprod(Xj, solve(Sigma_epsilon, Xi))
      )
    }
  })

  # NOTE(mgnb): drop parts equal to zero
  keep_part <- sapply(part_x, function(x) {
    nnzero(x) > 0
  })
  part_ij <- part_ij[keep_part, , drop = FALSE]
  part_x <- part_x[keep_part]

  list(
    ij = part_ij,
    x = part_x
  )
}

.make_Xt_Q_epsilon_X <- function(
  X,
  model,
  Sigma_epsilon = .make_Sigma_epsilon(model)
) {
  attenuation_index <- as.integer(model$attenuation_factor)

  get_parts <- memoise::memoise(function(params) {
    params$gamma <- params$rho
    params$gamma[seq_along(params$gamma)] <- 1
    .Xt_Q_epsilon_X_parts(
      X,
      model,
      Sigma_epsilon(params),
      attr(Sigma_epsilon, 'has_cross_correlations')
    )
  }, cache = .cache_memory_fifo())

  actual <- memoise::memoise(function(params) {
    parts <- get_parts(params[c('rho', 'ell')])

    gamma_ij <- (
      sqrt(params$gamma)[parts$ij[, 1]] * sqrt(params$gamma)[parts$ij[, 2]]
    )

    Reduce(function(l, k) {
      if (is.null(l)) {
        gamma_ij[k] * parts$x[[k]]
      } else {
        l + gamma_ij[k] * parts$x[[k]]
      }
    }, seq_len(nrow(parts$ij)), NULL)
  }, cache = .cache_memory_fifo())

  function(params) {
    actual(params[c('gamma', 'rho', 'ell')])
  }
}

.make_Xt_Q_epsilon_X_tf <- function(
  X,
  model,
  Sigma_epsilon = .make_Sigma_epsilon(model)
) {
  attenuation_index <- as.integer(model$attenuation_factor)
  n_parts <- nlevels(model$attenuation_factor)

  if (!requireNamespace('tensorflow', quietly = TRUE)) {
    stop('tensorflow requested but not installed')
  }
  tf <- tensorflow::tf

  if (attr(Sigma_epsilon, 'has_cross_correlations')) {
    stop('Cross correlations in Sigma_epsilon not supported with Tensorflow')
  }

  as_tf <- function(x, ...) {
    tf$convert_to_tensor(x, dtype = tf$float32, ...)
  }

  X_parts_tf <- lapply(
    levels(model$attenuation_factor),
    function(k) {
      as_tf(as.matrix(X[model$attenuation_factor == k, ]))
    }
  )

  function(params) {
    output_parts <- lapply(seq_len(n_parts), function(i) {
      X_i <- X_parts_tf[[i]]
      Sigma_epsilon_i <- Sigma_epsilon(params, i)

      A_diag <- as_tf(Sigma_epsilon_i@A@x)
      A <- tf$linalg$LinearOperatorDiag(A_diag)
      B_major <- as_tf(Sigma_epsilon_i@B@major)
      B_minor <- as_tf(Sigma_epsilon_i@B@minor)
      O_major <- A_diag + B_major

      A_X_i <- A$matmul(X_i)
      rhs <- A_X_i - A$matmul(tf$linalg$tridiagonal_solve(
        list(B_minor, O_major, B_minor),
        A_X_i,
        diagonals_format = 'sequence',
        partial_pivoting = FALSE
      ))

      tf$matmul(X_i, rhs, transpose_a = TRUE)
    })

    forceSymmetric(as.array(tf$add_n(output_parts)))
  }
}

.make_Sigma_epsilon <- function(model) {
  attenuation_index <- as.integer(model$attenuation_factor)
  n_parts <- nlevels(model$attenuation_factor)

  time_parts <- split(model$time, model$attenuation_factor)
  ell_units <- .recycle_vector_to(model$ell_prior$unit, n_parts)
  diff_time_since_parts <- lapply(seq_len(n_parts), function(k) {
    diff(as.double(
      time_parts[[k]] - min(time_parts[[k]]),
      unit = ell_units[k]
    ))
  })
  precisions <- 1 / model$measurement_variance
  precision_parts <- split(precisions, model$attenuation_factor)
  cross_precision_parts <- precision_parts %>%
    lapply(function(x) {
      sd_x <- sqrt(x)
      head(sd_x, -1) * tail(sd_x, -1)
    })

  output <- memoise::memoise(function(params, parts = seq_len(n_parts)) {
    gamma <- params$gamma
    rho <- params$rho
    ell <- params$ell

    if (all(rho == 1)) {
      stop('rho = 1 not supported')
    }

    A <- FastDiagonal(
      x = (
        (gamma / (1 - rho))[model$attenuation_factor]
        * precisions
      )[attenuation_index %in% parts]
    )
    if (all(rho == 0)) {
      return(solve(A))
    }

    B <- bdiag_tridiagonal(lapply(parts, function(k) {
      .ou_precision(
        diff_time_since_parts[[k]],
        1 / ell[k],
        precision_parts[[k]] * gamma[k] / rho[k],
        cross_precision_parts[[k]] * gamma[k] / rho[k]
      )
    }))

    O <- TridiagonalMatrix(
      A@x + B@major,
      B@minor
    )

    WoodburyMatrix(
      A,
      B,
      O = O,
      symmetric = TRUE
    )
  }, cache = .cache_memory_fifo())
  attr(output, 'has_cross_correlations') <- FALSE
  output
}

.ou_precision <- function(diff_x, lambda, precisions, cross_precisions) {
  if (length(precisions) == 1) {
    return(TridiagonalMatrix(
      precisions,
      numeric(0)
    ))
  }

  e_lambda_d_x <- exp(-lambda * diff_x)
  e_2lambda_d_x <- exp(-2 * lambda * diff_x)

  r <- e_2lambda_d_x / (1 - e_2lambda_d_x)
  major <- c(r, 0) + c(0, r) + 1
  minor <- -e_lambda_d_x / (1 - e_2lambda_d_x)

  TridiagonalMatrix(
    major * precisions,
    minor * cross_precisions
  )
}
