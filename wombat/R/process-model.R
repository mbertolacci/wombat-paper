#' @export
flux_process_model <- function(
  control_emissions,
  control_mole_fraction,
  perturbations,
  sensitivities,
  lag = months(3),
  H = transport_matrix(
    perturbations,
    control_mole_fraction,
    sensitivities,
    lag
  ),
  alpha,
  Gamma = sparseMatrix(
    i = seq_len(ncol(H)),
    j = rep(
      seq_along(unique(perturbations$region)),
      ncol(H) / length(unique(perturbations$region))
    )
  ),
  kappa = rep(0, length(unique(perturbations$region))),
  kappa_prior_mean = 0,
  kappa_prior_variance = 0.3 ^ 2,
  a,
  a_prior = list(
    shape1 = 1,
    shape2 = 1
  ),
  a_factor = factor(
    sprintf('Region %d', sort(unique(perturbations$region))),
    levels = sprintf('Region %d', sort(unique(perturbations$region)))
  ),
  w,
  w_prior = gamma_quantile_prior(1 / 10 ^ 2, 1 / 0.1 ^ 2),
  w_factor = factor(
    sprintf('Region %d', sort(unique(perturbations$region))),
    levels = sprintf('Region %d', sort(unique(perturbations$region)))
  ),
  Psi = latitudinal_random_effects(control_mole_fraction),
  eta,
  eta_prior_mean = 0,
  eta_prior_variance = 4,
  eta_prior_precision = Diagonal(ncol(Psi), 1 / eta_prior_variance)
) {
  stopifnot_within <- function(x, y) {
    stopifnot(
      all(x %in% y) && all(y %in% x)
    )
  }

  stopifnot_within(control_emissions$model_id, perturbations$model_id)
  stopifnot_within(
    perturbations$from_month_start,
    sensitivities$from_month_start
  )
  stopifnot_within(perturbations$region, sensitivities$region)
  stopifnot_within(sensitivities$model_id, control_mole_fraction$model_id)

  stopifnot(!is.unsorted(control_mole_fraction$time))

  regions <- sort(unique(perturbations$region))
  perturbations <- perturbations %>%
    arrange(from_month_start, region, model_id)

  if (!missing(alpha)) {
    alpha <- .recycle_vector_to(alpha, ncol(H))
  }

  if (!missing(kappa)) {
    kappa <- .recycle_vector_to(kappa, length(regions))
  }

  if (!missing(w)) {
    w <- .recycle_vector_to(w, length(regions))
  }

  kappa_prior_mean <- .recycle_vector_to(kappa_prior_mean, ncol(Gamma))
  kappa_prior_variance <- .recycle_vector_to(kappa_prior_variance, ncol(Gamma))

  eta_prior_mean <- .recycle_vector_to(eta_prior_mean, ncol(Psi))
  if (!missing(eta)) {
    eta <- .recycle_vector_to(eta, ncol(Psi))
  }

  stopifnot(is.list(a_prior))
  stopifnot(length(a_prior) == 2)
  stopifnot(length(a_factor) == length(regions))
  stopifnot(is.list(w_prior))
  stopifnot(length(w_prior) == 2 || length(w_prior) == 4)
  stopifnot(length(w_factor) == length(regions))
  stopifnot(nrow(eta_prior_precision) == ncol(Psi))
  stopifnot(ncol(eta_prior_precision) == ncol(Psi))

  structure(.remove_nulls_and_missing(mget(c(
    'control_emissions',
    'control_mole_fraction',
    'perturbations',
    'lag',
    'H',
    'regions',
    'alpha',
    'Gamma',
    'kappa',
    'kappa_prior_mean',
    'kappa_prior_variance',
    'a',
    'a_prior',
    'a_factor',
    'w',
    'w_prior',
    'w_factor',
    'Psi',
    'eta',
    'eta_prior_mean',
    'eta_prior_precision'
  ))), class = 'flux_process_model')
}

#' @export
update.flux_process_model <- function(model, ...) {
  current_arguments <- .remove_nulls_and_missing(model[c(
    'control_emissions',
    'control_mole_fraction',
    'perturbations',
    'lag',
    'H',
    'alpha',
    'Gamma',
    'kappa',
    'kappa_prior_mean',
    'kappa_prior_variance',
    'a',
    'a_prior',
    'a_factor',
    'w',
    'w_prior',
    'w_factor',
    'Psi',
    'eta',
    'eta_prior_mean',
    'eta_prior_precision'
  )])
  update_arguments <- list(...)

  # Ensure that current_arguments doesn't override anything
  terminal_arguments <- list(
    H = c('perturbations', 'control_mole_fraction', 'lag'),
    Psi = c('control_mole_fraction'),
    eta_prior_precision = c('control_mole_fraction', 'Psi')
  )
  for (name in names(terminal_arguments)) {
    if (any(terminal_arguments[[name]] %in% names(update_arguments))) {
      current_arguments[[name]] <- NULL
    }
  }

  do.call(flux_process_model, .extend_list(
    current_arguments,
    update_arguments
  ))
}

#' @export
transport_matrix <- function(
  perturbations,
  control_mole_fraction,
  sensitivities,
  lag = months(3)
) {
  if (!is.infinite(lag)) {
    log_debug('Truncating sensitivities')
    # NOTE(mgnb): do lag computation on the month_start from control because
    # it's much shorter than doing it after it's joined to sensitivities
    control_month_start_lag <- to_month_start(control_mole_fraction$time) - lag
    truncated_sensitivities <- sensitivities %>%
      mutate(
        month_start_lag = control_month_start_lag[
          match(model_id, control_mole_fraction$model_id)
        ]
      ) %>%
      filter(
        month_start_lag <= from_month_start
      )
  } else {
    truncated_sensitivities <- sensitivities
  }

  log_debug('Adding row and column indices')
  column_indices <- expand.grid(
    region = sort(unique(perturbations$region)),
    from_month_start = sort(unique(perturbations$from_month_start))
  ) %>%
    mutate(column_index = 1 : n())

  row_indices <- data.frame(
    model_id = sort(unique(control_mole_fraction$model_id))
  ) %>%
    mutate(row_index = 1 : n())

  indexed_sensitivities <- truncated_sensitivities %>%
    left_join(column_indices, by = c('region', 'from_month_start')) %>%
    left_join(row_indices, by = c('model_id'))

  log_debug('Constructing transport matrix')
  sparseMatrix(
    i = indexed_sensitivities$row_index,
    j = indexed_sensitivities$column_index,
    x = indexed_sensitivities$co2_sensitivity,
    dims = c(nrow(control_mole_fraction), nrow(column_indices))
  )
}

#' @export
latitudinal_random_effects <- function(
  df,
  n_latitude_bands = 5,
  latitude_basis_scale = 50,
  time_varying = TRUE,
  intercept = TRUE
) {
  log_debug('Contructing basis and design matrix')
  basis <- FRK::local_basis(
    FRK::real_line(),
    loc = matrix(seq(-90, 90, length = n_latitude_bands)),
    scale = rep(latitude_basis_scale, n_latitude_bands)
  )
  df <- df %>% arrange(model_id)
  if (time_varying) {
    output <- df %>%
      select(time, latitude) %>%
      mutate(month_start = to_month_start(time)) %>%
      group_by(month_start) %>%
      group_map(
        ~ FRK::eval_basis(basis, as.matrix(.x$latitude))
      ) %>%
      bdiag()
  } else {
    output <- FRK::eval_basis(basis, as.matrix(df$latitude))
  }

  if (intercept) {
    output <- cbind(1, output)
  }
  attr(output, 'is_latitudinal') <- TRUE
  attr(output, 'n_latitude_bands') <- n_latitude_bands
  attr(output, 'time_varying') <- time_varying
  attr(output, 'intercept') <- intercept
  attr(output, 'basis') <- basis
  output
}

#' @export
generate.flux_process_model <- function(model, n_samples = 1) {
  a_sample <- t(replicate(n_samples, rbeta(
    nlevels(model$a_factor),
    shape1 = model$a_prior[['shape1']],
    shape2 = model$a_prior[['shape2']]
  )))
  if (!is.null(model[['a']])) {
    is_fixed <- which(!is.na(model$a))
    fixed_a <- model$a[is_fixed]
    model$a <- a_sample
    for (i in seq_along(is_fixed)) {
      model$a[, is_fixed[i]] <- fixed_a[i]
    }
  } else {
    model$a <- a_sample
  }

  # Not yet supported
  stopifnot(!('lower' %in% model$w_prior))
  w_sample <- t(replicate(n_samples, rgamma(
    nlevels(model$w_factor),
    shape = ifelse(model$w_prior[['shape']] <= 0, 1, model$w_prior[['shape']]),
    rate = ifelse(model$w_prior[['rate']] <= 0, 1, model$w_prior[['rate']])
  )))
  if (any(model$w_prior[['shape']] < 0 | model$w_prior[['rate']] < 0)) {
    warning(
      'Improper prior for w; generating values using incorrect proper prior'
    )
  }
  if (!is.null(model[['w']])) {
    is_fixed <- which(!is.na(model$w))
    fixed_w <- model$w[is_fixed]
    model$w <- w_sample
    for (i in seq_along(is_fixed)) {
      model$w[, is_fixed[i]] <- fixed_w[i]
    }
  } else {
    model$w <- w_sample
  }

  .recycle_index <- function(i, n) {
    1 + ((i - 1) %% n)
  }
  .as_matrix <- function(x) {
    if (length(dim(x)) == 2) x else t(x)
  }

  if (is.null(model[['kappa']])) {
    model$kappa <- t(replicate(n_samples, rnorm(
      ncol(model$Gamma),
      mean = model$kappa_prior_mean,
      sd = sqrt(model$kappa_prior_variance)
    )))
  }

  if (is.null(model[['alpha']])) {
    a_matrix <- .as_matrix(model$a)
    w_matrix <- .as_matrix(model$w)
    kappa_matrix <- .as_matrix(model$kappa)
    model$alpha <- t(sapply(seq_len(n_samples), function(index) {
      as.vector(
        model$Gamma %*% kappa_matrix[
          .recycle_index(index, nrow(kappa_matrix)),
        ] + .sample_normal_precision(.make_Q_alpha(model)(list(
          a = a_matrix[.recycle_index(index, nrow(a_matrix)), ],
          w = w_matrix[.recycle_index(index, nrow(w_matrix)), ]
        )))
      )
    }))
  }

  if (is.null(model[['eta']])) {
    if (ncol(model$eta_prior_precision) > 0) {
      model$eta <- t(replicate(
        n_samples,
        .sample_normal_precision(model$eta_prior_precision)
      ))
    } else {
      model$eta <- numeric(0)
    }
  }

  simplify <- function(x) {
    if (is.matrix(x) && nrow(x) == 1) x[1, ] else x
  }

  for (x in c('a', 'w', 'kappa', 'alpha', 'eta')) {
    model[[x]] <- simplify(model[[x]])
  }

  model
}

#' @export
calculate.flux_process_model <- function(
  x,
  name = c('Y2_tilde', 'Y2', 'Y2_control', 'Y1_tilde', 'Y1', 'H_alpha'),
  parameters = x
) {
  name <- match.arg(name)

  parameters <- lapply(parameters[c('alpha', 'eta')], function(x) {
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

  get_samples <- function(l, r) {
    as.matrix(t(tcrossprod(l, r)))
  }

  add_rowwise <- function(x, y) {
    if (is.vector(x) && is.matrix(y)) {
      # HACK(mgnb): this is the easiest way to add to each row
      t(x + t(y))
    } else {
      x + y
    }
  }

  output <- if (name == 'Y2_tilde') {
    get_samples(x$H, parameters$alpha) + get_samples(x$Psi, parameters$eta)
  } else if (name == 'Y2') {
    add_rowwise(x$control_mole_fraction$co2, calculate(
      x,
      'Y2_tilde',
      parameters
    ))
  } else if (name == 'Y2_control') {
    x$control_mole_fraction$co2
  } else if (name == 'H_alpha') {
    get_samples(x$H, parameters$alpha)
  }

  if (is.matrix(output) && nrow(output) == 1) {
    output[1, ]
  } else {
    output
  }
}

#' @export
log_prior.flux_process_model <- function(model, parameters = model) {
  output <- 0

  if (is.null(model[['a']]) || anyNA(model[['a']])) {
    a_shape1 <- .recycle_vector_to(
      model$a_prior[['shape1']],
      length(parameters$a)
    )
    a_shape2 <- .recycle_vector_to(
      model$a_prior[['shape2']],
      length(parameters$a)
    )
    a_na <- if (is.null(model[['a']])) {
      seq_len(length(parameters$a))
    } else {
      is.na(model[['a']])
    }
    output <- output + sum(dbeta(
      parameters$a[a_na],
      shape1 = a_shape1[a_na],
      shape2 = a_shape2[a_na],
      log = TRUE
    ))
  }

  if (is.null(model[['w']]) || anyNA(model[['w']])) {
    w_shape <- .recycle_vector_to(
      model$w_prior[['shape']],
      length(parameters$w)
    )
    w_rate <- .recycle_vector_to(model$w_prior[['rate']], length(parameters$w))
    w_na <- if (is.null(model[['w']])) {
      seq_len(length(parameters$w))
    } else {
      is.na(model[['w']])
    }
    output <- output + sum(ifelse(
      w_shape[w_na] <= 0 | w_rate[w_na] <= 0,
      if (any(parameters$w[w_na] <= 0)) -Inf else 0,
      dgamma(
        parameters$w[w_na],
        shape = w_shape[w_na],
        rate = w_rate[w_na],
        log = TRUE
      )
    ))
  }

  output
}

#' @export
flux_aggregator <- function(model, filter_expr) {
  filter_expr <- enquo(filter_expr)

  control_aggregate <- model$control_emissions %>%
    mutate(
      test_condition = !! filter_expr,
      flux = if_else(
        test_condition,
        # kgCO2 / s / m ^ 2 => PgC
        flux_density
          * area
          * 10 ^ (3 - 15) / 44.01
          * 12.01
          * lubridate::days_in_month(month_start)
          * 24 * 60 * 60,
        0
      )
    ) %>%
    group_by(month_start) %>%
    summarise(
      total_flux = sum(flux)
    ) %>%
    ungroup()

  row_indices <- control_aggregate %>%
    select(month_start) %>%
    mutate(i = 1 : n())

  column_indices <- expand.grid(
    region = model$regions,
    from_month_start = sort(unique(model$perturbations$from_month_start))
  ) %>%
    mutate(j = 1 : n())

  matching_control <- model$control_emissions %>%
    filter(!! filter_expr) %>%
    select(model_id, type, month_start, area)

  aggregate_df <- matching_control %>%
    left_join(
      model$perturbations,
      by = c('model_id', 'type')
    ) %>%
    mutate(
      # kgCO2 / s / m ^ 2 => PgC
      flux = flux_density
        * area
        * 10 ^ (3 - 15) / 44.01
        * 12.01
        * lubridate::days_in_month(month_start)
        * 24 * 60 * 60
    ) %>%
    group_by(from_month_start, region, month_start) %>%
    summarise(
      total_flux = sum(flux)
    ) %>%
    ungroup() %>%
    left_join(row_indices, by = 'month_start') %>%
    left_join(column_indices, by = c('from_month_start', 'region'))

  list(
    total = control_aggregate,
    Phi = sparseMatrix(
      i = aggregate_df$i,
      j = aggregate_df$j,
      x = aggregate_df$total_flux,
      dims = c(
        nrow(row_indices),
        nrow(column_indices)
      )
    )
  )
}

#' Aggregate monthly fluxes according to a constraint
#' @export
aggregate_flux <- function(
  model,
  filter_expr = TRUE,
  parameters = model,
  aggregator = flux_aggregator(
    model,
    !! enquo(filter_expr)
  )
) {
  if (missing(parameters) && !('alpha' %in% names(model))) {
    parameters <- list(alpha = model$alpha_prior_mean)
  }

  tibble::tibble(
    month_start = aggregator$total$month_start,
    flux = if (is.vector(parameters[['alpha']])) {
      aggregator$total$total_flux + as.vector(
        aggregator$Phi %*% parameters[['alpha']]
      )
    } else {
      aggregator$total$total_flux + as.matrix(
        aggregator$Phi %*% t(parameters[['alpha']])
      )
    }
  )
}

.make_Q_alpha <- function(model) {
  n_alpha <- ncol(model$H)
  n_regions <- length(model$regions)
  n_times <- n_alpha / n_regions

  permutation <- rep(0, n_alpha)
  for (i in seq_len(n_times)) {
    permutation[
      (1 + (i - 1) * n_regions) : (i * n_regions)
    ] <- i + (0 : (n_regions - 1)) * n_times
  }

  function(params, parts = seq_len(n_regions)) {
    a_region <- params$a[model$a_factor]
    w_region <- params$w[model$w_factor]
    # TODO(mgnb): the permutation is not quite appropriate if parts is not full
    bdiag(lapply(parts, function(i) {
      w_region[i] * .ar1_Q(n_times, a_region[i])
    }))[
      permutation,
      permutation
    ]
  }
}

.make_alpha_log_likelihood <- function(model) {
  n_times <- ncol(model$H) / length(model$regions)

  function(params) {
    a_region <- params$a[model$a_factor]
    w_region <- params$w[model$w_factor]

    alpha_matrix <- matrix(
      params$alpha - as.vector(model$Gamma %*% params$kappa),
      ncol = n_times
    )
    alpha_rhs <- alpha_matrix[, 1 : (ncol(alpha_matrix) - 1)]
    alpha_mean <- cbind(0, a_region * alpha_rhs)
    alpha_sd <- 1 / sqrt(cbind(
      w_region,
      # HACK(mgnb): first term makes a zero matrix with the right shape,
      # second term sets each column to the right value
      0 * alpha_rhs + w_region / (1 - a_region ^ 2)
    ))

    sum(dnorm(
      alpha_matrix,
      mean = alpha_mean,
      sd = alpha_sd,
      log = TRUE
    ))
  }
}
