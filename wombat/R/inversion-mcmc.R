#' @export
inversion_mcmc <- function(
  n_iterations = 1000,
  measurement_model,
  process_model,
  start = NULL,
  warm_up = 100,
  tuning = list(
    a = list(w = 0.25, min_w = 0.1, max_evaluations = 100),
    gamma = list(w = 1, min_w = 0.1, max_evaluations = 100),
    rho = list(w = 0.1, min_w = 0.05, max_evaluations = 100),
    ell = list(w = 1, min_w = 0.1, max_evaluations = 100)
  ),
  use_tensorflow = FALSE,
  show_progress = TRUE
) {
  if (!missing(tuning)) {
    tuning <- .extend_list(eval(formals(inversion_mcmc)$tuning), tuning)
  }

  n_kappa <- ncol(process_model$Gamma)
  n_alpha <- ncol(process_model$H)
  n_eta <- ncol(process_model$Psi)
  n_beta <- ncol(measurement_model$A)
  n_times <- n_alpha / length(process_model$regions)
  n_a <- nlevels(process_model$a_factor)
  n_w <- nlevels(process_model$w_factor)
  n_gamma <- nlevels(measurement_model$attenuation_factor)

  log_debug('Precomputing various quantities')
  X <- .make_X_omega(process_model, measurement_model)
  Sigma_epsilon <- .make_Sigma_epsilon(measurement_model)
  if (use_tensorflow) {
    Xt_Q_epsilon_X <- .make_Xt_Q_epsilon_X_tf(
      X,
      measurement_model,
      Sigma_epsilon = Sigma_epsilon
    )
  } else {
    Xt_Q_epsilon_X <- .make_Xt_Q_epsilon_X(
      X,
      measurement_model,
      Sigma_epsilon = Sigma_epsilon
    )
  }

  kappa_sampler <- .make_kappa_sampler(process_model)
  omega_sampler <- .make_omega_sampler(
    measurement_model,
    process_model,
    Xt_Q_epsilon_X = Xt_Q_epsilon_X
  )
  a_sampler <- .make_a_sampler(process_model, tuning[['a']])
  w_sampler <- .make_w_sampler(process_model)
  gamma_sampler <- .make_gamma_sampler(
    measurement_model,
    process_model,
    tuning = tuning[['gamma']]
  )
  rho_sampler <- .make_rho_ell_sampler(
    measurement_model,
    process_model,
    tuning = tuning[['rho']],
    name = 'rho'
  )
  ell_sampler <- .make_rho_ell_sampler(
    measurement_model,
    process_model,
    tuning = tuning[['ell']],
    name = 'ell'
  )

  log_debug('Setting start values')
  generated_process_model <- generate(process_model)[
    c('kappa', 'alpha', 'eta', 'a', 'w')
  ]
  start <- .extend_list(
    c(
      generated_process_model,
      generate(measurement_model)[c('beta', 'gamma', 'rho', 'ell')]
    ),
    start,
    .remove_nulls(process_model[c('kappa', 'alpha', 'eta', 'a', 'w')]),
    .remove_nulls(measurement_model[c('beta', 'gamma', 'rho', 'ell')])
  )[c('kappa', 'alpha', 'eta', 'beta', 'a', 'w', 'gamma', 'rho', 'ell')]
  if (any(is.na(start$a))) {
    start$a[is.na(start$a)] <- generated_process_model$a[is.na(start$a)]
  }
  if (any(is.na(start$w))) {
    start$w[is.na(start$w)] <- generated_process_model$w[is.na(start$w)]
  }

  kappa_samples <- matrix(NA, nrow = n_iterations, ncol = n_kappa)
  alpha_samples <- matrix(NA, nrow = n_iterations, ncol = n_alpha)
  eta_samples <- matrix(NA, nrow = n_iterations, ncol = n_eta)
  a_samples <- matrix(NA, nrow = n_iterations, ncol = n_a)
  w_samples <- matrix(NA, nrow = n_iterations, ncol = n_w)
  beta_samples <- matrix(NA, nrow = n_iterations, ncol = n_beta)
  gamma_samples <- matrix(NA, nrow = n_iterations, ncol = n_gamma)
  rho_samples <- matrix(NA, nrow = n_iterations, ncol = n_gamma)
  ell_samples <- matrix(NA, nrow = n_iterations, ncol = n_gamma)

  current <- start

  kappa_samples[1, ] <- current$kappa
  alpha_samples[1, ] <- current$alpha
  eta_samples[1, ] <- current$eta
  a_samples[1, ] <- current$a
  w_samples[1, ] <- current$w
  beta_samples[1, ] <- current$beta
  gamma_samples[1, ] <- current$gamma
  rho_samples[1, ] <- current$rho
  ell_samples[1, ] <- current$ell

  if (show_progress) {
    pb <- progress::progress_bar$new(
      total = n_iterations,
      format = '[:bar] :current/:total eta: :eta'
    )
    pb$tick()
  }

  log_debug('Starting sampler')
  for (iteration in 2 : n_iterations) {
    if (show_progress) {
      pb$tick()
    }

    log_trace('[{iteration}/{n_iterations}] Sampling kappa')
    current <- kappa_sampler(current)

    log_trace('[{iteration}/{n_iterations}] Sampling omega')
    current <- omega_sampler(current)

    log_trace('[{iteration}/{n_iterations}] Sampling a')
    current <- a_sampler(current, iteration <= warm_up)

    log_trace('[{iteration}/{n_iterations}] Sampling w')
    current <- w_sampler(current, iteration <= warm_up)

    log_trace('[{iteration}/{n_iterations}] Sampling gamma')
    current <- gamma_sampler(current, iteration <= warm_up)

    log_trace('[{iteration}/{n_iterations}] Sampling rho')
    current <- rho_sampler(current, iteration <= warm_up)

    log_trace('[{iteration}/{n_iterations}] Sampling ell')
    current <- ell_sampler(current, iteration <= warm_up)

    kappa_samples[iteration, ] <- current$kappa
    alpha_samples[iteration, ] <- current$alpha
    eta_samples[iteration, ] <- current$eta
    a_samples[iteration, ] <- current$a
    w_samples[iteration, ] <- current$w
    beta_samples[iteration, ] <- current$beta
    gamma_samples[iteration, ] <- current$gamma
    rho_samples[iteration, ] <- current$rho
    ell_samples[iteration, ] <- current$ell
  }

  region_month <- expand.grid(
    region = process_model$regions,
    month_index = seq_len(length(unique(
      process_model$control_emissions$month_start
    )))
  )
  colnames(kappa_samples) <- sprintf('kappa[%d]', seq_len(n_kappa))
  colnames(alpha_samples) <- sprintf(
    'alpha[%d, %d]',
    region_month$region,
    region_month$month_index
  )
  colnames(eta_samples) <- sprintf('eta[%d]', seq_len(ncol(eta_samples)))
  colnames(a_samples) <- levels(process_model$a_factor)
  colnames(w_samples) <- levels(process_model$w_factor)
  colnames(beta_samples) <- colnames(measurement_model$A)
  colnames(gamma_samples) <- levels(measurement_model$attenuation_factor)
  colnames(rho_samples) <- levels(measurement_model$attenuation_factor)
  colnames(ell_samples) <- levels(measurement_model$attenuation_factor)

  structure(
    list(
      kappa = coda::mcmc(kappa_samples),
      alpha = coda::mcmc(alpha_samples),
      eta = coda::mcmc(eta_samples),
      a = coda::mcmc(a_samples),
      w = coda::mcmc(w_samples),
      beta = coda::mcmc(beta_samples),
      gamma = coda::mcmc(gamma_samples),
      rho = coda::mcmc(rho_samples),
      ell = coda::mcmc(ell_samples)
    ),
    class = 'flux_inversion_mcmc'
  )
}

#' @export
window.flux_inversion_mcmc <- function(object, ...) {
  for (name in names(object)) {
    object[[name]] <- window(object[[name]], ...)
  }
  object
}

#' @export
plot_traces <- function(object, n_columns = 4) {
  matrix_to_long_df <- function(x) {
    as.data.frame(x) %>%
      mutate(iteration = 1 : n()) %>%
      tidyr::pivot_longer(-iteration) %>%
      mutate(name = factor(name, levels = colnames(x)))
  }

  trace_plot <- function(x, is_parsed = TRUE) {
    df <- matrix_to_long_df(x)

    df_summary <- df %>%
      group_by(name) %>%
      summarise(
        q025 = quantile(value, probs = 0.025),
        q500 = quantile(value, probs = 0.5),
        mean = mean(value),
        q975 = quantile(value, probs = 0.975)
      ) %>%
      ungroup() %>%
      mutate(
        label = sprintf(
          'mean = %.03g | 2.5%% = %.03g | 50%% = %.03g | 97.5%% = %.03g',
          mean,
          q025,
          q500,
          q975
        )
      )

    ggplot2::ggplot(
      df,
      ggplot2::aes(iteration, value)
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_text(
        data = df_summary,
        mapping = aes(x = -Inf, y = Inf, label = label),
        hjust = 'left',
        vjust = 'top'
      ) +
      ggplot2::facet_wrap(
        ~ name,
        scales = 'free_y',
        ncol = n_columns,
        labeller = if (is_parsed) 'label_parsed' else label_value
      ) +
      ggplot2::labs(x = NULL, y = NULL)
  }

  alpha_subset <- object$alpha[
    ,
    seq(1, ncol(object$alpha), by = ceiling(ncol(object$alpha) / 8)),
    drop = FALSE
  ]
  if (ncol(object$eta) > 0) {
    eta_subset <- object$eta[
      ,
      seq(1, ncol(object$eta), by = ceiling(ncol(object$eta) / 8)),
      drop = FALSE
    ]
  }

  gridExtra::grid.arrange(
    grobs = .remove_nulls(list(
      trace_plot(alpha_subset),
      if (ncol(object$eta) > 0) trace_plot(eta_subset) else NULL,
      if (ncol(object$beta) > 0) trace_plot(object$beta) else NULL,
      trace_plot(object$a, FALSE),
      trace_plot(object$w, FALSE),
      trace_plot(object$gamma, FALSE),
      trace_plot(object$rho, FALSE),
      trace_plot(object$ell, FALSE)
    )),
    left = 'Value',
    bottom = 'Iteration',
    heights = ceiling(c(
      ncol(alpha_subset),
      if (ncol(object$eta) > 0) ncol(eta_subset) else NULL,
      if (ncol(object$beta) > 0) ncol(object$beta) else NULL,
      ncol(object$a),
      ncol(object$w),
      ncol(object$gamma),
      ncol(object$rho),
      ncol(object$ell)
    ) / n_columns),
    ncol = 1
  )
}
