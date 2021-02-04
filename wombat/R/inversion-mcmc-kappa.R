.make_kappa_sampler <- function(
  process_model,
  Q_alpha = .make_Q_alpha(process_model)
) {
  if (!is.null(process_model[['kappa']])) {
    return(function(current) {
      current$kappa <- process_model[['kappa']]
      current
    })
  }

  kappa_prior_precision <- Diagonal(
    x = 1 / process_model$kappa_prior_variance
  )

  function(current) {
    Q_alpha_i <- Q_alpha(current)

    chol_kappa_precision <- chol(
      kappa_prior_precision
      + crossprod(process_model$Gamma, Q_alpha_i %*% process_model$Gamma)
    )

    kappa_mean <- as.vector(.chol_solve(
      chol_kappa_precision,
      crossprod(process_model$Gamma, Q_alpha_i %*% current$alpha)
      + kappa_prior_precision %*% process_model$kappa_prior_mean
    ))

    current$kappa <- kappa_mean + .sample_normal_precision_chol(
      chol_kappa_precision
    )
    current
  }
}
