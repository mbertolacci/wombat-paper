.make_w_sampler <- function(
  model,
  Q_alpha = .make_Q_alpha(model)
) {
  n_times <- ncol(model$H) / length(model$regions)
  n_w <- nlevels(model$w_factor)
  w_index <- as.integer(model$w_factor)

  rg <- rgamma
  if ('lower' %in% names(model$w_prior)) {
    rg <- function(...) rtgamma(
      ...,
      lower = model$w_prior[['lower']],
      upper = model$w_prior[['upper']]
    )
  }

  function(current, ...) {
    alpha_p <- as.vector(chol(Q_alpha(list(
      a = current$a, w = rep(1, n_w)
    ))) %*% (
      current$alpha - as.vector(model$Gamma %*% current$kappa)
    ))
    alpha_p_matrix <- matrix(alpha_p, ncol = n_times)

    n <- n_times * table(model$w_factor)
    sum_squares <- rep(0, n_w)
    for (i in seq_len(n_w)) {
      sum_squares[i] <- sum(alpha_p_matrix[w_index == i, ] ^ 2)
    }

    current$w <- rg(
      n_w,
      shape = (
        model$w_prior[['shape']]
        + 0.5 * n
      ),
      rate = (
        model$w_prior[['rate']]
        + 0.5 * sum_squares
      )
    )
    if (!is.null(model[['w']])) {
      has_preset <- !is.na(model$w)
      current$w[has_preset] <- model$w[has_preset]
    }
    current
  }
}
