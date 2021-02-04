.ar1_Q <- function(n_times, rho) {
  if (n_times == 1) {
    return(t(sparseMatrix(i = 1, j = 1, x = 1, symmetric = TRUE)))
  }

  stopifnot(rho >= -1 && rho <= 1)

  # Transpose ensures this is upper-triangular, the convention for this package
  t(sparseMatrix(
    i = c(
      # (1, 1) and (n_times, n_times)
      1, n_times,
      # Rest of the diagonal
      if (n_times > 2) (2 : (n_times - 1)) else NULL,
      # One off-diagonal (the other comes in via symmetry)
      2 : n_times
    ),
    j = c(
      1, n_times,
      if (n_times > 2) (2 : (n_times - 1)) else NULL,
      1 : (n_times - 1)
    ),
    x = c(
      1, 1,
      rep(1 + rho ^ 2, n_times - 2),
      rep(-rho, n_times - 1)
    ) / (1 - rho ^ 2),
    symmetric = TRUE
  ))
}
