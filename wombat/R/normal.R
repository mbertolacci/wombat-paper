.log_pdf_precision_cholesky <- function(Y, chol_Q) {
  sum(log(diag(chol_Q))) - 0.5 * sum((chol_Q %*% Y) ^ 2)
}

.sample_normal_precision_chol <- function(chol_Q) {
  if (is(chol_Q, 'triangularMatrix')) {
    as.vector(solve(chol_Q, rnorm(ncol(chol_Q))))
  } else if (is(chol_Q, 'CHMfactor')) {
    as.vector(solve(chol_Q, solve(
      chol_Q,
      rnorm(ncol(chol_Q)),
      system = 'Lt'
    ), system = 'Pt'))
  } else {
    as.vector(backsolve(chol_Q, rnorm(ncol(chol_Q))))
  }
}

.sample_normal_precision <- function(Q) {
  .sample_normal_precision_chol(Cholesky(Q, LDL = FALSE))
}

.sample_normal_covariance <- function(S) {
  if (is(S, 'WoodburyMatrix')) {
    rwnorm(1, covariance = S)
  } else {
    as.vector(crossprod(chol(S), rnorm(ncol(S))))
  }
}


.sample_Q <- function(n, Q) {
  z <- matrix(stats::rnorm(n * nrow(Q)), nrow = nrow(Q))

  if (is(Q, 'denseMatrix')) {
    solve(chol(Q), z)
  } else {
    chol_Q <- Cholesky(Q, LDL = FALSE)
    solve(chol_Q, solve(
      chol_Q,
      z,
      system = 'Lt'
    ), system = 'Pt')
  }
}


.dmvnorm <- function(x, mean, covariance, precision, log = FALSE) {
  if (missing(mean)) mean <- 0

  z <- x - mean
  output <- if (!missing(covariance)) {
    as.numeric(
      - 0.5 * (length(x) * log(2 * pi))
      - 0.5 * determinant(covariance, logarithm = TRUE)$modulus
      - 0.5 * crossprod(z, solve(covariance, z))
    )
  } else {
    as.numeric(
      - 0.5 * (length(x) * log(2 * pi))
      + 0.5 * determinant(precision, logarithm = TRUE)$modulus
      - 0.5 * crossprod(z, precision %*% z)
    )
  }

  if (log) output else exp(output)
}

.rmvnorm <- function(mean, covariance, precision) {
  if (missing(mean)) mean <- 0

  if (!missing(covariance)) {
    if (is(covariance, 'WoodburyMatrix')) {
      (
        mean
        + .rmvnorm(precision = covariance@A)
        + covariance@X %*% .rmvnorm(precision = covariance@B)
      )
    } else {
      mean + as.vector(crossprod(chol(covariance), rnorm(ncol(covariance))))
    }
  } else if (!missing(precision)) {
    if (is(precision, 'STridiagonalMatrix')) {
      precision <- as(precision, 'sparseMatrix')
    }
    mean + .sample_Q(1, precision)
  } else {
    stop('One of precision or covariance must be provided')
  }
}
