#' @export
setClass(
  'TridiagonalMatrix',
  contains = c('Matrix', 'VIRTUAL')
)

#' @export
setClass(
  'GTridiagonalMatrix',
  contains = c('TridiagonalMatrix', 'symmetricMatrix'),
  slots = list(
    major = 'numeric',
    sub_minor = 'numeric',
    sup_minor = 'numeric',
    factors = 'list'
  )
)

#' @export
setClass(
  'STridiagonalMatrix',
  contains = c('TridiagonalMatrix', 'symmetricMatrix'),
  slots = list(
    major = 'numeric',
    minor = 'numeric',
    factors = 'list'
  )
)

#' @export
TridiagonalMatrix <- function(
  major,
  # sub_minor,
  # sup_minor,
  minor,
  dimnames,
  symmetric
) {
  n <- length(major)

  symmetric <- TRUE
  # if (missing(symmetric)) {
  #   symmetric <- !missing(minor) || missing(sub_minor) || missing(sup_minor)
  # }
  if (symmetric) {
    new(
      'STridiagonalMatrix',
      major = as.numeric(major),
      minor = as.numeric(minor),
      factors = list(),
      Dim = c(n, n),
      Dimnames = if (missing(dimnames)) list(NULL, NULL) else dimnames
    )
  } else {
    new(
      'GTridiagonalMatrix',
      major = as.numeric(major),
      sub_minor = as.numeric(sub_minor),
      sup_minor = as.numeric(sup_minor),
      factors = list(),
      Dim = c(n, n),
      Dimnames = if (missing(dimnames)) list(NULL, NULL) else dimnames
    )
  }
}

#' @export
setMethod(
  'determinant',
  signature(x = 'STridiagonalMatrix', logarithm = 'logical'),
  function(x, logarithm) {
    modulus <- spdt_cholesky_obj(x)$log_determinant
    sign <- 1

    if (!logarithm) modulus <- exp(modulus)
    attr(modulus, 'logarithm') <- logarithm
    structure(list(modulus = modulus, sign = sign), class = 'det')
  }
)

#' @export
setMethod(
  'solve',
  signature(a = 'STridiagonalMatrix', b = 'ANY'),
  function(a, b) {
    solve(as(a, 'CsparseMatrix'), b)
  }
)

#' @export
setMethod(
  'solve',
  signature(a = 'STridiagonalMatrix', b = 'ddenseMatrix'),
  function(a, b) {
    ldl <- spdt_cholesky_obj(a)
    matrix(
      spdt_ldl_solve_vec(ldl$major, ldl$minor, b@x, ncol(b)),
      nrow = nrow(b),
      ncol = ncol(b)
    )
  }
)

#' @export
setMethod(
  'solve',
  signature(a = 'STridiagonalMatrix', b = 'numeric'),
  function(a, b) {
    ldl <- spdt_cholesky_obj(a)
    spdt_ldl_solve_vec(ldl$major, ldl$minor, b, 1)
  }
)

#' @export
setMethod(
  'solve',
  signature(a = 'STridiagonalMatrix', b = 'matrix'),
  function(a, b) {
    ldl <- spdt_cholesky_obj(a)
    spdt_ldl_solve_mat(ldl$major, ldl$minor, b)
  }
)

#' @export
setMethod(
  '%*%',
  c(x = 'STridiagonalMatrix', y = 'ANY'),
  function(x, y) {
    as(x, 'CsparseMatrix') %*% y
  }
)

#' @export
setMethod(
  '%*%',
  c(x = 'STridiagonalMatrix', y = 'ddenseMatrix'),
  function(x, y) {
    matrix(
      spdt_mul_vec(x@major, x@minor, y@x, ncol(y)),
      nrow = nrow(y),
      ncol = ncol(y)
    )
  }
)

#' @export
setMethod(
  '%*%',
  c(x = 'STridiagonalMatrix', y = 'numeric'),
  function(x, y) {
    # x@major * y + c(0, head(y, -1) * x@minor) + c(tail(y, -1) * x@minor, 0)
    spdt_mul_vec(x@major, x@minor, y, 1)
  }
)

#' @export
setMethod(
  '%*%',
  c(x = 'STridiagonalMatrix', y = 'matrix'),
  function(x, y) {
    spdt_mul_mat(x@major, x@minor, y)
  }
)

#' @export
setMethod(
  'crossprod',
  c(x = 'STridiagonalMatrix', y = 'ANY'),
  function(x, y) {
    x %*% y
  }
)

#' @export
setMethod(
  't',
  c(x = 'STridiagonalMatrix'),
  function(x) {
    x
  }
)

#' @export
bdiag_tridiagonal <- function(x) {
  if (length(x) == 1) return(x[[1]])

  TridiagonalMatrix(
    do.call(c, lapply(x, slot, 'major')),
    head(do.call(c, lapply(x, function(x) c(x@minor, 0))), -1)
  )
}

#' @export
setMethod(
  'Cholesky',
  c(A = 'STridiagonalMatrix'),
  function(A, ...) {
    Cholesky(as(A, 'CsparseMatrix'), ...)
  }
)

#' @export
setMethod(
  'chol',
  c(x = 'STridiagonalMatrix'),
  function(x, ...) {
    ldl <- spdt_cholesky_obj(x)
    sqrt_major <- sqrt(ldl$major)

    bandSparse(
      nrow(x),
      k = c(0, 1),
      diagonals = list(
        sqrt_major,
        head(sqrt_major, -1) * ldl$minor
      )
    )

    # chol(as(x, 'CsparseMatrix'), ...)
  }
)

setAs('STridiagonalMatrix', 'sparseMatrix', function(from) {
  as(from, 'CsparseMatrix')
})

setAs('STridiagonalMatrix', 'CsparseMatrix', function(from) {
  bandSparse(
    length(from@major),
    k = c(0, 1),
    diagonals = list(from@major, from@minor),
    symmetric = TRUE
  )
})
