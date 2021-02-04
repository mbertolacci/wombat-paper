# Conversion of diagonal matrix to dsC for fast add
.ddi_to_dsC <- function(x, symmetric) {
  # Transpose ensures upper triangular
  t(sparseMatrix(
    i = seq_len(ncol(x)),
    j = seq_len(ncol(x)),
    x = x@x,
    symmetric = TRUE
  ))
}

#' Faster kronecker product for sparse matrices
#'
#' A much faster sparse kronecker product than that in the Matrix package, with
#' methods for symmetric and general matrices. Currently this method doesn't
#' work for all combinations of inputs, so check the parameter type
#' documentation.
#' @param a Left operand, can be dgCMatrix or dsCMatrix
#' @param b Right operand, must have same type as left operand
#' @export
setGeneric('fast_kronecker', function(a, b) {
  standardGeneric('fast_kronecker')
})

#' @describeIn fast_kronecker General and general.
#' @export
setMethod('fast_kronecker',
  c(a = 'dgCMatrix', b = 'dgCMatrix'),
  function(a, b) {
    .fast_kronecker(a, b)
  }
)

#' @describeIn fast_kronecker Symmetric and symmetric.
#' @export
setMethod('fast_kronecker',
  c(a = 'dsCMatrix', b = 'dsCMatrix'),
  function(a, b) {
    .fast_kronecker_sym(a, b)
  }
)

#' @describeIn fast_kronecker Symmetric and diagonal
#' @export
setMethod('fast_kronecker',
  c(a = 'dsCMatrix', b = 'ddiMatrix'),
  function(a, b) {
    .fast_kronecker_sym(a, .ddi_to_dsC(b))
  }
)
