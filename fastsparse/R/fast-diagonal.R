#' @export
setClass(
  'FastDiagonal',
  contains = 'ddiMatrix'
)

#' @export
FastDiagonal <- function(
  x,
  dimnames
) {
  n <- length(x)
  new(
    'FastDiagonal',
    x = x,
    Dim = c(n, n),
    Dimnames = if (missing(dimnames)) list(NULL, NULL) else dimnames
  )
}

#' @export
setMethod(
  '%*%',
  signature(x = 'FastDiagonal', y = 'numeric'),
  function(x, y) {
    x@x * y
  }
)

#' @export
setMethod(
  '%*%',
  signature(x = 'FastDiagonal', y = 'matrix'),
  function(x, y) {
    x@x * y
  }
)

#' @export
setMethod(
  'solve',
  signature(a = 'FastDiagonal', b = 'numeric'),
  function(a, b) {
    b / a@x
  }
)

#' @export
setMethod(
  'solve',
  signature(a = 'FastDiagonal', b = 'matrix'),
  function(a, b) {
    b / a@x
  }
)
