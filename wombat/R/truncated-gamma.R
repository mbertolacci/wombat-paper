#' Truncated gamma distribution
#' @export
rtgamma <- function(n, shape, rate, lower = 0, upper = 1, ...) {
  armspp::arms(
    n,
    dgamma,
    lower,
    upper,
    metropolis = FALSE,
    arguments = list(
      shape = shape,
      rate = rate,
      log = TRUE
    ),
    ...
  )
}
