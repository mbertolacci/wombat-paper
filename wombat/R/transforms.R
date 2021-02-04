.logit_transform <- function(x, include_gradient = FALSE) {
  log_x <- log(x)
  log1m_x <- log1p(-x)
  output <- log_x - log1m_x
  if (include_gradient) {
    attr(output, 'log_gradient') <- -log_x - log1m_x
  }
  output
}

.expit_transform <- function(x, include_gradient = FALSE) {
  output <- 1 / (1 + exp(-x))
  if (include_gradient) {
    attr(output, 'log_gradient') <- log(output * (1 - output))
  }
  output
}

.log_transform <- function(x, include_gradient = FALSE) {
  output <- log(x)
  if (include_gradient) {
    attr(output, 'log_gradient') <- -output
  }
  output
}

.exp_transform <- function(x, include_gradient = FALSE) {
  output <- exp(x)
  if (include_gradient) {
    attr(output, 'log_gradient') <- x
  }
  output
}

.softplus_transform <- function(x, include_gradient = FALSE) {
  if (include_gradient) stop('Not supported')
  ifelse(
    x > 30,
    x,
    log(1 + exp(x))
  )
}

.inv_softplus_transform <- function(y, include_gradient = FALSE) {
  if (include_gradient) stop('Not supported')
  ifelse(
    y > 30,
    y,
    log(exp(y) - 1)
  )
}
