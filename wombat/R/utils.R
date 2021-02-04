#' @export
to_month_start <- function(x) {
  lubridate::floor_date(x, 'months')
}

#' @export
gamma_quantile_prior <- function(
  q_lower, q_upper,
  p_lower = 0.05, p_upper = 0.95,
  interval = c(0.01, 10)
) {
  stopifnot(q_lower < q_upper)
  stopifnot(p_lower < p_upper)

  # Find the shape parameter that gives the appropriate ratio between the
  # quantiles
  ratio <- q_lower / q_upper
  shape <- uniroot(function(shape) {
    theoretical <- qgamma(c(p_lower, p_upper), shape = shape, rate = 1)
    theoretical[1] / theoretical[2] - ratio
  }, interval, tol = sqrt(.Machine$double.eps))$root

  # Find the rate parameter that gives the correct quantiles
  rate <- qgamma(p_upper, shape = shape, rate = 1) / q_upper

  list(shape = shape, rate = rate)
}

#' @export
generate <- function(x, ...) UseMethod('generate', x)

#' @export
calculate <- function(x, ...) {
  UseMethod('calculate', x)
}

#' @export
log_prior <- function(x, ...) UseMethod('log_prior', x)

#' @export
n_terms <- function(f) {
  length(attr(terms(f), 'term.labels'))
}

.rnorm_truncated <- function(
  n, mean = 0, sd = 1, lower = -Inf, upper = Inf, max_iterations = 1000
) {
  sapply(1 : n, function(i) {
    mean_i <- mean[1 + (i - 1) %% length(mean)]
    sd_i <- sd[1 + (i - 1) %% length(sd)]
    for (j in 1 : max_iterations) {
      output <- rnorm(1, mean_i, sd_i)
      if (output > lower && output < upper) {
        return(output)
      }
    }
    stop('max iterations exceeded')
  })
}

.chol_solve <- function(R, b) {
  if (is(R, 'triangularMatrix')) {
    solve(R, solve(t(R), b))
  } else if (is(R, 'CHMfactor')) {
    solve(R, b, system = 'A')
  } else {
    backsolve(R, backsolve(R, b, transpose = TRUE))
  }
}

# Given a series of lists as arguments, shallowly merge the lists from left to
# right (so that the right-most values override the left-most)
.extend_list <- function(...) {
  lists <- list(...)
  output <- lists[[1]]
  for (value in lists[2 : length(lists)]) {
    for (name in names(value)) {
      output[[name]] <- value[[name]]
    }
  }
  return(output)
}

.remove_nulls <- function(x) {
  Filter(Negate(is.null), x)
}

.remove_nulls_and_missing <- function(x) {
  Filter(Negate(is.symbol), Filter(Negate(is.null), x))
}

.strip_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

.recycle_vector_to <- function(x, to_length) {
  x[
    ((seq_len(to_length) - 1) %% length(x)) + 1
  ]
}

.cache_memory_fifo <- function(algo = 'sha512', size = 10) {
  cache <- NULL
  keys <- NULL

  cache_reset <- function() {
    cache <<- new.env(TRUE, emptyenv())
    keys <<- NULL
  }

  cache_set <- function(key, value) {
    assign(key, value, envir = cache)
    keys <<- c(keys, key)
    if (length(keys) > size) {
      cache_drop_key(keys[[1]])
    }
  }

  cache_get <- function(key) {
    get(key, envir = cache, inherits = FALSE)
  }

  cache_has_key <- function(key) {
    exists(key, envir = cache, inherits = FALSE)
  }

  cache_drop_key <- function(key) {
    rm(list = key, envir = cache, inherits = FALSE)
    keys <<- keys[keys != key]
  }

  cache_reset()
  list(
    digest = function(...) digest::digest(..., algo = algo),
    reset = cache_reset,
    set = cache_set,
    get = cache_get,
    has_key = cache_has_key,
    drop_key = cache_drop_key,
    keys = function() ls(cache)
  )
}

#' @export
scale_fill_wes_palette_c <- function(
  palette = 'Zissou1',
  reverse = FALSE,
  n = 100,
  ...
) {
  colours <- wesanderson::wes_palette(name = palette, n, type = 'continuous')
  if (reverse) colours <- rev(colours)
  scale_fill_gradientn(colours = colours, ...)
}

#' @export
scale_colour_wes_palette_c <- function(
  palette = 'Zissou1',
  reverse = FALSE,
  n = 100,
  ...
) {
  colours <- wesanderson::wes_palette(name = palette, n, type = 'continuous')
  if (reverse) colours <- rev(colours)
  scale_colour_gradientn(colours = colours, ...)
}

make_scale_x_gradient2_oob <- function(x) {
  function(
    ...,
    limits,
    midpoint = 0,
    low = scales::muted('red'),
    mid = 'white',
    high = scales::muted('blue'),
    low_oob = colorspace::darken(low, 0.5),
    high_oob = colorspace::darken(high, 0.5)
  ) {
    midpoint01 <- scales::rescale(
      midpoint, from = limits, to = c(0.1, 0.9)
    )
    x(
      colours = c(low_oob, low_oob, low, mid, high, high_oob, high_oob),
      values = c(0, 0.099, 0.1, midpoint01, 0.9, 0.901, 1),
      breaks = scales::rescale(
        c(0, 0.1, midpoint01, 0.9, 1),
        from = c(0.1, 0.9),
        to = limits
      ),
      labels = c(
        parse(text = sprintf('\'\'<=%f', limits[1])),
        limits[1],
        midpoint,
        limits[2],
        parse(text = sprintf('\'\'>=%f', limits[2]))
      ),
      oob = scales::squish,
      limits = scales::rescale(
        c(0, 1),
        from = c(0.1, 0.9),
        to = limits
      )
    )
  }
}

#' @export
scale_colour_gradient2_oob <- make_scale_x_gradient2_oob(scale_colour_gradientn)

#' @export
scale_fill_gradient2_oob <- make_scale_x_gradient2_oob(scale_fill_gradientn)
