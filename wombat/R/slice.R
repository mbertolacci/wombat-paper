#' @title Wrapper to automate learning in a slice sampler
#' @description Implements Neal (2003) stepping out univariate slice sampler,
#' with learning -- best for unimodal distributions (does not break Markov
#' property), but also possible to use in spin-up.  Uses enclosure to preserve
#' values from one iteration to the next.  Supply the log density to the
#' returned function.
#' @param w stepping-out interval. This is just a starting value if returned
#' function is called with \code{learn=T}.
#' @details \code{slice} returns a function which should be called with the
#' current sample (\code{x0}), the log density (\code{logf}) and a learn toggle
#' (\code{learn}). If learn is \code{TRUE} the stepping-out interval \code{w} is
#' adjusted according to a running sum of samples so far.
#' @author Jonathan Rougier
#' @references Neal, Radford M. "Slice sampling." Annals of statistics (2003):
#' 705-741.
#' @examples
#' ## Not run:
#' # sample from a mixture of normals with spinup
#'
#' myfun <- function(x)
#'      log(0.8 * dnorm(x, 0, 1) + 0.2 * dnorm(x, 3, 0.25))
#' myslice <- slice(w = 0.1) # obviously too small
#'
#' N <- 1e4
#' rsam <- rep(NA, N)
#' x <- 0.0
#' spinup <- 20
#'
#' for (i in (-spinup):N) {
#'   x <- myslice(x, myfun, learn = i <= 0)
#'   if (i > 0) rsam[i] <- x
#'   if (abs(i) <= 5)
#'    cat(sprintf(
#'      'i = %i, w = %.3f\n',
#'      i,
#'      get("w", envir = environment(myslice))
#'    ))
#' }
#' hist(rsam, col = "grey", freq = FALSE, breaks = 40)
#' x <- sort(rsam)
#' lines(x, exp(myfun(x)), xpd = NA)
#'
#' ## End(Not run)
slice <- function(w, min_w = 0, max_evaluations = 100) {
  learning <- TRUE
  sum <- 0
  n <- 0

  function(x0, logf, learn = FALSE, include_n_evaluations = FALSE) {
    # sort out the learning.  Only learn when learning == TRUE
    n_evaluations <- 1
    logf_count <- function(x) {
      output <- logf(x)
      n_evaluations <<- n_evaluations + 1
      stopifnot(n_evaluations <= max_evaluations)
      output
    }

    if (learn) {
      if (!learning) learning <<- TRUE
    } else {
      if (learning) {
        if (n > 0) w <<- max(min_w, sum / n)
        learning <<- FALSE
      }
    }

    # do the sampling, same notation as Neal (2003)

    y <- log(runif(1)) + logf_count(x0)
    if (y == -Inf) stop('x0 value outside the support')
    U <- runif(1)
    L <- x0 - w * U
    R <- L + w

    while (y < logf_count(L)) L <- L - w
    while (y < logf_count(R)) R <- R + w

    # sample uniformly and adjust the interval

    repeat {
      x1 <- runif(1, L, R)
      if (y < logf_count(x1)) break
      if (x1 < x0) L <- x1 else R <- x1
    }

    # implement the learning and return new value

    if (learning) {
      sum <<- sum + abs(x1 - x0) # as suggested, sec 4.4
      n <<- n + 1
    }

    if (include_n_evaluations) {
      list(n_evaluations = n_evaluations, w = w, sample = x1)
    } else {
      x1
    }
  }
}
