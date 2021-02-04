.make_a_sampler <- function(
  model,
  tuning,
  alpha_log_likelihood = .make_alpha_log_likelihood(model)
) {
  n_times <- ncol(model$H) / length(model$regions)
  n_params <- nlevels(model$a_factor)

  part_slice <- lapply(seq_len(n_params), function(i) {
    do.call(slice, tuning)
  })

  function(current, warming_up) {
    for (i in seq_len(n_params)) {
      if (!is.null(model[['a']]) && !is.na(model$a[i])) next

      output <- part_slice[[i]](
        current$a[i],
        function(param_i) {
          current2 <- current
          current2$a[i] <- param_i

          log_prior_value <- log_prior(model, current2)
          if (!is.finite(log_prior_value)) return(log_prior_value)

          log_prior_value + alpha_log_likelihood(current2)
        },
        learn = warming_up,
        include_n_evaluations = TRUE
      )
      log_trace(paste0(
        'a[{i}] = {round(output$sample, 3)} took {output$n_evaluations}',
        ' evaluations, w = {output$w}'
      ))
      current$a[i] <- output$sample
    }

    current
  }
}
