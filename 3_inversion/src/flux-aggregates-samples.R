source(Sys.getenv('INVERSION_BASE_PARTIAL'))

options(dplyr.summarise.inform = FALSE)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--flux-aggregators', '') %>%
  add_argument('--model-case', '') %>%
  add_argument('--process-model', '') %>%
  add_argument('--samples', '') %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading model case')
model_case <- readRDS(args$model_case)
if (is.null(model_case$process_model$H)) {
  model_case$process_model$H <- readRDS(args$process_model)$H
}
model_case$process_model$H <- model_case$process_model$H[1, , drop = FALSE]
gc(verbose = FALSE)

log_info('Loading flux aggregators')
flux_aggregators <- readRDS(args$flux_aggregators)

log_info('Loading samples')
samples <- window(readRDS(args$samples), start = 1001)

log_debug('Taking prior samples')
prior_alpha_samples <- generate(model_case$process_model, nrow(samples$alpha))$alpha

log_debug('Computing emissions summaries')
emissions_summary <- lapply(flux_aggregators, function(emission_group) {
  postprocess_estimate <- function(estimate, alpha) {
    if (length(dim(alpha)) > 1 && ncol(alpha) < ncol(emission_group$aggregator$Phi)) {
      # HACK(mgnb): assume this means the ocean is fixed
      alpha_subset <- alpha
      alpha <- matrix(0, nrow = nrow(alpha), ncol = ncol(emission_group$aggregator$Phi))
      regions <- rep(1 : 22, 31)
      alpha[, regions <= 11] <- alpha_subset
    }
    aggregate_flux(
      aggregator = emission_group$aggregator,
      parameters = list(alpha = alpha)
    ) %>%
      mutate(
        name = emission_group$name,
        estimate = estimate,
        flux_mean = if (length(dim(flux)) > 1) rowMeans(flux) else flux,
        flux_samples = if (length(dim(flux)) > 1) flux else as.double(NA),
      ) %>%
      group_by(
        name,
        estimate,
        month_start
      ) %>%
      summarise(
        flux_mean = sum(flux_mean),
        flux_samples = if (any(is.na(flux_samples))) NA else t(colSums(flux_samples))
      ) %>%
      select(
        name,
        estimate,
        month_start,
        flux_mean,
        flux_samples
      )
  }

  log_trace('Processing {emission_group$name}')
  bind_rows(
    if ('true_parameters' %in% names(model_case)) {
      postprocess_estimate(
        'Truth',
        model_case$true_parameters$alpha
      )
    } else NULL,
    postprocess_estimate(
      'Prior',
      prior_alpha_samples
    ) %>%
      select(-flux_mean) %>%
      left_join(
        postprocess_estimate(
          'Prior',
          as.vector(model_case$process_model$Gamma %*% model_case$process_model$kappa)
        ) %>%
          select(name, estimate, month_start, flux_mean),
        by = c('name', 'estimate', 'month_start')
      ) %>%
      select(
        name,
        estimate,
        month_start,
        flux_mean,
        flux_samples
      ),
    postprocess_estimate(
      'Posterior',
      as(samples$alpha, 'matrix')
    )
  )
}) %>%
  bind_rows()

log_info('Saving')
saveRDS(emissions_summary, args$output)

log_info('Done')
