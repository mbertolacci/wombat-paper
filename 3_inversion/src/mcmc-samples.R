source(Sys.getenv('INVERSION_BASE_PARTIAL'))

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--model-case', '') %>%
  add_argument('--process-model', '') %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading model')
model_case <- readRDS(args$model_case)

if (is.null(model_case$process_model$H)) {
  log_info('Loading process model')
  process_model <- readRDS(args$process_model)
  model_case$process_model$H <- process_model$H
  rm(process_model)
  gc(verbose = FALSE)
}

log_info('Running MCMC')
output <- inversion_mcmc(
  11000,
  model_case$measurement_model,
  model_case$process_model,
  show_progress = TRUE,
  use_tensorflow = Sys.getenv('WOMBAT_TENSORFLOW') == '1' && is.null(model_case$measurement_model[['rho']])
)

log_info('Saving')
saveRDS(output, args$output)

log_info('Done')
