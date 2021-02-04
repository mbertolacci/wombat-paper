source(Sys.getenv('INVERSION_BASE_PARTIAL'))
library(coda)

options(dplyr.summarise.inform = FALSE)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--model-case', '') %>%
  add_argument('--process-model', '') %>%
  add_argument('--samples', '') %>%
  add_argument('--observations', '') %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading observations')
tccon_observations <- fst::read_fst(args$observations) %>%
  filter(observation_type == 'tccon')

log_info('Loading model case')
model_case <- readRDS(args$model_case)

if (is.null(model_case$process_model$H)) {
  log_info('Loading process model')
  process_model <- readRDS(args$process_model)
  model_case$process_model$H <- process_model$H
  rm(process_model)
  gc(verbose = FALSE)
}

log_info('Loading samples')
samples <- window(readRDS(args$samples), start = 1001)

log_info('Computing TCCON samples')
tccon_matching <- match(
  tccon_observations$observation_id,
  model_case$process_model$control_mole_fraction$observation_id
)

H_tccon <- model_case$process_model$H[tccon_matching, ]
Psi_tccon <- model_case$process_model$Psi[tccon_matching, ]

Y2_prior <- model_case$process_model$control_mole_fraction$co2[tccon_matching]
Y2_tilde_samples <- as.matrix(
  H_tccon %*% t(as.matrix(window(coda::mcmc(samples$alpha), thin = 10)))
  + if (ncol(samples$eta) > 0) Psi_tccon %*% t(as.matrix(window(coda::mcmc(samples$eta), thin = 10))) else 0
)

output <- as_tibble(tccon_observations) %>%
  mutate(
    station_id = as.numeric(observation_id) %% 100,
    station_name = c(
      'parkfalls', 'lamont', 'bialystok', 'orleans', 'karlsruhe', NA,
      'tsukuba125', NA, 'lauder125', 'darwin', 'wollongong', 'garmisch', 'bremen', 'eureka',
      'sodankyla', 'izana', 'reunion', 'ascension', NA, 'caltech', NA, NA, 'nyalesund', NA,
      'dryden', 'saga', 'rikubetsu', 'manaus', 'paris', 'anmyeondo'
    )[station_id],
    station = sprintf('%02d-%s', station_id, station_name),
    Y2_prior = Y2_prior,
    Y2_tilde_samples = Y2_tilde_samples
  )

log_info('Saving')
saveRDS(output, args$output)

log_info('Done')
