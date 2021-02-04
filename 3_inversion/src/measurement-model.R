source(Sys.getenv('INVERSION_BASE_PARTIAL'))
library(Matrix)

model_matrix2 <- function(x, y, ...) model.matrix.lm(y, x, ...)
na_to_zero <- function(x) {
  x[1 : length(x)] <- ifelse(
    is.na(x[1 : length(x)]),
    0,
    x[1 : length(x)]
  )
  x
}

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--observations', '') %>%
  add_argument('--process-model', '') %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading observations')
observations <- fst::read_fst(args$observations) %>%
  filter(
    observation_type != 'tccon',
    !is.na(co2_error)
  ) %>%
  arrange(observation_group, time) %>%
  distinct(observation_group, time, .keep_all = TRUE)

log_info('Loading process model')
process_model <- readRDS(args$process_model)

log_info('Constructing bias matrix')
A <- observations %>%
  mutate(is_oco2 = as.integer(startsWith(observation_type, 'oco2'))) %>%
  model_matrix2(
    ~ is_oco2:oco2_operation_mode
    + is_oco2:oco2_operation_mode:oco2_dp
    + is_oco2:oco2_operation_mode:oco2_co2_grad_del
    + is_oco2:oco2_operation_mode:oco2_log_dws
    - 1,
    na.action = 'na.pass'
  ) %>%
  na_to_zero()

# Omit is_oco2:oco2_operation_modeOG:oco2_log_dws
A <- A[, -12]

# Scale all non-zero values to have mean zero and standard deviation one, except
# for indicators
A_locations <- NULL
A_scales <- NULL
for (i in seq_len(ncol(A))) {
  A_i <- A[, i]
  non_zero_indices <- which(A_i != 0)
  A_i_non_zero <- A_i[non_zero_indices]
  mean_i <- if (all(A_i_non_zero == 1)) 0 else mean(A_i_non_zero)
  stdev_i <- if (all(A_i_non_zero == 1)) 1 else sd(A_i_non_zero)
  A[non_zero_indices, i] <- (A_i_non_zero - mean_i) / stdev_i
  A_locations <- c(A_locations, mean_i)
  A_scales <- c(A_scales, stdev_i)
}
A <- Matrix(A, sparse = TRUE)
attr(A, 'locations') <- A_locations
attr(A, 'scales') <- A_scales

log_info('Constructing measurement model')
measurement_model <- flux_measurement_model(
  observations,
  A = A,
  measurement_variance = observations$co2_error ^ 2,
  beta_prior_variance = 100,
  matching = match(observations$observation_id, process_model$control_mole_fraction$observation_id),
  process_model = process_model,
  attenuation_variables = 'observation_group',
  ell_prior = list(shape = 1, rate = 1, unit = 'mins')
)

log_info('Saving')
saveRDS(measurement_model, args$output, version = 2)

log_info('Done')
