source(Sys.getenv('INVERSION_BASE_PARTIAL'))
source(Sys.getenv('INVERSION_OSSE_PARTIAL'))
library(Matrix)

set.seed(20201026)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--case', '') %>%
  add_argument('--measurement-model', '') %>%
  add_argument('--process-model', '') %>%
  add_argument('--transport-matrix-fullhexp', '') %>%
  add_argument('--osse-anomaly', '') %>%
  add_argument('--output', '') %>%
  parse_args()

case_parts <- as.vector(stringr::str_split(
  args$case,
  '-',
  simplify = TRUE
))

run <- Find(function(run_i) {
  run_i$name == case_parts[1]
}, OSSE_CASES)

log_info('Loading measurement model')
measurement_model <- readRDS(args$measurement_model)

log_info('Loading process model')
process_model <- readRDS(args$process_model)

log_info('Loading OSSE anomaly')
osse_anomaly <- fst::read_fst(args$osse_anomaly)

log_info('Constructing case')
run_osse_anomaly <- osse_anomaly %>%
  filter(name == run$name)

run_measurement_model <- measurement_model %>%
  filter(overall_observation_mode %in% case_parts)

n_groups <- nlevels(run_measurement_model$attenuation_factor)
generate_measurement_model <- run_measurement_model
if ('TRUERHO0' %in% case_parts) {
  generate_measurement_model[['rho']] <- rep(0, n_groups)
} else {
  generate_measurement_model[['rho']] <- rep(0.8, n_groups)
}
generate_measurement_model[['ell']] <- rep(1, n_groups)
generate_measurement_model[['gamma']] <- rep(0.8, n_groups)
if ('IS' %in% case_parts) {
  generate_measurement_model$ell_prior$unit <- 'days'
}

generate_measurement_model[['beta']] <- rep(0, ncol(generate_measurement_model$A))

if ('TRUEBIAS' %in% case_parts) {
  generate_measurement_model[['beta']] <- c(
    'is_oco2:oco2_operation_modeLG' = -1.8,
    'is_oco2:oco2_operation_modeLN' = -0,
    'is_oco2:oco2_operation_modeOG' = -0.4,
    'is_oco2:oco2_operation_modeLG:oco2_dp' = -0.3,
    'is_oco2:oco2_operation_modeLN:oco2_dp' = -0.3,
    'is_oco2:oco2_operation_modeOG:oco2_dp' = -0.08,
    'is_oco2:oco2_operation_modeLG:oco2_co2_grad_del' = -0.028,
    'is_oco2:oco2_operation_modeLN:oco2_co2_grad_del' = -0.028,
    'is_oco2:oco2_operation_modeOG:oco2_co2_grad_del' = 0.077,
    'is_oco2:oco2_operation_modeLG:oco2_log_dws' = -0.6,
    'is_oco2:oco2_operation_modeLN:oco2_log_dws' = -0.6
  )[colnames(generate_measurement_model$A)]
}

generate_process_model <- process_model
generate_process_model$alpha <- rep(0, ncol(process_model$H))
generate_process_model$eta <- rep(0, ncol(process_model$Psi))
generate_measurement_model <- generate(
  generate_measurement_model,
  process_model = generate_process_model
)

run_measurement_model$observations$co2 <- (
  generate_measurement_model$observations$co2
  + as.vector(run_measurement_model$C %*% run_osse_anomaly$co2_anomaly)
)

if ('FREEKAPPA' %in% case_parts) {
  process_model[['Gamma']] <- sparseMatrix(
    i = seq_len(ncol(process_model$H)),
    j = rep(1 : 22, 31)
  )
  process_model[['kappa']] <- NULL
  process_model[['kappa_prior_mean']] <- rep(0, 22)
  process_model[['kappa_prior_variance']] <- rep(0.25 ^ 2, 22)
}

if ('FREEKAPPA2' %in% case_parts) {
  process_model[['Gamma']] <- sparseMatrix(
    i = seq_len(ncol(process_model$H)),
    j = rep(1 : 22, 31)
  )
  process_model[['kappa']] <- NULL
  process_model[['kappa_prior_mean']] <- rep(0, 22)
  process_model[['kappa_prior_variance']] <- rep(1, 22)
}

if ('FREEKAPPA2L' %in% case_parts) {
  i <- seq_len(ncol(process_model$H))
  j <- rep(1 : 22, 31)
  process_model[['Gamma']] <- sparseMatrix(
    i = i[j <= 11],
    j = j[j <= 11],
    dims = c(ncol(process_model$H), 11)
  )
  process_model[['kappa']] <- NULL
  process_model[['kappa_prior_mean']] <- rep(0, 11)
  process_model[['kappa_prior_variance']] <- rep(1, 11)
}

if ('FIXEDWO4' %in% case_parts) {
  process_model[['w']] <- c(rep(NA, 11), rep(9, 11))
}

if ('FIXEDWO5' %in% case_parts) {
  process_model[['w']] <- c(rep(NA, 11), rep(4, 11))
}

if ('FIXEDWO6' %in% case_parts) {
  process_model[['w']] <- c(rep(NA, 11), rep(1, 11))
}

if ('FIXEDAO' %in% case_parts) {
  process_model[['a']] <- c(rep(NA, 11), rep(0, 11))
}

if ('FIXEDHYPER' %in% case_parts) {
  run_measurement_model[['gamma']] <- 0.8
  process_model[['a']] <- rep(0, 22)
  process_model[['w']] <- rep(1 / 0.09, 22)
}

time_since <- with(process_model$control_mole_fraction, {
  as.double(time - lubridate::ymd_hms('2015-01-01 00:00:00'), units = 'days')
})
time_trend <- time_since / max(time_since)

if ('BGTRN' %in% case_parts) {
  process_model$Psi <- cbind(
    process_model$Psi[, 1, drop = FALSE],
    time_trend
  )
}

zonal_basis <- FRK::local_basis(
  FRK::real_line(),
  loc = matrix(seq(-90, 90, length = 5)),
  scale = rep(50, 5)
)

if ('BGLATTRN' %in% case_parts) {
  Psi_zonal <- FRK::eval_basis(zonal_basis, as.matrix(process_model$control_mole_fraction$latitude))
  process_model$Psi <- cbind(
    1,
    time_trend * Psi_zonal
  )
}

if ('BGLATHAR' %in% case_parts) {
  Psi_zonal <- FRK::eval_basis(zonal_basis, as.matrix(process_model$control_mole_fraction$latitude))
  process_model$Psi <- cbind(
    1,
    time_trend,
    Psi_zonal,
    time_trend * Psi_zonal,
    cospi(2 * time_since / 365.25) * Psi_zonal,
    sinpi(2 * time_since / 365.25) * Psi_zonal
  )
}

if ('BGLATHAR' %in% case_parts) {
  Psi_zonal <- FRK::eval_basis(zonal_basis, as.matrix(process_model$control_mole_fraction$latitude))
  process_model$Psi <- cbind(
    1,
    time_trend,
    Psi_zonal,
    time_trend * Psi_zonal,
    cospi(2 * time_since / 365.25) * Psi_zonal,
    sinpi(2 * time_since / 365.25) * Psi_zonal
  )
}

if ('BGINT' %in% case_parts) {
  process_model$Psi <- process_model$Psi[, 1, drop = FALSE]
}

if ('NOBG' %in% case_parts) {
  process_model$Psi <- matrix(0, nrow = nrow(process_model$Psi), ncol = 0)
}

if ('BGMON' %in% case_parts) {
  month_start <- as.factor(lubridate::floor_date(
    process_model$control_mole_fraction$time,
    'month'
  ))
  process_model$Psi <- sparse.model.matrix(~ 1 + month_start)
}

process_model$eta_prior_mean <- rep(0, ncol(process_model$Psi))
process_model$eta_prior_precision <- Diagonal(x = 1 / 25, n = ncol(process_model$Psi))

if ('FIXEDCORR' %in% case_parts) {
  run_measurement_model[['rho']] <- rep(0.8, n_groups)
  run_measurement_model[['ell']] <- rep(1, n_groups)
}

if ('RHO0' %in% case_parts) {
  run_measurement_model[['rho']] <- rep(0, n_groups)
  run_measurement_model[['ell']] <- rep(1, n_groups)
}

if ('NOBIAS' %in% case_parts) {
  run_measurement_model$A <- matrix(0, nrow = nrow(run_measurement_model$A), ncol = 0)
  run_measurement_model$beta_prior_mean <- rep(0, 0)
  run_measurement_model$beta_prior_precision <- Matrix::Diagonal(n = 0)
}

if ('TIGHTW' %in% case_parts) {
  process_model$w_prior <- list(shape = 15.5, rate = 15.5 / 4)
}

# HACK(mgnb): later code is configured to load these if missing
process_model$sensitivities <- NULL
process_model$H <- NULL

log_info('Saving')
saveRDS_gz1(list(
  true_parameters = list(
    alpha = as.vector(t(run$alpha))
  ),
  process_model = process_model,
  measurement_model = run_measurement_model
), args$output)

log_info('Done')
