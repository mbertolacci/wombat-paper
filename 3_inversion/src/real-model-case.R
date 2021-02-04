source(Sys.getenv('INVERSION_BASE_PARTIAL'))
library(Matrix)
library(WoodburyMatrix)

set.seed(20200706)

as_case <- function(process_model, measurement_model) {
  list(
    process_model = process_model,
    measurement_model = measurement_model
  )
}

ar1_Q <- function(n_times, rho) {
  if (n_times == 1) {
    return(t(sparseMatrix(i = 1, j = 1, x = 1, symmetric = TRUE)))
  }

  stopifnot(rho >= -1 && rho <= 1)

  # Transpose ensures this is upper-triangular, the convention for this package
  t(sparseMatrix(
    i = c(
      # (1, 1) and (n_times, n_times)
      1, n_times,
      # Rest of the diagonal
      if (n_times > 2) (2 : (n_times - 1)) else NULL,
      # One off-diagonal (the other comes in via symmetry)
      2 : n_times
    ),
    j = c(
      1, n_times,
      if (n_times > 2) (2 : (n_times - 1)) else NULL,
      1 : (n_times - 1)
    ),
    x = c(
      1, 1,
      rep(1 + rho ^ 2, n_times - 2),
      rep(-rho, n_times - 1)
    ) / (1 - rho ^ 2),
    symmetric = TRUE
  ))
}

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--case', '') %>%
  add_argument('--measurement-model', '') %>%
  add_argument('--process-model', '') %>%
  add_argument('--transport-matrix-fullhexp', '') %>%
  add_argument('--true-model', '') %>%
  add_argument('--output', '') %>%
  parse_args()

log_info_case <- function(str, ...) {
  log_info(paste0(
    sprintf('[%s] ', args$case),
    str
  ), ...)
}

log_info_case('Loading measurement model')
measurement_model <- readRDS(args$measurement_model)

log_info_case('Loading process model')
process_model <- readRDS(args$process_model)

log_info_case('Constructing case')
case_parts <- as.vector(stringr::str_split(
  args$case,
  '-',
  simplify = TRUE
))

measurement_model <- measurement_model %>%
  filter(overall_observation_mode %in% case_parts)


n_groups <- nlevels(measurement_model$attenuation_factor)

if ('CLAMP2' %in% case_parts) {
  measurement_model$measurement_variance <- pmax(
    measurement_model$measurement_variance,
    2 ^ 2
  )
}

if ('RHO0' %in% case_parts) {
  measurement_model[['rho']] <- rep(0, n_groups)
  measurement_model[['ell']] <- rep(1, n_groups)
}

if ('FIXEDCORR' %in% case_parts) {
  measurement_model[['rho']] <- c(
    'LG' = 0.820,
    'LGr' = 0.820,
    'LN' = 0.883,
    'LNr' = 0.883
  )[as.character(levels(measurement_model$attenuation_factor))]
  measurement_model[['ell']] <- c(
    'LG' = 1.110,
    'LGr' = 1.110,
    'LN' = 1.000,
    'LNr' = 1.000
  )[as.character(levels(measurement_model$attenuation_factor))]
}

if ('FIXEDGAMMA1' %in% case_parts) {
  measurement_model[['gamma']] <- rep(1, n_groups)
}

if ('FIXEDGAMMA2' %in% case_parts) {
  measurement_model[['gamma']] <- rep(0.25, n_groups)
}

if ('NIW' %in% case_parts) {
  process_model$w_prior <- list(shape = -0.5, rate = 0)
}

if ('POOLA' %in% case_parts) {
  process_model$a_factor <- factor(c(rep('Land', 11), rep('Ocean', 11)))
}

if ('ONEA' %in% case_parts) {
  process_model$a_factor <- factor(rep('a', 22))
}

if ('POOLW' %in% case_parts) {
  process_model$w_factor <- factor(c(rep('Land', 11), rep('Ocean', 11)))
}

if ('TIGHTLESSW' %in% case_parts) {
  if ('POOLW' %in% case_parts) {
    process_model$w_prior <- list(shape = 0.5 * 11 * 15.5, rate = 0.5 * 11 * 15.5 / 4)
  } else {
    process_model$w_prior <- list(shape = 0.5 * 15.5, rate = 0.5 * 15.5 / 4)
  }
}

if ('TIGHTW' %in% case_parts) {
  if ('POOLW' %in% case_parts) {
    process_model$w_prior <- list(shape = 11 * 15.5, rate = 11 * 15.5 / 4)
  } else {
    process_model$w_prior <- list(shape = 15.5, rate = 15.5 / 4)
  }
}

if ('TIGHTERW' %in% case_parts) {
  if ('POOLW' %in% case_parts) {
    process_model$w_prior <- list(shape = 2 * 11 * 15.5, rate = 2 * 11 * 15.5 / 4)
  } else {
    process_model$w_prior <- list(shape = 2 * 15.5, rate = 2 * 15.5 / 4)
  }
}

if ('TIGHTWO' %in% case_parts) {
  if ('POOLW' %in% case_parts) {
    process_model$w_prior <- list(
      shape = c(0.3542832, 11 * 15.5),
      rate = c(0.01534294, 11 * 15.5 / 16)
    )
  } else {
    process_model$w_prior <- list(
      shape = c(rep(0.3542832, 11), rep(15.5, 11)),
      rate = c(rep(0.01534294, 11), rep(15.5 / 16, 11))
    )
  }
}

if ('TIGHTAO' %in% case_parts) {
  if ('POOLA' %in% case_parts) {
    process_model$a_prior <- list(
      shape1 = c(1, 1),
      shape2 = c(1, 11 * 20)
    )
  } else {
    process_model$a_prior <- list(
      shape1 = c(rep(1, 11), rep(1, 11)),
      shape2 = c(rep(1, 11), rep(20, 11))
    )
  }
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

if ('FIXEDA' %in% case_parts) {
  process_model[['a']] <- rep(0, 22)
}

if ('FIXEDAO' %in% case_parts) {
  process_model[['a']] <- c(rep(NA, 11), rep(0, 11))
}

if ('FIXEDWO' %in% case_parts) {
  process_model[['w']] <- c(rep(NA, 11), rep(400, 11))
}

if ('FIXEDWO2' %in% case_parts) {
  process_model[['w']] <- c(rep(NA, 11), rep(100, 11))
}

if ('FIXEDWO3' %in% case_parts) {
  process_model[['w']] <- c(rep(NA, 11), rep(25, 11))
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

if ('FIXEDO' %in% case_parts) {
  process_model <- update(
    process_model,
    perturbations = process_model$perturbations %>% filter(region <= 11),
    sensitivities = process_model$sensitivities %>% filter(region <= 11),
    a_factor = factor(sprintf('Region %d', 1 : 11)),
    w_factor = factor(sprintf('Region %d', 1 : 11))
  )
}

time_since <- with(process_model$control_mole_fraction, {
  as.double(time - lubridate::ymd_hms('2015-01-01 00:00:00'), units = 'days')
})

time_trend <- time_since / max(time_since)
zonal_basis <- FRK::local_basis(
  FRK::real_line(),
  loc = matrix(seq(-90, 90, length = 5)),
  scale = rep(50, 5)
)

if ('BGINT' %in% case_parts) {
  process_model$Psi <- process_model$Psi[, 1, drop = FALSE]
}

if ('NOBG' %in% case_parts) {
  process_model$Psi <- matrix(0, nrow = nrow(process_model$Psi), ncol = 0)
}

if ('BGTRN' %in% case_parts) {
  process_model$Psi <- cbind(
    process_model$Psi[, 1, drop = FALSE],
    time_trend
  )
}

if ('BGLATTRN' %in% case_parts) {
  Psi_zonal <- FRK::eval_basis(zonal_basis, as.matrix(process_model$control_mole_fraction$latitude))

  process_model$Psi <- cbind(
    1,
    time_trend * Psi_zonal
  )
}

if ('BGTRNLATTRN' %in% case_parts) {
  Psi_zonal <- FRK::eval_basis(zonal_basis, as.matrix(process_model$control_mole_fraction$latitude))

  process_model$Psi <- cbind(
    1,
    time_trend,
    Psi_zonal,
    time_trend * Psi_zonal
  )
}

if ('BGLATSEA1' %in% case_parts) {
  Psi_zonal <- FRK::eval_basis(zonal_basis, as.matrix(process_model$control_mole_fraction$latitude))

  cycle <- cospi(2 * (time_since + 260.7281) / 365.25)

  process_model$Psi <- cbind(
    1,
    time_trend,
    cycle * Psi_zonal
  )
}

if ('BGLATSEA2' %in% case_parts) {
  Psi_zonal <- FRK::eval_basis(zonal_basis, as.matrix(process_model$control_mole_fraction$latitude))

  cycle <- cospi(2 * (time_since + 260.7281 - (365.25 / 12) * 4) / 365.25)

  process_model$Psi <- cbind(
    1,
    time_trend,
    cycle * Psi_zonal
  )
}

if ('BGLATSEA3' %in% case_parts) {
  Psi_zonal <- FRK::eval_basis(zonal_basis, as.matrix(process_model$control_mole_fraction$latitude))

  cycle <- cospi(2 * (time_since + 260.7281 + (365.25 / 12) * 4) / 365.25)

  process_model$Psi <- cbind(
    1,
    time_trend,
    cycle * Psi_zonal
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

process_model$eta_prior_mean <- rep(0, ncol(process_model$Psi))
process_model$eta_prior_precision <- Diagonal(x = 1 / 25, n = ncol(process_model$Psi))

if ('ETAAR1' %in% case_parts) {
  n_bands <- attr(process_model$Psi, 'n_latitude_bands')
  n_months <- (ncol(process_model$Psi) - 1) / n_bands
  Q_base <- (1 / 25) * ar1_Q(n_months, 0.99)

  permutation <- rep(0, n_bands * n_months)
  for (i in seq_len(n_months)) {
    permutation[
      (1 + (i - 1) * n_bands) : (i * n_bands)
    ] <- i + (0 : (n_bands - 1)) * n_months
  }
  process_model$eta_prior_precision <- bdiag(
    1 / 25,
    bdiag(rep(list(Q_base), n_bands))[permutation, permutation]
  )
}

if ('ETAAR1B' %in% case_parts) {
  n_bands <- attr(process_model$Psi, 'n_latitude_bands')
  n_months <- (ncol(process_model$Psi) - 1) / n_bands
  Q_base <- (1 / 6) * ar1_Q(n_months, 0.99)

  permutation <- rep(0, n_bands * n_months)
  for (i in seq_len(n_months)) {
    permutation[
      (1 + (i - 1) * n_bands) : (i * n_bands)
    ] <- i + (0 : (n_bands - 1)) * n_months
  }
  process_model$eta_prior_precision <- bdiag(
    1 / 6,
    bdiag(rep(list(Q_base), n_bands))[permutation, permutation]
  )
}

if ('FULLH' %in% case_parts) {
  xco2_weighted_df <- readRDS('~/FluxInversion/20201013_background/xco2-weighted.rds') %>%
    mutate(from_month_start = as.Date(sprintf('%04d-%02d-01', year, month))) %>%
    select(region, from_month_start, value)
  xco2_weighted <- expand.grid(
    region = sort(unique(process_model$perturbations$region)),
    from_month_start = sort(unique(process_model$perturbations$from_month_start))
  ) %>%
    left_join(xco2_weighted_df, by = c('region', 'from_month_start'))

  H_full <- as.matrix(process_model$H)
  for (j in seq_len(ncol(H_full))) {
    max_time <- max(process_model$control_mole_fraction$time[H_full[, j] > 0])
    future_indices <- which(process_model$control_mole_fraction$time > max_time)
    H_full[future_indices, j] <- xco2_weighted$value[j]
  }
  process_model$H <- H_full
}

if ('FULLHEXP' %in% case_parts) {
  process_model$H <- readRDS(args$transport_matrix_fullhexp)
}

if ('NOBIAS' %in% case_parts) {
  measurement_model$A <- matrix(0, nrow = nrow(measurement_model$A), ncol = 0)
  measurement_model$beta_prior_mean <- rep(0, 0)
  measurement_model$beta_prior_precision <- Matrix::Diagonal(n = 0)
}

if ('DEEPPRIOR' %in% case_parts) {
  added_regions <- list(
    list(region = 2, month = 10, alpha = 0.25),
    list(region = 2, month = 11, alpha = 0.25),
    list(region = 2, month = 22, alpha = 0.25),
    list(region = 2, month = 23, alpha = 0.25),
    list(region = 7, month = 10, alpha = 0.25),
    list(region = 7, month = 11, alpha = 0.25),
    list(region = 7, month = 22, alpha = 0.25),
    list(region = 7, month = 23, alpha = 0.25),
    list(region = 8, month = 10, alpha = 0.25),
    list(region = 8, month = 11, alpha = 0.25),
    list(region = 8, month = 22, alpha = 0.25),
    list(region = 8, month = 23, alpha = 0.25),
    list(region = 11, month = 9, alpha = 0.25),
    list(region = 11, month = 10, alpha = 0.25),
    list(region = 11, month = 11, alpha = 0.25),
    list(region = 11, month = 21, alpha = 0.25),
    list(region = 11, month = 22, alpha = 0.25),
    list(region = 11, month = 23, alpha = 0.25)
  )
  process_model$Gamma <- matrix(
    1,
    nrow = ncol(process_model$H),
    ncol = 1 + length(added_regions)
  )
  for (i in seq_along(added_regions)) {
    process_model$Gamma[, i + 1] <- 0

    alpha_index <- (added_regions[[i]]$month - 1) * 22 + added_regions[[i]]$region
    process_model$Gamma[alpha_index, 1] <- 0
    process_model$Gamma[alpha_index, i + 1] <- 1
  }
  process_model$kappa <- c(0, sapply(added_regions, getElement, 'alpha'))

  stopifnot(all(rowSums(process_model$Gamma) == 1))
}

# HACK(mgnb): later code is configured to load these if missing
process_model$sensitivities <- NULL
process_model$H <- NULL

output <- as_case(
  process_model,
  measurement_model
)

log_info_case('Saving')
saveRDS_gz1(output, args$output)

log_info_case('Done')
