quantiles_log_density <- function(
  log_density,
  lower,
  upper,
  probs,
  grid_size = 1e-3
) {
  x_grid <- seq(lower, upper, by = grid_size)
  f_grid <- exp(log_density(x_grid))
  F_grid_unnormalised <- grid_size * (
    cumsum(f_grid)
    - 0.5 * f_grid[1]
    - 0.5 * f_grid
  )
  F_grid <- (
    F_grid_unnormalised
    / F_grid_unnormalised[length(F_grid_unnormalised)]
  )

  sapply(probs, function(prob) {
    max(x_grid[F_grid <= prob])
  })
}
