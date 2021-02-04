source(Sys.getenv('RESULTS_BASE_PARTIAL'))
library(argparse)

N_REGIONS_TO_INCLUDE <- 11

parser <- ArgumentParser()
parser$add_argument('--mcmc-samples-lg')
parser$add_argument('--mcmc-samples-ln')
parser$add_argument('--output')
args <- parser$parse_args()

as_df <- function(samples) {
  n_iterations <- nrow(samples$beta)
  bind_rows(
    data.frame(
      variable = 'kappa',
      name = rep(colnames(samples$a)[seq_len(N_REGIONS_TO_INCLUDE)], each = n_iterations),
      value = as.vector(samples$a[, seq_len(N_REGIONS_TO_INCLUDE)])
    ),
    data.frame(
      variable = 'tau_w',
      name = rep(colnames(samples$w)[seq_len(N_REGIONS_TO_INCLUDE)], each = n_iterations),
      value = as.vector(
        (samples$w / (1 - samples$a ^ 2))[, seq_len(N_REGIONS_TO_INCLUDE)]
      )
    ),
    data.frame(
      variable = 'gamma',
      name = 'gamma',
      value = as.vector(samples$gamma)
    ),
    data.frame(
      variable = 'rho',
      name = 'rho',
      value = as.vector(samples$rho)
    ),
    data.frame(
      variable = 'ell',
      name = 'ell',
      value = as.vector(samples$ell)
    )
  )
}

samples_summary_df <- bind_rows(
  readRDS(args$mcmc_samples_lg) %>%
    as_df() %>%
    mutate(group = 'LG'),
  readRDS(args$mcmc_samples_ln) %>%
    as_df() %>%
    mutate(group = 'LN')
) %>%
  group_by(group, variable, name) %>%
  summarise(
    mean = mean(value),
    q025 = quantile(value, probs = 0.025),
    q500 = quantile(value, probs = 0.500),
    q975 = quantile(value, probs = 0.975)
  )

sink(args$output)
samples_summary_df %>%
  knitr::kable(digits = 3)
sink(NULL)
