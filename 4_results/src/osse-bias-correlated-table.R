source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_DISPLAY_PARTIAL'))
library(argparse)
library(tidyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--flux-samples-lg-bias-correlated')
parser$add_argument('--flux-samples-lg-no-bias-correlated')
parser$add_argument('--flux-samples-lg-bias-uncorrelated')
parser$add_argument('--flux-samples-lg-no-bias-uncorrelated')
parser$add_argument('--flux-samples-lg-no-bias-uncorrelated-fixedhyper')
parser$add_argument('--flux-samples-ln-bias-correlated')
parser$add_argument('--flux-samples-ln-no-bias-correlated')
parser$add_argument('--flux-samples-ln-bias-uncorrelated')
parser$add_argument('--flux-samples-ln-no-bias-uncorrelated')
parser$add_argument('--flux-samples-ln-no-bias-uncorrelated-fixedhyper')
parser$add_argument('--output')
args <- parser$parse_args()

read_flux_samples <- function(filename, estimates = 'Posterior') {
  readRDS(filename) %>%
    filter(
      estimate %in% estimates,
      name %in% sprintf('T%02d', 1 : 22)
    )
}

log_info('Loading flux samples')
flux_samples <- bind_rows(
  read_flux_samples(
    args$flux_samples_ln_bias_correlated,
    c('Truth', 'Prior', 'Posterior')
  ) %>%
    mutate(
      group = 'LN',
      estimate = ifelse(
        estimate == 'Posterior',
        'Bias correction/correlated errors',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_ln_no_bias_correlated,
  ) %>%
    mutate(
      group = 'LN',
      estimate = 'No bias correction/correlated errors'
    ),
  read_flux_samples(
    args$flux_samples_ln_no_bias_uncorrelated,
  ) %>%
    mutate(
      group = 'LN',
      estimate = 'No bias correction/uncorrelated errors'
    ),
  read_flux_samples(
    args$flux_samples_ln_bias_uncorrelated,
  ) %>%
    mutate(
      group = 'LN',
      estimate = 'Bias correction/uncorrelated errors'
    ),
  read_flux_samples(
    args$flux_samples_ln_no_bias_uncorrelated_fixedhyper,
  ) %>%
    mutate(
      group = 'LN',
      estimate = 'No bias correction/uncorrelated errors/fixed hyperparameters'
    ),
  read_flux_samples(
    args$flux_samples_lg_bias_correlated,
    c('Truth', 'Prior', 'Posterior')
  ) %>%
    mutate(
      group = 'LG',
      estimate = ifelse(
        estimate == 'Posterior',
        'Bias correction/correlated errors',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_lg_no_bias_correlated,
  ) %>%
    mutate(
      group = 'LG',
      estimate = 'No bias correction/correlated errors'
    ),
  read_flux_samples(
    args$flux_samples_lg_no_bias_uncorrelated,
  ) %>%
    mutate(
      group = 'LG',
      estimate = 'No bias correction/uncorrelated errors'
    ),
  read_flux_samples(
    args$flux_samples_lg_bias_uncorrelated,
  ) %>%
    mutate(
      group = 'LG',
      estimate = 'Bias correction/uncorrelated errors'
    ),
  read_flux_samples(
    args$flux_samples_lg_no_bias_uncorrelated_fixedhyper,
  ) %>%
    mutate(
      group = 'LG',
      estimate = 'No bias correction/uncorrelated errors/fixed hyperparameters'
    )
) %>%
  ungroup()

output <- flux_samples %>%
  filter(estimate != 'Truth') %>%
  left_join(
    flux_samples %>%
      filter(estimate == 'Truth') %>%
      select(name, month_start, flux_truth = flux_mean),
    by = c('name', 'month_start')
  ) %>%
  group_by(estimate, group) %>%
  summarise(
    rmse = sqrt(mean((flux_mean - flux_truth) ^ 2)),
    mcrps = mean(scoringRules::crps_sample(
      flux_truth,
      flux_samples
    ))
  ) %>%
  ungroup()

estimates <- unique(sort(output$estimate))
groups <- unique(sort(output$group))

rmse_matrix <- output %>%
  select(estimate, group, rmse) %>%
  pivot_wider(names_from = group, values_from = rmse) %>%
  select(-estimate) %>%
  as.matrix()

mcrps_matrix <- output %>%
  select(estimate, group, mcrps) %>%
  pivot_wider(names_from = group, values_from = mcrps) %>%
  select(-estimate) %>%
  as.matrix()

sink(args$output)
printf(
  '\\begin{tabular}{l%s}\n\\hline\n',
  paste0(
    rep(collapse0(rep('l', length(groups))), 2),
    collapse = ''
  )
)
cat('& \\multicolumn{2}{c}{RMSE [PgC mo$^{-1}$]} & \\multicolumn{2}{c}{CRPS} \\\\\n')
printf(
  'Setup & %s \\\\\n \\hline\n',
  paste_columns(rep(paste_columns(groups), 2))
)
for (i in seq_along(estimates)) {
  printf(
    '%s & %s & %s \\\\\n',
    estimates[i],
    paste_columns(sprintf('%.03f', rmse_matrix[i, ])),
    paste_columns(sprintf('%.03f', mcrps_matrix[i, ]))
  )
}
cat('\\hline\n\\end{tabular}\n')
sink(NULL)
