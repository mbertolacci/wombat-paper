source(Sys.getenv('RESULTS_BASE_PARTIAL'))
library(argparse)

parser <- ArgumentParser()
parser$add_argument('--mcmc-samples-lg')
parser$add_argument('--mcmc-samples-ln')
parser$add_argument('--output')
args <- parser$parse_args()

as_df <- function(samples) {
  n_iterations <- nrow(samples$beta)
  bind_rows(
    data.frame(
      variable = 'gamma',
      value = as.vector(1 / samples$gamma)
    ),
    data.frame(
      variable = 'rho',
      value = as.vector(samples$rho)
    ),
    data.frame(
      variable = 'ell',
      value = as.vector(samples$ell)
    )
  ) %>%
    group_by(variable) %>%
    summarise(
      mean = mean(value),
      q025 = quantile(value, probs = 0.025),
      q500 = quantile(value, probs = 0.500),
      q975 = quantile(value, probs = 0.975)
    )
}

samples_summary_LG <- readRDS(args$mcmc_samples_lg) %>%
    as_df()
samples_summary_LN <- readRDS(args$mcmc_samples_ln) %>%
    as_df()


sink(args$output)
cat('\\begin{tabular}{l|lll|lll}\n\\hline\n')
cat(' & \\multicolumn{3}{c|}{LG} & \\multicolumn{3}{c}{LN} \\\\\n')
cat('Variable & Mean & 2.5\\% & 97.5\\% & Mean & 2.5\\% & 97.5\\% \\\\ \\hline\n')
for (i in seq_len(nrow(samples_summary_LG))) {
  printf(
    '%s & %.03f & %.03f & %.03f & %.03f & %.03f & %.03f \\\\\n',
    c(
      'ell' = '$\\ell_{g, 1}$',
      'gamma' = '$\\gamma_g$',
      'rho' = '$\\rho_g$'
    )[samples_summary_LG$variable[i]],
    samples_summary_LG$mean[i],
    samples_summary_LG$q025[i],
    samples_summary_LG$q975[i],
    samples_summary_LN$mean[i],
    samples_summary_LN$q025[i],
    samples_summary_LN$q975[i]
  )
}
cat('\\hline\n\\end{tabular}')
sink(NULL)
