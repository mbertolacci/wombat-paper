source(Sys.getenv('RESULTS_BASE_PARTIAL'))
library(argparse)
library(tidyr)

parser <- ArgumentParser()
parser$add_argument('--tccon-metric-df')
parser$add_argument('--output')
args <- parser$parse_args()

overall_metric_df <- fst::read_fst(args$tccon_metric_df) %>%
  group_by(case, group) %>%
  summarise(
    mmse = mean(mse)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = case, values_from = mmse)

sink(args$output)
printf('\\begin{tabular}{l|%s|ll}\n', collapse0(rep('l', nlevels(overall_metric_df$group) - 2)))
printf('& %s\\\\ \\hline\n', paste_columns(
  sprintf(
    '\\multicolumn{1}{%sc}{\\adjustbox{angle=45,lap=\\width-1em}{%s}}',
    ifelse(overall_metric_df$group == 'WOMBAT Post.', '|', ''),
    overall_metric_df$group
  )
))
for (mode in c('LG', 'LN')) {
  printf('%s & %s\\\\\n', mode, paste_columns(
    sprintf('%.02f', overall_metric_df[[mode]])
  ))
}
cat('\\hline\n\\end{tabular}\n')
sink(NULL)
