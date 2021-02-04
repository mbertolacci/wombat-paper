source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)

parser <- ArgumentParser()
parser$add_argument('--output')
args <- parser$parse_args()

sink(args$output)
cat('\\begin{tabular}{lll}\n\\hline\n')
printf('Code & Name & Type\\\\ \\hline\n')
for (i in seq_along(REGION_NAMES)) {
  printf(
    '%s & %s & %s \\\\\n',
    REGION_CODES[i],
    REGION_NAMES[i],
    if (i <= 11) 'Land' else 'Ocean'
  )
}
cat('\\hline\n\\end{tabular}\n')
sink(NULL)
