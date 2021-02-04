source(Sys.getenv('INVERSION_BASE_PARTIAL'))
source(Sys.getenv('INVERSION_DISPLAY_PARTIAL'))
library(coda)
library(grid)
library(gridExtra, warn.conflicts = FALSE)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--samples', '') %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading MCMC samples')
samples <- readRDS(args$samples) %>%
  window(start = 101)

log_info('Plotting')
output <- plot_traces(samples)

log_info('Saving')
ggsave_size(args$output, output, size = 'a1', dpi = 100, bg = 'white')

log_info('Done')
