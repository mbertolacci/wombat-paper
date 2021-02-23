source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_DISPLAY_PARTIAL'))
library(argparse)

parser <- ArgumentParser()
parser$add_argument('--tccon-metric-df')
parser$add_argument('--output')
args <- parser$parse_args()

metric_df <- fst::read_fst(args$tccon_metric_df)

side_colourbar <- guide_colourbar(
  title.position = 'right',
  title.theme = element_text(angle = 90, hjust = 0.5),
  barheight = 10,
  frame.colour = 'black'
)

plot_theme <- theme(
  axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
  axis.text.y = element_text(size = 8)
)

output <- gridExtra::arrangeGrob(
  ggplot(metric_df, aes(group, station, fill = pmin(pmax(bias, -1.5), 1.5))) +
    geom_tile() +
    geom_vline(xintercept = 9.5, linetype = 2) +
    geom_vline(xintercept = 10.5, linetype = 3) +
    facet_wrap(~ case, ncol = 2) +
    scale_fill_gradientn(
      colours = rev(RColorBrewer::brewer.pal(11, 'RdBu')),
      limits = c(-1.5, 1.5),
      guide = side_colourbar
    ) +
    labs(x = NULL, y = NULL, fill = 'Mean error [ppm]') +
    plot_theme,
  ggplot(metric_df, aes(group, station, fill = pmin(stdev, 1.5))) +
    geom_tile() +
    geom_vline(xintercept = 9.5, linetype = 2) +
    geom_vline(xintercept = 10.5, linetype = 3) +
    facet_wrap(~ case, ncol = 2) +
    scale_fill_gradientn(
      colours = c(RColorBrewer::brewer.pal(9, 'YlOrBr'), '#000000'),
      guide = side_colourbar,
      limits = c(0, 1.5)
    ) +
    labs(x = NULL, y = NULL, fill = 'Error st. dev. [ppm]') +
    plot_theme,
  ncol = 1
)

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width,
  height = DISPLAY_SETTINGS$full_height - 3.5
)
