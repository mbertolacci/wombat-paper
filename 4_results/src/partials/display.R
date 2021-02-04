library(ggplot2)

theme_set(theme_bw())
theme_replace(
  legend.background = element_blank(),
  legend.key = element_blank(),
  panel.background = element_blank(),
  strip.background = element_blank(),
  plot.background = element_blank(),
  # panel.border = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.text = element_text(colour = '#23373b'),
  axis.title = element_text(colour = '#23373b')
)

DISPLAY_SETTINGS <- list(
  full_width = 16.8,
  full_height = 25.7,
  png_plot_dpi = 320
)

get_legend <- function(plot_object){
  tmp <- ggplot_gtable(ggplot_build(plot_object))
  legend_index <- which(sapply(tmp$grobs, function(x) x$name) == 'guide-box')
  tmp$grobs[[legend_index]]
}

ggsave_base <- function(filename, plot, bg = 'transparent', dpi = DISPLAY_SETTINGS$png_plot_dpi, ...) {
  ggsave(
    filename,
    plot,
    units = 'cm',
    dpi = dpi,
    bg = bg,
    ...
  )
}
