library(ggplot2)

theme_set(theme_bw())
theme_replace(
  legend.background = element_blank(),
  legend.key = element_blank(),
  panel.background = element_blank(),
  strip.background = element_blank(),
  plot.background = element_blank(),
  panel.border = element_blank()
)

display_settings <- list(
  png_plot_dpi = 320
)

ggsave_base <- function(filename, plot, bg = 'transparent', dpi = display_settings$png_plot_dpi, ...) {
  ggsave(
    filename,
    plot,
    units = 'cm',
    dpi = dpi,
    bg = bg,
    ...
  )
}

A_PAPER_SIZES <- list(
  widths = c(
    'a0' = 84.1,
    'a1' = 59.4,
    'a2' = 42.0,
    'a3' = 29.7,
    'a4' = 21.0
  ),
  heights = c(
    'a0' = 118.9,
    'a1' = 84.1,
    'a2' = 59.4,
    'a3' = 42.0,
    'a4' = 29.7
  )
)

ggsave_size <- function(..., size = 'a4', width = size, height = width) {
  if (is.character(width)) {
    match <- stringr::str_match(width, '(a\\d)_?(portrait|landscape)?')
    if (!is.na(match[3]) && match[3] == 'landscape') {
      width <- A_PAPER_SIZES$heights[width]
    } else {
      width <- A_PAPER_SIZES$widths[width]
    }
  }
  if (is.character(height)) {
    match <- stringr::str_match(height, '(a\\d)_?(portrait|landscape)?')
    if (!is.na(match[3]) && match[3] == 'landscape') {
      height <- A_PAPER_SIZES$widths[height]
    } else {
      height <- A_PAPER_SIZES$heights[height]
    }
  }
  ggsave_base(..., width = width, height = height)
}

get_legend <- function(plot_object){
  tmp <- ggplot_gtable(ggplot_build(plot_object))
  legend_index <- which(sapply(tmp$grobs, function(x) x$name) == 'guide-box')
  tmp$grobs[[legend_index]]
}
