source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_DISPLAY_PARTIAL'))
library(argparse)
library(lubridate, warn.conflicts = FALSE)
library(wombat)
library(ncdf4)

parser <- ArgumentParser()
parser$add_argument('--control-emissions')
parser$add_argument('--perturbations')
parser$add_argument('--xco2-daily-base')
parser$add_argument('--xco2-daily-201601r02')
parser$add_argument('--xco2-daily-201601r06')
parser$add_argument('--output')
args <- parser$parse_args()

GEOS_CHEM_GRID <- list(
  longitude = list(
    centres = seq(-180, 177.51, 2.5),
    widths = rep(2.5, 144)
  ),
  latitude = list(
    centres = c(-89.5, seq(-88, 88.1, 2), 89.5),
    widths = c(1, rep(2, 89), 1)
  )
)

clamp <- function(x, a, b) pmin(pmax(x, a), b)

low_colour <- '#35978f'
mid_colour <- '#ffffff'
high_colour <- '#bf812d'
flux_max_abs <- 1.5
sensitivity_max_abs <- 0.3

read_xco2_daily <- function(filename) {
  with_nc_file(list(fn = filename), {
    times <- ymd_hms('2000-01-01 00:00:00') + minutes(ncvar_get(fn, 'time'))
    xco2 <- ncvar_get(fn, 'xco2')
  })

  expand.grid(
    longitude_index = seq_along(GEOS_CHEM_GRID$longitude$centres),
    latitude_index = seq_along(GEOS_CHEM_GRID$latitude$centres),
    time = times
  ) %>%
    mutate(
      date = as.Date(time),
      longitude = GEOS_CHEM_GRID$longitude$centres[longitude_index],
      cell_width = GEOS_CHEM_GRID$longitude$widths[longitude_index],
      latitude = GEOS_CHEM_GRID$latitude$centres[latitude_index],
      cell_height = GEOS_CHEM_GRID$latitude$widths[latitude_index],
      xco2 = as.vector(xco2)
    ) %>%
    select(-time, -longitude_index, -latitude_index)
}

xco2_base_df <- read_xco2_daily(args$xco2_daily_base)
xco2_201601r02_df <- read_xco2_daily(args$xco2_daily_201601r02) %>%
  mutate(region = 2)
xco2_201601r06_df <- read_xco2_daily(args$xco2_daily_201601r06) %>%
  mutate(region = 6)

xco2_sensitivity <- bind_rows(
  xco2_201601r02_df,
  xco2_201601r06_df
) %>%
  left_join(
    xco2_base_df %>% select(date, longitude, latitude, xco2_base = xco2),
    by = c('date', 'longitude', 'latitude')
  ) %>%
  mutate(
    xco2_sensitivity = xco2 - xco2_base
  )

control_emissions <- fst::read_fst(args$control_emissions)
perturbations <- fst::read_fst(args$perturbations) %>%
  left_join(
    control_emissions %>%
      select(
        model_id,
        month_start,
        longitude,
        cell_width,
        latitude,
        cell_height
      ),
    by = 'model_id'
  ) %>%
  mutate(flux_density = flux_density * 31536000)

perturbations_i <- perturbations %>%
  filter(region %in% c(2, 6), month_start == '2016-01-01', abs(flux_density) > 0) %>%
  mutate(
    date = '2016-01',
    region = sprintf('TransCom %02d', region)
  )

sensitivities_i <- xco2_sensitivity %>%
  filter(
    region %in% c(2, 6),
    format(date) %in% c(
      '2016-01-01',
      '2016-01-15',
      '2016-02-15'
    )
  ) %>%
  mutate(
    region = sprintf('TransCom %02d', region)
  )

flux_plot <- ggplot() +
  geom_tile(
    data = perturbations_i,
    mapping = aes(
      longitude,
      latitude,
      width = cell_width,
      height = cell_height,
      fill = clamp(flux_density, -flux_max_abs, flux_max_abs)
    )
  ) +
  geom_world() +
  coord_quickmap() +
  scale_fill_gradient2(
    low = low_colour,
    mid = mid_colour,
    high = high_colour,
    limits = c(-1, 1) * flux_max_abs
  ) +
  labs(
    x = 'Longitude',
    y = 'Latitude'
  ) +
  facet_grid(date ~ region) +
  scale_x_continuous(expand = c(0, 0), limits = c(-180, 180)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-90, 90)) +
  guides(fill = guide_colourbar(
    title = expression('[kg/'*m^2*'/year]'),
    title.position = 'right',
    title.theme = element_text(angle = 90, hjust = 0.5),
    barheight = 6,
    frame.colour = 'black'
  )) +
  theme(
    legend.position = 'right',
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank()
  )

sensitivity_plot <- ggplot() +
  geom_tile(
    data = sensitivities_i,
    mapping = aes(
      longitude,
      latitude,
      width = cell_width,
      height = cell_height,
      fill = clamp(xco2_sensitivity, -sensitivity_max_abs, sensitivity_max_abs)
    )
  ) +
  geom_world() +
  coord_quickmap() +
  scale_fill_gradient2(
    low = low_colour,
    mid = mid_colour,
    high = high_colour
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(-180, 180)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-90, 90)) +
  labs(x = 'Longitude', y = 'Latitude') +
  facet_grid(date ~ region) +
  guides(fill = guide_colourbar(
    title = '[ppm]',
    title.position = 'right',
    title.theme = element_text(angle = 90, hjust = 0.5),
    barheight = 10,
    frame.colour = 'black'
  )) +
  theme(
    legend.position = 'right',
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank()
  )

plot_width <- DISPLAY_SETTINGS$full_width
plot_height <- DISPLAY_SETTINGS$full_height - 8
legend_width <- 2.2
title_height <- 1
plot_margin <- 0.5
spacer <- 0.5
row_height <- (plot_height - spacer - 2 * title_height - 2 * plot_margin) / 4

output <- gridExtra::arrangeGrob(
  grobs = list(
    grid::textGrob(expression(phi[j](bold(s), t))),
    flux_plot + theme(legend.position = 'none'),
    get_legend(flux_plot),
    grid::textGrob(expression(psi[j](bold(s), h, t))),
    sensitivity_plot + theme(legend.position = 'none'),
    get_legend(sensitivity_plot)
  ),
  layout_matrix = rbind(
    c(1, NA),
    c(2, 3),
    c(NA, NA),
    c(4, NA),
    c(5, 6)
  ),
  heights = c(
    title_height,
    row_height + plot_margin,
    spacer,
    title_height,
    3 * row_height + plot_margin
  ),
  widths = c(
    plot_width - legend_width,
    legend_width
  )
)

ggsave(
  args$output,
  plot = output,
  width = plot_width,
  height = plot_height,
  units = 'cm'
)
