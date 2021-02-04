source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_DISPLAY_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)
library(wombat)
library(sf)
library(raster, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--transcom-map')
parser$add_argument('--output')
args <- parser$parse_args()

transcom_regions_sp <- raster(args$transcom_map) %>%
  rotate() %>%
  rasterToPolygons(dissolve = TRUE)

transcom_regions_sf <- transcom_regions_sp %>%
  st_as_sf() %>%
  rename(region = regions) %>%
  mutate(region_code = sprintf('T%02d', region)) %>%
  filter(region != 0) %>%
  arrange(region)

transcom_centroids <- transcom_regions_sf %>%
  st_centroid() %>%
  st_coordinates()

transcom_centroids[12, 1] <- 160
transcom_centroids[13, 1] <- 160
transcom_centroids[15, 1] <- -120

output <- ggplot() +
  geom_sf(
    data = transcom_regions_sf,
    mapping = aes(fill = region_code),
    colour = 'black',
    size = 0.2
  ) +
  geom_text(
    data = cbind(
      as.data.frame(transcom_regions_sf) %>% dplyr::select(-geometry),
      transcom_centroids,
      data.frame(
        colour = c(
          'white',
          'white',
          'black',
          'black',
          'black',
          'black',
          'white',
          'white',
          'white',
          'black',
          'black',
          'white',
          'white',
          'white',
          'white',
          'black',
          'black',
          'black',
          'black',
          'black',
          'black',
          'black'
        ),
        stringsAsFactors = FALSE
      )
    ),
    mapping = aes(
      x = X,
      y = Y,
      label = region_code,
      colour = colour
    )
  ) +
  geom_path(
    data = data.frame(
      longitude = c(-178.75, 181.25, 181.25, -178.75, -178.75),
      latitude = c(-77.68421, -77.68421, 90.94737, 90.94737, -77.68421)
    ),
    mapping = aes(longitude, latitude),
    colour = 'black',
    size = 0.2
  ) +
  guides(colour = FALSE, fill = FALSE) +
  scale_colour_manual(values = c('black', 'white')) +
  scale_fill_manual(values = c(
    rev(RColorBrewer::brewer.pal(9, 'BuGn'))[1 : 6],
    rev(RColorBrewer::brewer.pal(9, 'OrRd'))[1 : 5],
    colorRampPalette(rev(RColorBrewer::brewer.pal(9, 'PuBu')[3 : 9]))(11)
  )) +
  theme(
    plot.margin = margin(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width,
  height = 8,
  units = 'cm'
)
