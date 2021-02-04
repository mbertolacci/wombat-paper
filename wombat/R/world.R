#' @export
geom_world <- function(
  include_border = FALSE,
  colour = 'black',
  size = 0.1,
  inherit.aes = FALSE,
  wrap = c(-180, 180),
  ...
) {
  worldmap <- fortify(rnaturalearth::ne_coastline(110))

  if (include_border) {
    border <- data.frame(
      long = c(wrap[1] + 0.01, wrap[1] + 0.01, wrap[2] - 0.01, wrap[2] - 0.01),
      lat = c(-89.99, 89.99, 89.99, -89.99),
      # High group number to avoid duplication
      group = 1e5,
      # new name for border not already used
      region = 'border'
    )
    worldmap <- bind_rows(worldmap, border)
  }

  geom_path(
    data = worldmap,
    mapping = aes(x = long, y = lat, group = group),
    colour = colour,
    size = size,
    inherit.aes = inherit.aes,
    na.rm = TRUE,
    ...
  )
}

#' @export
geom_world_sf <- function(
  colour = 'black',
  size = 0.1,
  inherit.aes = FALSE,
  ...
) {
  worldmap <- sf::st_as_sf(rnaturalearth::ne_coastline(110))

  geom_sf(
    data = worldmap,
    colour = colour,
    size = size,
    inherit.aes = inherit.aes,
    ...
  )
}
