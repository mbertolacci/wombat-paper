library(raster)

read_transcom_raster <- function(filename) {
  output <- raster(filename, varname = 'Region00')
  values(output) <- NA

  with_nc_file(list(fn = filename), {
    v <- function(...) ncdf4::ncvar_get(fn, ...)

    regions <- as.integer(substr(names(fn$var), 7, 9))
    regions <- regions[regions > 0]

    for (region in regions) {
      x <- v(sprintf('Region%02d', region))
      values(output) <- ifelse(
        as.vector(x[, ncol(x) : 1]),
        region,
        values(output)
      )
    }
  })
  names(output) <- 'region'
  output %>%
    rotate() %>%
    setZ(NULL)
}

# transcom_raster_to_boundary <- function(transcom_raster) {
#   transcom_raster %>%
#     rasterToPolygons(dissolve = TRUE) %>%
#     st_as_sf() %>%
#     st_cast('MULTILINESTRING') %>%
#     st_crop(
#       c(ymin = -90, ymax = 90, xmin = -178.74, xmax = 181.24)
#     )
# }
