with_traceability_attributes <- function(x) {
  for (name in c('year', 'region', 'month')) {
    if (name %in% names(x)) {
      x[[name]] <- as.integer(x[[name]])
    }
  }
  x$date_created <- format(Sys.Date())
  x$hostname <- Sys.info()['nodename']
  x$git_commit <- system('git rev-parse HEAD', intern = TRUE)
  x
}

write_nc <- function(
  df,
  filename,
  dimension_name = 'obs',
  units,
  long_name,
  attributes
) {
  if (dimension_name %in% colnames(df)) {
    dimension <- ncdim_def(
      dimension_name,
      vals = df[[dimension_name]],
      units = units[dimension_name],
      longname = long_name[dimension_name]
    )
  } else {
    dimension <- ncdim_def(
      dimension_name,
      vals = seq_len(nrow(df)),
      units = '',
      create_dimvar = FALSE
    )
  }
  variable_names <- setdiff(colnames(df), dimension_name)
  variables <- lapply(variable_names, function(name) {
    dim <- dimension
    if (class(df[[name]])[1] == 'character') {
      max_length <- max(stringr::str_length(df[[name]]))
      dim <- list(
        ncdim_def(sprintf('%s_nchar', name), '', seq_len(max_length), create_dimvar = FALSE),
        dimension
      )
    }
    ncvar_def(
      name,
      units = units[name],
      longname = long_name[name],
      dim = dim,
      prec = c(
        'integer' = 'integer',
        'array' = 'double',
        'numeric' = 'double',
        'factor' = 'integer',
        'character' = 'char'
      )[class(df[[name]])[1]]
    )
  })

  nc_fn <- nc_create(filename, variables)
  on.exit(nc_close(nc_fn))

  for (i in seq_along(variable_names)) {
    ncvar_put(
      nc_fn,
      variables[[i]],
      df[[variable_names[[i]]]]
    )
  }

  if (!missing(attributes)) {
    for (name in names(attributes)) {
      ncatt_put(nc_fn, 0, name, attributes[[name]])
    }
  }
}

interpolate_vco2_pce <- function(from_vco2, from_pressure_edge, to_pressure_level) {
  n_levels_to <- ncol(to_pressure_level)

  k_for_l <- matrix(0L, nrow = nrow(from_vco2), ncol = n_levels_to)
  for (l in 1 : n_levels_to) {
    # NOTE(mgnb): if OCO-2 surface pressure is higher than GC, this is 0
    k_for_l[, l] <- rowSums(
      to_pressure_level[, l] <= from_pressure_edge
    )
  }
  to_vco2 <- to_pressure_level
  for (l in 1 : n_levels_to) {
    to_vco2[, l] <- from_vco2[cbind(
      seq_len(nrow(from_vco2)),
      # NOTE(mgnb): this extends the bottom GC layer down to the surface when
      # the to surface pressure is higher than the GC
      pmax(1, k_for_l[, l])
    )]
  }
  to_vco2
}

# HACK(mgnb): base::rbind is very slow with matrix columns, while
# dplyr::bind_rows doesn't support them. This is much faster.
rbind_alt <- function(xs) {
  x1 <- xs[[1]]
  output <- lapply(colnames(x1), function(name) {
    do.call(
      if (is.null(dim(x1[[name]])) || is.vector(x1[[name]])) c else rbind,
      lapply(xs, getElement, name)
    )
  })
  names(output) <- colnames(x1)
  as_tibble(output)
}
