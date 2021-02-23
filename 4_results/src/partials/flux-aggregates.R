ESTIMATE_COLOURS <- c(
  'Truth' = 'black',
  'MIP Prior (min/mean/max)' = '#7570b3',
  'MIP LG (min/mean/max)' = '#d95f02',
  'MIP LN (min/mean/max)' = '#1b9e77',
  'WOMBAT Prior (mean)' = get_colour('wombat_prior'),
  'WOMBAT LG (mean, 95% cred. int.)' = get_colour('wombat_lg'),
  'WOMBAT LN (mean, 95% cred. int.)' = get_colour('wombat_ln'),
  'Prior (mean)' = get_colour('wombat_prior'),
  'LG, correlated errors (mean, 95% cred. int.)' = get_colour('wombat_lg'),
  'LG, uncorrelated errors (mean, 95% cred. int.)' = 'orange',
  'LN, correlated errors (mean, 95% cred. int.)' = get_colour('wombat_ln'),
  'LN, uncorrelated errors (mean, 95% cred. int.)' = '#4a716d',
  'LG, offline-corr. (mean, 95% cred. int.)' = get_colour('wombat_lg'),
  'LG, online-corr. (mean, 95% cred. int.)' = 'orange',
  'LN, offline-corr. (mean, 95% cred. int.)' = get_colour('wombat_ln'),
  'LN, online-corr. (mean, 95% cred. int.)' = '#4a716d'
)

ESTIMATE_LINETYPES = c(
  'Truth' = 'solid',
  'MIP Prior (min/mean/max)' = 'longdash',
  'MIP LG (min/mean/max)' = 'longdash',
  'MIP LN (min/mean/max)' = 'longdash',
  'WOMBAT Prior (mean)' = 'solid',
  'WOMBAT LG (mean, 95% cred. int.)' = 'solid',
  'WOMBAT LN (mean, 95% cred. int.)' = 'solid',
  'Prior (mean)' = 'solid',
  'LG, correlated errors (mean, 95% cred. int.)' = 'solid',
  'LG, uncorrelated errors (mean, 95% cred. int.)' = 'longdash',
  'LN, correlated errors (mean, 95% cred. int.)' = 'solid',
  'LN, uncorrelated errors (mean, 95% cred. int.)' = 'longdash',
  'LG, offline-corr. (mean, 95% cred. int.)' = 'solid',
  'LG, online-corr. (mean, 95% cred. int.)' = 'longdash',
  'LN, offline-corr. (mean, 95% cred. int.)' = 'solid',
  'LN, online-corr. (mean, 95% cred. int.)' = 'longdash'
)

log_info('Loading flux samples')
flux_samples <- flux_samples %>%
  mutate(
    year = year(month_start),
    estimate = factor(ifelse(
      is_prior,
      sprintf('%s (mean)', observation_group),
      sprintf('%s (mean, 95%% cred. int.)', observation_group)
    ), levels = names(ESTIMATE_COLOURS))
  ) %>%
  select(-observation_group)

if (show_mip_fluxes) {
  log_info('Loading MIP fluxes')
  mip_fluxes <- fst::read_fst(args$mip_fluxes)

  mip_fluxes_modified <- mip_fluxes %>%
    mutate(
      name = MIP_REGION_TO_REGION[region_name]
    ) %>%
    filter(
      month_start >= '2015-01-01',
      month_start < '2017-01-01',
      type != 'fossil',
      case %in% c('Prior', 'LG', 'LN')
    ) %>%
    select(name, which = case, everything())

  mip_fluxes_modified <- bind_rows(c(list(mip_fluxes_modified), lapply(
    REGION_AGGREGATES,
    function(region_aggregate) {
      mip_fluxes_modified %>%
        filter(
          name %in% region_aggregate$parts,
          type %in% region_aggregate$types
        ) %>%
        group_by(group, which, type, month_start) %>%
        summarise(
          flux = sum(flux)
        ) %>%
        ungroup() %>%
        mutate(name = region_aggregate$name)
    }
  ))) %>%
    mutate(
      year = year(month_start),
      is_prior = FALSE,
      estimate = factor(sprintf('MIP %s (min/mean/max)', which), levels = names(ESTIMATE_COLOURS))
    ) %>%
    select(-which)
}

log_info('Calculating')
annual_fluxes <- flux_samples %>%
  group_by(is_prior, estimate, name, year) %>%
  summarise(
    flux_mean = sum(flux_mean),
    flux_lower = quantile(colSums(flux_samples), probs = 0.025, na.rm = TRUE),
    flux_upper = quantile(colSums(flux_samples), probs = 0.975, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    flux_lower = if_else(
      is_prior & !show_prior_uncertainty,
      as.double(NA),
      flux_lower
    ),
    flux_upper = if_else(
      is_prior & !show_prior_uncertainty,
      as.double(NA),
      flux_upper
    )
  ) %>%
  filter(
    name %in% MIP_REGION_TO_REGION,
    year %in% c(2015, 2016)
  )

if (show_mip_fluxes) {
  annual_mip_fluxes <- mip_fluxes_modified %>%
    group_by(group, estimate, name, year) %>%
    summarise(
      flux_mean = sum(flux)
    ) %>%
    ungroup() %>%
    group_by(estimate, name, year) %>%
    summarise(
      flux_lower = min(flux_mean, na.rm = TRUE),
      flux_upper = max(flux_mean, na.rm = TRUE),
      flux_mean = mean(flux_mean, na.rm = TRUE)
    ) %>%
    filter(
      year %in% c(2015, 2016)
    )
} else {
  annual_mip_fluxes <- NULL
}

monthly_fluxes <- flux_samples %>%
  filter(
    name %in% MIP_REGION_TO_REGION,
    month_start >= '2015-01-01',
    month_start < '2017-01-01'
  ) %>%
  ungroup() %>%
  mutate(
    flux_lower = if_else(
      is_prior & !show_prior_uncertainty,
      as.double(NA),
      matrixStats::rowQuantiles(flux_samples, probs = 0.025)
    ),
    flux_upper = if_else(
      is_prior & !show_prior_uncertainty,
      as.double(NA),
      matrixStats::rowQuantiles(flux_samples, probs = 0.975)
    )
  ) %>%
  select(estimate, name, month_start, flux_mean, flux_lower, flux_upper)

if (show_mip_fluxes) {
  monthly_mip_fluxes <- mip_fluxes_modified %>%
    group_by(group, estimate, name, month_start) %>%
    summarise(
      flux_mean = sum(flux)
    ) %>%
    ungroup() %>%
    group_by(estimate, name, month_start) %>%
    summarise(
      flux_lower = min(flux_mean, na.rm = TRUE),
      flux_upper = max(flux_mean, na.rm = TRUE),
      flux_mean = mean(flux_mean, na.rm = TRUE)
    )
} else {
  monthly_mip_fluxes <- NULL
}

scale_colour_estimate <- scale_colour_manual(values = ESTIMATE_COLOURS)
scale_linetype_estimate <- scale_linetype_manual(values = ESTIMATE_LINETYPES)
scale_fill_estimate <- scale_fill_manual(values = ESTIMATE_COLOURS)

region_plots <- lapply(args$region, function(region_i) {
  annual_plot <- ggplot() +
    geom_tile(
      mapping = aes(
        x = '2016',
        y = 0,
        width = 1,
        height = Inf
      ),
      alpha = 0.3,
      fill = '#bbbbbb'
    ) +
    geom_crossbar(
      data = bind_rows(
        annual_mip_fluxes,
        annual_fluxes
      ) %>%
        filter(name == region_i),
      mapping = aes(
        x = factor(year),
        y = flux_mean,
        ymin = flux_lower,
        ymax = flux_upper,
        colour = estimate,
        fill = estimate,
        linetype = estimate
      ),
      alpha = 0.4,
      position = 'dodge'
    ) +
    scale_colour_estimate +
    scale_linetype_estimate +
    scale_fill_estimate +
    guides(
      colour = guide_legend(ncol = legend_n_columns),
      fill = guide_legend(ncol = legend_n_columns),
      linetype = guide_legend(ncol = legend_n_columns)
    ) +
    labs(x = 'Year', y = expression('Flux [PgC '*yr^-1*']'), colour = NULL, fill = NULL, linetype = NULL)

  monthly_data <- bind_rows(
    monthly_fluxes,
    monthly_mip_fluxes
  ) %>%
    filter(name == region_i)

  monthly_plot <- ggplot() +
    geom_rect(
      mapping = aes(
        xmin = as.Date('2016-01-01'),
        xmax = as.Date('2016-12-01'),
        ymin = -Inf,
        ymax = Inf
      ),
      fill = '#bbbbbb',
      alpha = 0.3
    ) +
    geom_ribbon(
      data = monthly_data,
      mapping = aes(
        x = month_start,
        ymin = flux_lower,
        ymax = flux_upper,
        colour = estimate,
        fill = estimate,
        linetype = estimate
      ),
      size = 0.1,
      alpha = 0.15
    ) +
    geom_line(
      data = monthly_data,
      mapping = aes(
        x = month_start,
        y = flux_mean,
        colour = estimate,
        linetype = estimate
      )
    ) +
    scale_x_date(expand = c(0, 0)) +
    scale_colour_estimate +
    scale_linetype_estimate +
    scale_fill_estimate +
    labs(x = 'Month', y = expression('Flux [PgC '*mo^-1*']'), colour = NULL, fill = NULL, linetype = NULL) +
    guides(fill = FALSE, colour = FALSE, linetype = FALSE)

  region_name <- region_i
  if (region_i %in% names(REGION_TITLE)) {
    region_name <- REGION_TITLE[region_i]
  }

  list(
    title = region_name,
    annual = annual_plot,
    monthly = monthly_plot
  )
})

stopifnot(length(region_plots) %in% c(3, 4))

output <- wrap_plots(
  do.call(c, lapply(region_plots, function(x) {
    list(
      wrap_elements(
        grid::textGrob(
          x$title,
          gp = grid::gpar(fontsize = 11, fontface = 'bold')
        ),
        clip = FALSE
      ),
      x$annual,
      x$monthly
    )
  })),
  guides = 'collect',
  design = if (length(region_plots) == 3) '
    AAA
    BCC
    DDD
    EFF
    GGG
    HII
  ' else if (length(region_plots) == 4) '
    AAA
    BCC
    DDD
    EFF
    GGG
    HII
    JJJ
    KLL
  ',
  heights = if (length(region_plots) == 3) {
    rep(c(0.1, 1), 3)
  } else if (length(region_plots) == 4) {
    rep(c(0.2, 1), 4)
  }
) &
  theme(legend.position = 'bottom')

if (small_y_axes) {
  output <- output &
    theme(
      axis.title.y = element_text(size = 8)
    )
}

log_info('Saving')
ggsave(
  args$output,
  plot = output,
  width = DISPLAY_SETTINGS$full_width,
  height = args$height,
  units = 'cm'
)

log_info('Done')
