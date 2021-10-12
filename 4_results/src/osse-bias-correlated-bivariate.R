source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_DISPLAY_PARTIAL'))
library(argparse)

library(lubridate, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

ESTIMATE_COLOURS <- c(
  'Prior mean' = 'blue',
  'Truth' = 'black',
  'Bias correction/correlated errors' = 'red',
  'No bias correction/correlated errors' = 'purple',
  'Bias correction/uncorrelated errors' = 'orange',
  'No bias correction/uncorrelated errors' = '#00aa00',
  'No bias correction/uncorrelated errors/fixed hyperparameters' = '#004400',
  'True total flux' = '#aaaaaa'
)

ESTIMATE_SHAPES <- c(
  'Prior mean' = 1,
  'Truth' = 2,
  'Bias correction/correlated errors' = 3,
  'No bias correction/correlated errors' = 4,
  'Bias correction/uncorrelated errors' = 5,
  'No bias correction/uncorrelated errors' = 6,
  'No bias correction/uncorrelated errors/fixed hyperparameters' = 7,
  'True total flux' = 0
)

ESTIMATE_LINETYPES <- c(
  'Prior mean' = 1,
  'Truth' = 2,
  'Bias correction/correlated errors' = 3,
  'No bias correction/correlated errors' = 4,
  'Bias correction/uncorrelated errors' = 5,
  'No bias correction/uncorrelated errors' = 6,
  'No bias correction/uncorrelated errors/fixed hyperparameters' = 7,
  'True total flux' = 3
)

PLOT_AXES <- c(
  'Global land' = expression('Global land flux [PgC '*yr^-1*']'),
  'Global oceans' = expression('Global ocean flux [PgC '*yr^-1*']'),
  'T04' = expression('Flux in TransCom3 04: South American Temperate [PgC '*yr^-1*']'),
  'T06' = expression('Flux in TransCom3 06: Southern Africa [PgC '*yr^-1*']'),
  'N tropics (0 - 23.5)' = expression('Flux in northern tropics [PgC '*yr^-1*']'),
  'S tropics (-23.5 - 0)' = expression('Flux in southern tropics [PgC '*yr^-1*']')
)

parser <- ArgumentParser()
parser$add_argument('--region1')
parser$add_argument('--region2')
parser$add_argument('--flux-samples-lg-bias-correlated')
parser$add_argument('--flux-samples-lg-no-bias-correlated')
parser$add_argument('--flux-samples-lg-bias-uncorrelated')
parser$add_argument('--flux-samples-lg-no-bias-uncorrelated')
parser$add_argument('--flux-samples-lg-no-bias-uncorrelated-fixedhyper')
parser$add_argument('--flux-samples-ln-bias-correlated')
parser$add_argument('--flux-samples-ln-no-bias-correlated')
parser$add_argument('--flux-samples-ln-bias-uncorrelated')
parser$add_argument('--flux-samples-ln-no-bias-uncorrelated')
parser$add_argument('--flux-samples-ln-no-bias-uncorrelated-fixedhyper')
parser$add_argument('--height', type = 'double')
parser$add_argument('--output')
args <- parser$parse_args()

read_flux_samples <- function(filename, estimates = 'Posterior') {
  readRDS(filename) %>%
    filter(
      estimate %in% estimates,
      name %in% c(args$region1, args$region2)
    )
}

log_info('Loading flux samples')
flux_samples <- bind_rows(
  read_flux_samples(
    args$flux_samples_ln_bias_correlated,
    c('Truth', 'Prior', 'Posterior')
  ) %>%
    mutate(
      group = 'LN',
      estimate = ifelse(
        estimate == 'Posterior',
        'Bias correction/correlated errors',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_ln_no_bias_correlated,
  ) %>%
    mutate(
      group = 'LN',
      estimate = 'No bias correction/correlated errors'
    ),
  read_flux_samples(
    args$flux_samples_ln_no_bias_uncorrelated,
  ) %>%
    mutate(
      group = 'LN',
      estimate = 'No bias correction/uncorrelated errors'
    ),
  read_flux_samples(
    args$flux_samples_ln_bias_uncorrelated,
  ) %>%
    mutate(
      group = 'LN',
      estimate = 'Bias correction/uncorrelated errors'
    ),
  read_flux_samples(
    args$flux_samples_ln_no_bias_uncorrelated_fixedhyper,
  ) %>%
    mutate(
      group = 'LN',
      estimate = 'No bias correction/uncorrelated errors/fixed hyperparameters'
    ),
  read_flux_samples(
    args$flux_samples_lg_bias_correlated,
    c('Truth', 'Prior', 'Posterior')
  ) %>%
    mutate(
      group = 'LG',
      estimate = ifelse(
        estimate == 'Posterior',
        'Bias correction/correlated errors',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_lg_no_bias_correlated,
  ) %>%
    mutate(
      group = 'LG',
      estimate = 'No bias correction/correlated errors'
    ),
  read_flux_samples(
    args$flux_samples_lg_no_bias_uncorrelated,
  ) %>%
    mutate(
      group = 'LG',
      estimate = 'No bias correction/uncorrelated errors'
    ),
  read_flux_samples(
    args$flux_samples_lg_bias_uncorrelated,
  ) %>%
    mutate(
      group = 'LG',
      estimate = 'Bias correction/uncorrelated errors'
    ),
  read_flux_samples(
    args$flux_samples_lg_no_bias_uncorrelated_fixedhyper,
  ) %>%
    mutate(
      group = 'LG',
      estimate = 'No bias correction/uncorrelated errors/fixed hyperparameters'
    )
) %>%
  mutate(
    estimate = factor(
      ifelse(estimate == 'Prior', 'Prior mean', estimate),
      levels = names(ESTIMATE_COLOURS)
    ),
    year = year(month_start)
  )

name_map <- c('region1', 'region2')
names(name_map) <- c(args$region1, args$region2)

log_info('Calculating')
annual_flux_samples <- flux_samples %>%
  filter(
    year %in% c(2015, 2016),
    estimate != 'Prior mean',
    estimate != 'Truth',
    name != 'Global'
  ) %>%
  mutate(
    name = name_map[name]
  ) %>%
  group_by(group, estimate, name, year) %>%
  summarise(
    flux_samples = t(colSums(flux_samples))
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(group, estimate, year),
    names_from = name,
    values_from = flux_samples
  ) %>%
  group_by(group, estimate, year) %>%
  group_modify(~ data.frame(
    region1 = as.vector(.x$region1),
    region2 = as.vector(.x$region2)
  ))

annual_flux_means <- flux_samples %>%
  filter(
    year %in% c(2015, 2016)
  ) %>%
  mutate(
    name = name_map[name]
  ) %>%
  group_by(group, estimate, name, year) %>%
  summarise(
    flux_mean = sum(flux_mean)
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(group, estimate, year),
    names_from = name,
    values_from = flux_mean
  ) %>%
  mutate(total = region1 + region2)

annual_flux_ranges <- bind_rows(
  annual_flux_samples %>%
    group_by(estimate, year) %>%
    summarise(
      region1 = min(region1),
      region2 = min(region2)
    ),
  annual_flux_samples %>%
    group_by(estimate, year) %>%
    summarise(
      region1 = max(region1),
      region2 = max(region2)
    )
)

output <- ggplot(
  mapping = aes(region1, region2, colour = estimate, linetype = estimate, shape = estimate)
) +
  geom_abline(
    data = annual_flux_means %>%
      filter(
        estimate == 'Truth'
      ),
    mapping = aes(
      intercept = total,
      slope = -1
    ),
    linetype = 3,
    colour = '#777777'
  ) +
  stat_ellipse(
    data = annual_flux_samples # %>% filter(!endsWith(as.character(estimate), 'params'))
  ) +
  geom_point(
    data = annual_flux_means # %>% filter(!endsWith(as.character(estimate), 'params'))
  ) +
  scale_colour_manual(values = ESTIMATE_COLOURS) +
  scale_shape_manual(values = ESTIMATE_SHAPES) +
  scale_linetype_manual(values = ESTIMATE_LINETYPES) +
  facet_grid(group ~ year) +
  guides(
    colour = guide_legend(ncol = 2),
    shape = guide_legend(ncol = 2),
    linetype = guide_legend(ncol = 2)
  ) +
  labs(
    x = PLOT_AXES[args$region1],
    y = PLOT_AXES[args$region2],
    colour = NULL,
    linetype = NULL,
    shape = NULL
  ) +
  coord_fixed() +
  theme(legend.position = 'bottom')

output_grob <- ggplotGrob(output)
# HACK(mgnb): remove the lines for 'Prior Mean' and 'Truth'
blank_grob <- grid::rectGrob(gp = grid::gpar(col = NA))
output_grob$grobs[[22]]$grobs[[1]]$grobs[[4]] <- blank_grob
output_grob$grobs[[22]]$grobs[[1]]$grobs[[7]] <- blank_grob
output_grob$grobs[[22]]$grobs[[1]]$grobs[[26]] <- blank_grob

log_info('Saving')
ggsave_base(
  args$output,
  output_grob,
  width = DISPLAY_SETTINGS$full_width,
  height = args$height
)

log_info('Done')
