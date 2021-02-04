source(Sys.getenv('INVERSION_BASE_PARTIAL'))
library(ncdf4)
library(tidyr, warn.conflicts = FALSE)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--mip-fluxes-directory', '') %>%
  add_argument('--output', '') %>%
  parse_args()

add_group <- function(x, name) {
  lapply(x, function(y) { y$group <- name; y })
}

inputs <- c(
  add_group(list(
    list(
      filename = 'L4_FLUX_SCHUH_IS_20180102.nc4',
      case = 'IS'
    ),
    list(
      filename = 'L4_FLUX_SCHUH_IS_LN_20171120.nc4',
      case = 'IS_LN'
    ),
    list(
      filename = 'L4_FLUX_SCHUH_LG_20171120.nc4',
      case = 'LG'
    ),
    list(
      filename = 'L4_FLUX_SCHUH_LN_20171120.nc4',
      case = 'LN'
    ),
    list(
      filename = 'L4_FLUX_SCHUH_PRIOR_20171120.nc4',
      case = 'Prior'
    )
  ), 'Schuh'),
  add_group(list(
    list(
      filename = 'L4_FLUX_Chevallier_LMDZ_FixedOceans_IS_20171016.nc',
      case = 'IS'
    ),
    list(
      filename = 'L4_FLUX_Chevallier_LMDZ_FixedOceans_LG_20171016.nc',
      case = 'LG'
    ),
    list(
      filename = 'L4_FLUX_Chevallier_LMDZ_FixedOceans_LN_20171016.nc',
      case = 'LN'
    ),
    list(
      filename = 'L4_FLUX_Chevallier_LMDZ_FixedOceans_LNi_20171016.nc',
      case = 'IS_LN'
    ),
    list(
      filename = 'L4_FLUX_Chevallier_LMDZ_FixedOceans_prior_20171016.nc',
      case = 'Prior'
    )
  ), 'Chevallier'),
  add_group(list(
    list(
      filename = 'L4_FLUX_CMS-Flux_IS_20171019.nc4',
      case = 'IS'
    ),
    list(
      filename = 'L4_FLUX_CMS-Flux_LG_20171019.nc4',
      case = 'LG'
    ),
    list(
      filename = 'L4_FLUX_CMS-Flux_LN_20171019.nc4',
      case = 'LN'
    ),
    list(
      filename = 'L4_FLUX_CMS-Flux_LNi_20171019.nc4',
      case = 'IS_LN'
    ),
    list(
      filename = 'L4_FLUX_CMS-Flux_unopt_20171019.nc4',
      case = 'Prior'
    )
  ), 'CMS'),
  add_group(list(
    list(
      filename = 'L4_flux_CT-NRT.v2017_IS_20180730.nc4',
      case = 'IS'
    ),
    list(
      filename = 'L4_flux_CT-NRT.v2017_LG_20180730.nc4',
      case = 'LG'
    ),
    list(
      filename = 'L4_flux_CT-NRT.v2017_LN_20180730.nc4',
      case = 'LN'
    ),
    list(
      filename = 'L4_flux_CT-NRT.v2017_LNi_20180730.nc4',
      case = 'IS_LN'
    ),
    list(
      filename = 'L4_flux_CT-NRT.v2017_unopt_20171019.nc4',
      case = 'Prior'
    )
  ), 'CT'),
  add_group(list(
    list(
      filename = 'L4_flux_Deng_GEOSCHEM_GL_20180203gCperday.nc',
      case = 'LG'
    ),
    # Differs from the other in the fossil fluxes
    list(
      filename = 'L4_flux_Deng_GEOSCHEM_INS_20170918gCperday.nc',
      case = 'IS'
    ),
    list(
      filename = 'L4_flux_Deng_GEOSCHEM_NL_20180203gCperday.nc',
      case = 'LN'
    ),
    list(
      filename = 'L4_flux_Deng_GEOSCHEM_NLi_20180203gCperday.nc',
      case = 'IS_LN'
    ),
    # Differs from the other in the fossil fluxes. Has land fluxes in ocean transcoms???
    list(
      filename = 'L4_flux_Deng_GEOSCHEM_prior_20170918gCperday.nc',
      case = 'Prior'
    )
  ), 'Deng'),
  add_group(list(
    list(
      filename = 'L4_flux_OU_IS_20171120.nc4',
      case = 'IS'
    ),
    list(
      filename = 'L4_flux_OU_LG_20171116.nc4',
      case = 'LG'
    ),
    list(
      filename = 'L4_flux_OU_LN_20171107.nc4',
      case = 'LN'
    ),
    list(
      filename = 'L4_flux_OU_LNi_20171120.nc4',
      case = 'IS_LN'
    ),
    list(
      filename = 'L4_flux_OU_Prior_20171107.nc4',
      case = 'Prior'
    )
  ), 'OU'),
  add_group(list(
    list(
      filename = 'L4_FLUX_TM54DVAR-NOAA_IS_20180308.nc4',
      case = 'IS'
    ),
    list(
      filename = 'L4_FLUX_TM54DVAR-NOAA_LG_20180308.nc4',
      case = 'LG'
    ),
    list(
      filename = 'L4_FLUX_TM54DVAR-NOAA_LN_20180308.nc4',
      case = 'LN'
    ),
    list(
      filename = 'L4_FLUX_TM54DVAR-NOAA_LNi_20180308.nc4',
      case = 'IS_LN'
    ),
    list(
      filename = 'L4_FLUX_TM54DVAR-NOAA_OG_20180308.nc4',
      case = 'OG'
    ),
    list(
      filename = 'L4_FLUX_TM54DVAR-NOAA_PRIOR_20180308.nc4',
      case = 'Prior'
    )
  ), 'TM54DVAR'),
  add_group(list(
    list(
      filename = 'L4_FLUX_Baker_PCTM_Taka_IS_20171016.nc',
      case = 'IS'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Taka_LG_20171016.nc',
      case = 'LG'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Taka_LN_20171016.nc',
      case = 'LN'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Taka_LNi_20171016.nc',
      case = 'LNi'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Taka_prior_20171016.nc',
      case = 'Prior'
    )
  ), 'Baker_Taka'),
  add_group(list(
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldsh_IS_20171016.nc',
      case = 'IS'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldsh_LG_20171016.nc',
      case = 'LG'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldsh_LN_20171016.nc',
      case = 'LN'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldsh_LNi_20171016.nc',
      case = 'LNi'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldsh_prior_20171016.nc',
      case = 'Prior'
    )
  ), 'Baker_Ldsh'),
  add_group(list(
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldwo_IS_20171016.nc',
      case = 'IS'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldwo_LG_20171016.nc',
      case = 'LG'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldwo_LN_20171016.nc',
      case = 'LN'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldwo_LNi_20171016.nc',
      case = 'LNi'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_Ldwo_prior_20171016.nc',
      case = 'Prior'
    )
  ), 'Baker_Ldwo'),
  add_group(list(
    list(
      filename = 'L4_FLUX_Baker_PCTM_NOBM_IS_20171016.nc',
      case = 'IS'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_NOBM_LG_20171016.nc',
      case = 'LG'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_NOBM_LN_20171016.nc',
      case = 'LN'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_NOBM_LNi_20171016.nc',
      case = 'LNi'
    ),
    list(
      filename = 'L4_FLUX_Baker_PCTM_NOBM_prior_20171016.nc',
      case = 'Prior'
    )
  ), 'Baker_NOBM'),
  add_group(list(
    list(
      filename = 'L4_FLUX_UoE_APR_20170908.nc',
      case = 'Prior'
    ),
    list(
      filename = 'L4_FLUX_UoE_is_20171020.nc',
      case = 'IS'
    ),
    list(
      filename = 'L4_FLUX_UoE_lg_20171020.nc',
      case = 'LG'
    ),
    list(
      filename = 'L4_FLUX_UoE_ln_20170908.nc',
      case = 'LN'
    ),
    list(
      filename = 'L4_FLUX_UoE_ln_is_20170908.nc',
      case = 'IS_LN'
    ),
    list(
      filename = 'L4_FLUX_UoE_ln_og_20170908.nc',
      case = 'LN_OG'
    ),
    list(
      filename = 'L4_FLUX_UoE_og_20170908.nc',
      case = 'OG'
    )
  ), 'UoE')
)

REGION_NAMES <- c(
  'TransCom 01: North American Boreal',
  'TransCom 02: North American Temperate',
  'TransCom 03a: Northern Tropical South America',
  'TransCom 03b: Southern Tropical South America',
  'TransCom 04: South American Temperate',
  'TransCom 05a: Temperate Northern Africa',
  'TransCom 05b: Northern Tropical Africa',
  'TransCom 06a: Southern Tropical Africa',
  'TransCom 06b: Temperate Southern Africa',
  'TransCom 07: Eurasia Boreal',
  'TransCom 08: Eurasia Temperate',
  'TransCom 09a: Northern Tropical Asia',
  'TransCom 09b: Southern Tropical Asia',
  'TransCom 10a: Tropical Australia',
  'TransCom 10b: Temperate Australia',
  'TransCom 11: Europe',
  'TransCom 12: North Pacific Temperate',
  'TransCom 13: West Pacific Tropical',
  'TransCom 14: East Pacific Tropical',
  'TransCom 15: South Pacific Temperate',
  'TransCom 16: Northern Ocean',
  'TransCom 17: North Atlantic Temperate',
  'TransCom 18: Atlantic Tropical',
  'TransCom 19: South Atlantic Temperate',
  'TransCom 20: Southern Ocean',
  'TransCom 21: Indian Tropical',
  'TransCom 22: South Indian Temperate',
  'TransCom 23: Not optimized',
  'ZB: Arctic',
  'ZB: N. Midlatitudes',
  'ZB: N. Tropics',
  'ZB: S. Tropics',
  'ZB: S. Midlats',
  'ZB: Antarctic',
  'Nino3.4',
  'Amazonia',
  'East Asia',
  'South Asia'
)

REGION_AREAS <- c(
  10100736524288,
  11463987036160,
  3997169614848,
  5621277523968,
  9265082269696,
  5917827923968,
  15535362277376,
  8181677293568,
  1683802816512,
  13640157626368,
  26180661018624,
  4407305437184,
  2067728826368,
  3259532312576,
  4824639209472,
  11529232580608,
  44927010471936,
  31425363443712,
  34821266997248,
  38768220307456,
  21448483667968,
  22415962800128,
  24232884961280,
  19227738112000,
  60447780765696,
  31309183320064,
  27381496545280,
  15982897659904,
  20273824792576,
  131027448102912,
  103730959613952,
  103730959613952,
  131027456491520,
  20273824792576,
  6174311710720,
  7576393089024,
  12188755427328,
  5030716899328
)

log_info('Reading fluxes')
output <- bind_rows(lapply(inputs, function(input) {
  with_nc_file(list(
    flux_file = file.path(args$mip_fluxes_directory, input$filename)
  ), {
    v <- function(name) ncvar_get(flux_file, name)

    n_regions <- flux_file$dim$n_regions$len
    n_months <- flux_file$dim$n_months$len

    stopifnot(n_regions == 38)

    base_date <- ymd(sprintf(
      '%04d-%02d-%02d',
      v('start_date')[1, ],
      v('start_date')[2, ],
      v('start_date')[3, ]
    ))

    has_fossil <- 'fossil' %in% names(flux_file$var)

    dimensions <- sapply(flux_file$var$land$dim, getElement, 'name')

    tibble(
      month_start = if (dimensions[1] == 'n_regions') {
        rep(base_date, each = n_regions)
      } else {
        rep(base_date, n_regions)
      },
      region_name = if (dimensions[1] == 'n_regions') {
        rep(REGION_NAMES, n_months)
      } else {
        rep(REGION_NAMES, each = n_months)
      },
      region_area = if (dimensions[1] == 'n_regions') {
        rep(REGION_AREAS, n_months)
      } else {
        rep(REGION_AREAS, each = n_months)
      },
      fossil = if (has_fossil) as.vector(v('fossil')) else as.numeric(NA),
      land = as.vector(v('land')),
      ocean = as.vector(v('ocean'))
    ) %>%
      mutate(
        group = input$group,
        case = input$case
      )
  })
}))

schuh_fossil_prior <- output %>%
  filter(
    group == 'Schuh',
    case == 'Prior'
  )

output <- output %>%
  select(-fossil) %>%
  left_join(
    schuh_fossil_prior %>% select(month_start, region_name, fossil),
    by = c('month_start', 'region_name')
  ) %>%
  pivot_longer(
    -c(group, case, month_start, region_name, region_area),
    names_to = 'type',
    values_to = 'flux'
  ) %>%
  mutate(
    # gC/d to PgC
    flux = ifelse(
      flux > 1e36, NA, flux * lubridate::days_in_month(month_start) * 10 ^ (-15)
    )
  )

# Compute Baker_Mean
output <- output %>%
  filter(!startsWith(group, 'Baker')) %>%
  bind_rows(
    output %>%
      filter(startsWith(group, 'Baker')) %>%
      group_by(month_start, region_name, region_area, case, type) %>%
      summarise(
        flux = mean(flux)
      ) %>%
      mutate(group = 'Baker_Mean')
  )

log_info('Saving')
fst::write_fst(output, args$output)

log_info('Done')
