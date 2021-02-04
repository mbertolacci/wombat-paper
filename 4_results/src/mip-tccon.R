source(Sys.getenv('RESULTS_BASE_PARTIAL'))
source(Sys.getenv('RESULTS_TABLES_PARTIAL'))
library(argparse)

parser <- ArgumentParser()
parser$add_argument('--mip-tccon-residual')
parser$add_argument('--output')
args <- parser$parse_args()

mip_tccon <- fst::read_fst(args$mip_tccon_residual) %>%
  filter(
    datetime >= '2015-01-01',
    datetime < '2017-05-01',
    case != 'IS',
    !is.na(model)
  ) %>%
  distinct(station, case, group, datetime, .keep_all = TRUE) %>%
  rename(time = datetime) %>%
  mutate(
    group = MIP_TCCON_MODEL_NAME_TO_MODEL_NAME[group],
    station = factor(MIP_TCCON_CODE_TO_NAME[station], TCCON_ORDER),
    observed = 0
  ) %>%
  filter(
    station %in% MIP_TCCON_INCLUDED_STATIONS
  )

fst::write_fst(mip_tccon, args$output)
