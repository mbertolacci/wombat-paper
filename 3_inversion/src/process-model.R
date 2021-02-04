source(Sys.getenv('INVERSION_BASE_PARTIAL'))

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--control-emissions', '') %>%
  add_argument('--control-mole-fraction', '') %>%
  add_argument('--perturbations', '') %>%
  add_argument('--sensitivities', '') %>%
  add_argument('--output', '') %>%
  parse_args()

log_info('Loading control emissions')
control_emissions <- fst::read_fst(args$control_emissions)

log_info('Loading perturbations')
perturbations <- fst::read_fst(args$perturbations)

log_info('Loading control mole fraction')
control_mole_fraction <- fst::read_fst(args$control_mole_fraction) %>%
  filter(resolution != 'daily')

log_info('Loading sensitivities')
# Implicitly prefers hourly to daily sensitivities where available, because
# they come first in the data.frame
sensitivities <- fst::read_fst(args$sensitivities) %>%
  distinct(region, from_month_start, model_id, .keep_all = TRUE)

log_info('Constructing process model')
process_model <- flux_process_model(
  control_emissions,
  control_mole_fraction,
  perturbations,
  sensitivities,
  lag = Inf,
  Psi = matrix(0, nrow = nrow(control_mole_fraction), ncol = 0),
  eta_prior_variance = 5 ^ 2
)

log_info('Saving')
saveRDS_gz1(process_model, args$output)

log_info('Done')
