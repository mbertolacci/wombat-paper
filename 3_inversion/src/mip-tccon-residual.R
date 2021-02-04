source(Sys.getenv('INVERSION_BASE_PARTIAL'))
library(tidyr, warn.conflicts = FALSE)

args <- arg_parser('', hide.opts = TRUE) %>%
  add_argument('--mip-tccon-directory', '') %>%
  add_argument('--output', '') %>%
  parse_args()

filenames <- list.files(args$mip_tccon_directory, pattern = 'res\\.csv$', full.names = TRUE)

log_info('Processing {length(filenames)} inputs')
output <- bind_rows(lapply(filenames, function(filename) {
  log_debug('Processing {filename}')
  name <- basename(filename)
  parts <- stringr::str_split(name, '_')[[1]]

  df <- readr::read_csv(filename, col_types = 'cdddddddddd') %>%
    mutate(datetime = lubridate::parse_date_time(datstr, 'm/d/20y H:M')) %>%
    select(-datstr)

  df %>%
    pivot_longer(
      -datetime,
      names_to = 'group',
      values_to = 'model'
    ) %>%
    mutate(
      case = parts[1],
      station = parts[2]
    )
}))

log_info('Saving')
fst::write_fst(output, args$output)

log_info('Done')
