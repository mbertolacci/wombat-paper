COLOUR_TABLE <- c(
  'wombat_prior' = 'blue',
  'wombat_lg' = 'orange',
  'wombat_ln' = '#4a716d'
)

get_colour <- function(name) {
  x <- COLOUR_TABLE[name]
  names(x) <- NULL
  x
}

TCCON_CODE_TO_NAME <- c(
  '01-parkfalls' = 'Park Falls',
  '02-lamont' = 'Lamont',
  '03-bialystok' = 'Bialystok',
  '04-orleans' = 'Orleans',
  '05-karlsruhe' = 'Karlsruhe',
  '07-tsukuba125' = 'Tsukuba',
  '09-lauder125' = 'Lauder',
  '10-darwin' = 'Darwin',
  '11-wollongong' = 'Wollongong',
  '12-garmisch' = 'Garmisch',
  '13-bremen' = 'Bremen',
  '14-eureka' = 'Eureka',
  '15-sodankyla' = 'Sodankyla',
  '16-izana' = 'Izana',
  '17-reunion' = 'Reunion',
  '18-ascension' = 'Ascension',
  '20-caltech' = 'Caltech',
  '23-nyalesund' = 'Nyalesund',
  '25-dryden' = 'Dryden',
  '26-saga' = 'Saga',
  '27-rikubetsu' = 'Rikubetsu',
  '28-manaus' = 'Manaus',
  '29-paris' = 'Paris',
  '30-anmyeondo' = 'Anmyeondo'
)

TCCON_ORDER <- c(
  'Lauder',
  'Wollongong',
  'Reunion',
  'Darwin',
  'Ascension',
  'Manaus',
  'Izana',
  'Saga',
  'Caltech',
  'Dryden',
  'Tsukuba',
  'Anmyeondo',
  'Lamont',
  'Rikubetsu',
  'Park Falls',
  'Garmisch',
  'Orleans',
  'Paris',
  'Karlsruhe',
  'Bremen',
  'Bialystok',
  'Sodankyla',
  'Nyalesund',
  'Eureka'
)

MIP_TCCON_CODE_TO_NAME <- c(
  'parkfalls' = 'Park Falls',
  'lamont' = 'Lamont',
  'bialystok' = 'Bialystok',
  'orleans' = 'Orleans',
  'karlsruhe' = 'Karlsruhe',
  'tsukuba' = 'Tsukuba',
  'lauder' = 'Lauder',
  'darwin' = 'Darwin',
  'wollongong' = 'Wollongong',
  'garmisch' = 'Garmisch',
  'bremen' = 'Bremen',
  'eureka' = 'Eureka',
  'sodankyla' = 'Sodankyla',
  'izana' = 'Izana',
  'reunion' = 'Reunion',
  'ascension' = 'Ascension',
  'caltech' = 'Caltech',
  'nyalesund' = 'Nyalesund',
  'dryden' = 'Dryden',
  'saga' = 'Saga',
  'rikubetsu' = 'Rikubetsu',
  'manaus' = 'Manaus',
  'paris' = 'Paris',
  'anmyeondo' = 'Anmyeondo'
)

MIP_TCCON_INCLUDED_STATIONS <- c(
  'Lauder',
  'Wollongong',
  'Reunion',
  'Darwin',
  'Ascension',
  'Manaus',
  'Izana',
  'Saga',
  'Caltech',
  'Dryden',
  'Tsukuba',
  'Lamont',
  'Park Falls',
  'Orleans',
  'Karlsruhe',
  'Bremen',
  'Bialystok',
  'Sodankyla',
  'Eureka'
)

MIP_TCCON_MODEL_NAME_TO_MODEL_NAME <- c(
  'Baker-mean' = 'Baker-mean',
  'CAMS' = 'CAMS',
  'CMS-Flux' = 'CMS-Flux',
  'CSU' = 'SCHUH',
  'CT-NRT' = 'CT-NRT',
  'OU' = 'OU',
  'TM5-4DVAR' = 'TM5-4DVAR',
  'UT' = 'UT',
  'UoE' = 'UoE'
)

MODEL_NAME_ORDER <- c(
  'TM5-4DVAR',
  'CT-NRT',
  'OU',
  'CAMS',
  'Baker-mean',
  'SCHUH',
  'UT',
  'CMS-Flux',
  'UoE',
  'WOMBAT Post.',
  'WOMBAT Prior'
)

REGION_NAME_TO_CODE <- c(
  'North American Boreal' = 'T01',
  'North American Temperate' = 'T02',
  'Tropical South America' = 'T03',
  'South American Temperate' = 'T04',
  'Northern Africa' = 'T05',
  'Southern Africa' = 'T06',
  'Eurasia Boreal' = 'T07',
  'Eurasia Temperate' = 'T08',
  'Tropical Asia' = 'T09',
  'Australia' = 'T10',
  'Europe' = 'T11',
  'North Pacific Temperate' = 'T12',
  'West Pacific Tropical' = 'T13',
  'East Pacific Tropical' = 'T14',
  'South Pacific Temperate' = 'T15',
  'Northern Ocean' = 'T16',
  'North Atlantic Temperate' = 'T17',
  'Atlantic Tropical' = 'T18',
  'South Atlantic Temperate' = 'T19',
  'Southern Ocean' = 'T20',
  'Indian Tropical' = 'T21',
  'South Indian Temperate' = 'T22'
)

REGION_NAMES <- names(REGION_NAME_TO_CODE)
REGION_CODES <- REGION_NAME_TO_CODE

MIP_REGION_TO_REGION <- c(
  'null' = 'Global',
  'null2' = 'Global land',
  'null3' = 'Global oceans',
  'null4' = 'N upper (45 - 90)',
  'null5' = 'N lower (0 - 45)',
  'null6' = 'S upper (-45 - 0)',
  'null7' = 'S lower (-90 - -45)',
  'null8' = 'N extratropics (23.5 - 90)',
  'ZB: N. Tropics' = 'N tropics (0 - 23.5)',
  'ZB: S. Tropics' = 'S tropics (-23.5 - 0)',
  'null9' = 'S extratropics (-90 - -23.5)',
  'ZB: Arctic' = 'N polar (65 - 90)',
  'ZB: N. Midlatitudes' = 'N mid (23.5 - 65)',
  'ZB: S. Midlats' = 'S mid (-65 - -23.5)',
  'ZB: Antarctic' = 'S polar (-90 - -65)',
  'TransCom 01: North American Boreal' = 'T01',
  'TransCom 02: North American Temperate' = 'T02',
  'TransCom 03a: Northern Tropical South America' = 'T03',
  'TransCom 03b: Southern Tropical South America' = 'T03',
  'TransCom 04: South American Temperate' = 'T04',
  'TransCom 05a: Temperate Northern Africa' = 'T05',
  'TransCom 05b: Northern Tropical Africa' = 'T05',
  'TransCom 06a: Southern Tropical Africa' = 'T06',
  'TransCom 06b: Temperate Southern Africa' = 'T06',
  'TransCom 07: Eurasia Boreal' = 'T07',
  'TransCom 08: Eurasia Temperate' = 'T08',
  'TransCom 09a: Northern Tropical Asia' = 'T09',
  'TransCom 09b: Southern Tropical Asia' = 'T09',
  'TransCom 10a: Tropical Australia' = 'T10',
  'TransCom 10b: Temperate Australia' = 'T10',
  'TransCom 11: Europe' = 'T11',
  'TransCom 12: North Pacific Temperate' = 'T12',
  'TransCom 13: West Pacific Tropical' = 'T13',
  'TransCom 14: East Pacific Tropical' = 'T14',
  'TransCom 15: South Pacific Temperate' = 'T15',
  'TransCom 16: Northern Ocean' = 'T16',
  'TransCom 17: North Atlantic Temperate' = 'T17',
  'TransCom 18: Atlantic Tropical' = 'T18',
  'TransCom 19: South Atlantic Temperate' = 'T19',
  'TransCom 20: Southern Ocean' = 'T20',
  'TransCom 21: Indian Tropical' = 'T21',
  'TransCom 22: South Indian Temperate' = 'T22'
)

REGION_TITLE <- c(
  'Global land' = 'Global Land',
  'Global oceans' = 'Global Oceans',
  'N extratropics (23.5 - 90)' = 'N. Extratropics (23.5\u00B0 N - 90\u00B0 N)',
  'N tropics (0 - 23.5)' = 'N. Tropics (0\u00B0 - 23.5\u00B0 N)',
  'S tropics (-23.5 - 0)' = 'S. Tropics (23.5\u00B0 S - 0\u00B0)',
  'S extratropics (-90 - -23.5)' = 'S. Extratropics (90\u00B0 S - 23.5\u00B0 S)',
  'T01' = 'TransCom 01: North American Boreal',
  'T02' = 'TransCom 02: North American Temperate',
  'T03' = 'TransCom 03: Tropical South America',
  'T04' = 'TransCom 04: South American Temperate',
  'T05' = 'TransCom 05: Northern Africa',
  'T06' = 'TransCom 06: Southern Africa',
  'T07' = 'TransCom 07: Eurasia Boreal',
  'T08' = 'TransCom 08: Eurasia Temperate',
  'T09' = 'TransCom 09: Tropical Asia',
  'T10' = 'TransCom 10: Australia',
  'T11' = 'TransCom 11: Europe',
  'T12' = 'TransCom 12: North Pacific Temperate',
  'T13' = 'TransCom 13: West Pacific Tropical',
  'T14' = 'TransCom 14: East Pacific Tropical',
  'T15' = 'TransCom 15: South Pacific Temperate',
  'T16' = 'TransCom 16: Northern Ocean',
  'T17' = 'TransCom 17: North Atlantic Temperate',
  'T18' = 'TransCom 18: Atlantic Tropical',
  'T19' = 'TransCom 19: South Atlantic Temperate',
  'T20' = 'TransCom 20: Southern Ocean',
  'T21' = 'TransCom 21: Indian Tropical',
  'T22' = 'TransCom 22: South Indian Temperate'
)

REGION_AGGREGATES <- list(
  list(
    name = 'Global',
    parts = sprintf('T%02d', 1 : 22),
    types = c('land', 'ocean')
  ),
  list(
    name = 'Global land',
    parts = sprintf('T%02d', 1 : 22),
    types = 'land'
  ),
  list(
    name = 'Global oceans',
    parts = sprintf('T%02d', 1 : 22),
    types = 'ocean'
  ),
  list(
    name = 'N extratropics (23.5 - 90)',
    parts = c('N polar (65 - 90)', 'N mid (23.5 - 65)'),
    types = c('land', 'ocean')
  ),
  list(
    name = 'S extratropics (-90 - -23.5)',
    parts = c('S mid (-65 - -23.5)', 'S polar (-90 - -65)'),
    types = c('land', 'ocean')
  )
)
