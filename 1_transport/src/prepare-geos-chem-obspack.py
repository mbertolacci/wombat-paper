import argparse
import concurrent.futures
import datetime
import glob
import itertools
import logging
import re
import os

import xarray


logging.basicConfig(
    format='[%(levelname)s] [%(asctime)s] %(message)s',
    level=logging.INFO
)


parser = argparse.ArgumentParser(description='''
Prepare ObsPack files for GEOS-CHEM
''')

# Positional arguments
parser.add_argument(
    '--obspack',
    type=str,
    help='Folder containing ObsPack .nc files',
    required=True,
    nargs='+'
)
parser.add_argument(
    '--start-date',
    type=str,
    help='Start date in ISO8601 format, inclusive',
    required=True
)
parser.add_argument(
    '--end-date',
    type=str,
    help='Start date in ISO8601 format, exclusive',
    required=True
)
parser.add_argument(
    '--output-directory',
    type=str,
    help='Output directory',
    required=True
)
parser.add_argument(
    '--max-workers',
    type=int,
    help='Number of workers',
    default=None
)

args = parser.parse_args()

start_date = datetime.datetime.strptime(args.start_date, '%Y-%m-%d').date()
end_date = datetime.datetime.strptime(args.end_date, '%Y-%m-%d').date()


def process_obspack_nc(path):
    '''Take an ObsPack input file, and
    1) Drop unnecessary variables
    2) Rename the sampling_strategy variable to CT_sampling_strategy
    3) Fix the type of the obspack_id variables
    4) Write to obspack-YYYYMMDD.nc.
    Files if skipped if it's outside the date range.'''
    logging.info('Processing %s', path)
    filename = os.path.basename(path)
    result = re.match(r'^flask_input\.(\d{8})_\d{8}\.nc$', filename)
    date_string = result.group(1)
    file_date = datetime.datetime.strptime(date_string, '%Y%m%d').date()
    if file_date < start_date or file_date >= end_date:
        logging.debug('Skipping as outside date range')
        return

    output_path = os.path.join(
        args.output_directory,
        'obspack-{}.nc'.format(date_string)
    )
    with xarray.open_dataset(path) as ds:
        ds['obspack_id'] = ds['obspack_id'].astype('S200')

        logging.debug('Saving to %s', output_path)
        ds[[
            'obspack_id',
            'time_components',
            'latitude',
            'longitude',
            'altitude',
            'sampling_strategy'
        ]].rename({
            'sampling_strategy': 'CT_sampling_strategy'
        }).to_netcdf(output_path)


paths = list(itertools.chain.from_iterable([
    [path] if os.path.isfile(path)
    else glob.glob(os.path.join(path, '*.nc'))
    for path in args.obspack
]))
logging.info('Processing %d input files', len(paths))
with concurrent.futures.ProcessPoolExecutor(
    max_workers=args.max_workers
) as executor:
    executor.map(process_obspack_nc, paths)

logging.info('Done')
