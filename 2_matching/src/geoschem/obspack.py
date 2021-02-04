import concurrent.futures
import glob
import logging
import os

import numpy as np
import pandas as pd
import xarray

from .subsetter import GEOS_CHEM_GRID, match_to_grid


logger = logging.getLogger(__name__)


def process_obspack_file(obspack_filename):
    logger.debug('Loading %s', obspack_filename)
    with xarray.open_dataset(obspack_filename) as ds:
        return ds[[
            'obspack_id',
            'averaging_interval',
            'averaging_interval_start',
            'lat',
            'lon',
            'nsamples',
            'CO2'
        ]].load()


def subset_for_obspack(
    run_directory,
    output_path,
    attributes
):
    obspack_paths = glob.glob(os.path.join(
        run_directory,
        'output',
        'GEOSChem.ObsPack.*.nc4'
    ))
    if len(obspack_paths) == 0:
        return

    logger.debug('Reading and concatenating %d files', len(obspack_paths))
    datasets = (
        process_obspack_file(path)
        for path in obspack_paths
    )
    # HACK(mgnb): convert to pandas DataFrame because groupby is much, much
    # faster
    all_data_df = xarray.concat(datasets, dim='obs').to_dataframe()

    # The same ObsPack observation can span multiple files because the
    # averaging interval may pass midnight, so we need to recombine the samples
    logger.debug('Averaging samples')
    all_data_df['CO2_sum'] = all_data_df['CO2'] * all_data_df['nsamples']
    co2_df = all_data_df[['obspack_id', 'nsamples', 'CO2_sum']] \
        .groupby('obspack_id') \
        .sum()
    co2_df['CO2'] = co2_df['CO2_sum'] / co2_df['nsamples']
    # Grab the metadata. The min() aggregation is just to eliminate duplicates
    other_df = all_data_df[[
        'obspack_id',
        'averaging_interval_start',
        'averaging_interval',
        'lat',
        'lon'
    ]] \
        .groupby('obspack_id') \
        .min()

    output = pd.merge(co2_df, other_df, on='obspack_id')

    # Add the time and location. Note that the time is the centre of the
    # averaging period (i.e., the 'actual' observation time), while the
    # location is the model grid cell centre
    output['time'] = (
        output['averaging_interval_start'] + output['averaging_interval'] / 2
    )
    output['observation_latitude'] = output['lat']
    output['model_latitude'] = GEOS_CHEM_GRID['latitude']['centres'][
        match_to_grid(GEOS_CHEM_GRID['latitude'], output['lat'])
    ]
    output['observation_longitude'] = output['lon']
    output['model_longitude'] = GEOS_CHEM_GRID['longitude']['centres'][
        match_to_grid(GEOS_CHEM_GRID['longitude'], output['lon'])
    ]

    output = xarray.Dataset.from_dataframe(output)
    output.attrs = attributes

    # NOTE(mgnb): GEOS-Chem pads each string with spaces; remove
    output['obspack_id'] = np.char.strip(output['obspack_id'].astype('str'))

    logger.debug('Saving results to %s', output_path)
    output = output[[
        'obspack_id',
        'time',
        'observation_latitude',
        'observation_longitude',
        'model_latitude',
        'model_longitude',
        'CO2'
    ]]
    compression = dict(zlib=True, complevel=6)
    encoding = { var: compression for var in output.data_vars }
    output.to_netcdf(output_path, encoding=encoding)

    logger.debug('Done')


__all__ = ['subset_for_obspack']
