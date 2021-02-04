import argparse
import concurrent.futures
import datetime
import glob
import logging
import os

import numpy as np
import pandas as pd
import xarray

from .subsetter import GEOSChemSubsetter


logger = logging.getLogger(__name__)


def process_observation_file(observation_filename):
    '''Convert the input file into a compatible dataset'''
    logger.debug('Loading %s', observation_filename)
    with xarray.open_dataset(observation_filename, group='CO2') as ds:
        ds = ds.load()
        df = pd.DataFrame({
            'year': ds['cdate'].values[:, 0],
            'month': ds['cdate'].values[:, 1],
            'day': ds['cdate'].values[:, 2],
            'hour': ds['cdate'].values[:, 3],
            'minute': ds['cdate'].values[:, 4],
            'second': ds['cdate'].values[:, 5]
        })
        ds['time'] = ('n_obs', xarray.DataArray(pd.to_datetime(df)))

        return xarray.Dataset({
            'time': ('sounding_id', ds['time'].values),
            'longitude': ('sounding_id', ds['longitude'].values),
            'latitude': ('sounding_id', ds['latitude'].values)
        }, {
            'sounding_id': ds['obs_num'].values
        })


def read_tccon_observations(observation_directory):
    observation_filenames = sorted(glob.glob(os.path.join(
        observation_directory,
        '*.nc4'
    )))

    logger.debug(
        'Reading and concatenating %d observation files',
        len(observation_filenames)
    )
    datasets = (
        process_observation_file(filename)
        for filename in observation_filenames
    )
    observation_ds = xarray.concat(datasets, dim='sounding_id') \
        .sortby('time')

    return observation_ds


def subset_for_tccon(
    observation_ds,
    run_directory_species_conc,
    run_directory_level_edge,
    output_path,
    attributes,
    is_subset_hourly
):
    matched_ds = GEOSChemSubsetter(
        observation_ds,
        run_directory_species_conc,
        run_directory_level_edge,
        is_subset_hourly
    ).match_observations()

    if matched_ds is None:
        logger.debug('Nothing to subset')
        return

    matched_ds.attrs = attributes

    logger.debug('Saving results to %s', output_path)
    compression = dict(zlib=True, complevel=6)
    encoding = { var: compression for var in matched_ds.data_vars }
    matched_ds.to_netcdf(output_path)

    logger.debug('Done')


__all__ = ['subset_for_tccon', 'read_tccon_observations']
