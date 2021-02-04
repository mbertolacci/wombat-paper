import logging

import numpy as np
import xarray

from .subsetter import GEOSChemSubsetter


logger = logging.getLogger(__name__)


def subset_for_oco2(
    observations_path,
    run_directory_species_conc,
    run_directory_level_edge,
    output_path,
    attributes,
    is_subset_hourly
):
    logger.debug('Reading observation file %s', observations_path)
    observation_ds = xarray.open_dataset(
        observations_path,
        decode_times=False
    ) \
        .load() \
        .sortby('time')

    # HACK(mgnb): hardcoding the unit rather than reading it from the file
    assert(
        observation_ds['time'].attrs['units']
        == 'seconds since 1970-01-01 00:00:00'
    )
    observation_ds['time'] = ('sounding_id', (
        np.datetime64('1970-01-01 00:00:00') +
        observation_ds['time'].values.astype('timedelta64[s]')
    ))

    valid_times = np.argwhere(np.logical_not(np.isnat(
        observation_ds['time'].values
    )))
    observation_ds = observation_ds \
        .isel(sounding_id=valid_times[:, 0])

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
    matched_ds.to_netcdf(output_path, encoding=encoding)

    logger.debug('Done')


__all__ = ['subset_for_oco2']
