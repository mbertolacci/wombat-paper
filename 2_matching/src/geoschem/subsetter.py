import glob
import logging
import os
import re

import numpy as np
import xarray

from .grid import GEOS_CHEM_GRID

logger = logging.getLogger(__name__)


def to_iso8601_date(x):
    if len(x) == 8:
        return "{}-{}-{}".format(x[0:4], x[4:6], x[6:8])
    else:
        return "{}-{}-01".format(x[0:4], x[4:6])


def match_to_nearest(haystack, needles):
    """Find the index in the sorted array haystack of the value closest to
    needle."""
    needles = np.atleast_1d(needles)
    indices = np.searchsorted(haystack, needles)
    indices[indices >= haystack.shape[0]] = haystack.shape[0] - 1
    other_indices = indices - 1
    other_closer = np.abs(haystack[other_indices] - needles) < np.abs(
        haystack[indices] - needles
    )
    indices[other_closer] = other_indices[other_closer]
    return indices


def match_to_grid(grid, needles):
    right = grid["centres"] + grid["widths"] / 2
    indices = np.searchsorted(right, np.atleast_1d(needles))
    indices[indices == len(grid["centres"])] = 0
    return indices


def is_same_grid(grid, centres):
    return np.all(grid["centres"] == centres)


def process_history_file(
    filename,
    file_time,
    is_month,
    input_variable_name,
    output_variable_name,
    drop_variable_name,
    observation_ds,
    observation_times,
):
    if is_month:
        observation_times = observation_times.astype("datetime64[M]")

    logger.debug("Loading %s", filename)
    with xarray.open_dataset(filename)[
        ["lat", "lon", "time", input_variable_name]
    ].load() as ds:
        ds = ds.rename({"lat": "model_latitude", "lon": "model_longitude"})
        start_index = np.searchsorted(observation_times, file_time, side="left")
        end_index = np.searchsorted(observation_times, file_time, side="right")
        # NOTE(mgnb): check whether our hardcoded grid specification is the
        # same as the netcdf file
        assert is_same_grid(GEOS_CHEM_GRID["latitude"], ds["model_latitude"].values)
        assert is_same_grid(GEOS_CHEM_GRID["longitude"], ds["model_longitude"].values)
        observation_times = observation_ds["time"][start_index:end_index].values
        time_indices = xarray.DataArray(
            match_to_nearest(ds["time"].values, observation_times), dims="sounding_id"
        )
        observation_lons = observation_ds["longitude"][start_index:end_index].values
        lon_indices = xarray.DataArray(
            match_to_grid(GEOS_CHEM_GRID["longitude"], observation_lons),
            dims="sounding_id",
        )
        observation_lats = observation_ds["latitude"][start_index:end_index].values
        lat_indices = xarray.DataArray(
            match_to_grid(GEOS_CHEM_GRID["latitude"], observation_lats),
            dims="sounding_id",
        )
        variable = ds[input_variable_name][
            time_indices, :, lat_indices, lon_indices
        ].drop_vars([drop_variable_name])
        sounding_id = observation_ds["sounding_id"][start_index:end_index]
        return xarray.Dataset(
            {
                output_variable_name: variable,
                "observation_longitude": observation_ds["longitude"][
                    start_index:end_index
                ],
                "observation_latitude": observation_ds["latitude"][
                    start_index:end_index
                ],
            },
            coords={"sounding_id": sounding_id},
        )


class GEOSChemSubsetter(object):
    def __init__(
        self,
        observation_ds,
        run_directory_species_conc,
        run_directory_level_edge,
        is_subset_hourly,
    ):
        self.observation_ds = observation_ds
        self.observation_days = self.observation_ds["time"].values.astype(
            "datetime64[D]"
        )
        self.run_directory_species_conc = run_directory_species_conc
        self.run_directory_level_edge = run_directory_level_edge
        self.is_subset_hourly = is_subset_hourly

    def match_observations(self):
        logger.debug("Processing SpeciesConc files")
        unique_observation_days = np.unique(self.observation_days)
        output_ds = self._match_history_files(
            self.run_directory_species_conc,
            "GEOSChem.SpeciesConc.*_0000z.nc4"
            if self.is_subset_hourly
            else "GEOSChem.SpeciesConcDaily.*.nc4",
            "SpeciesConc_CO2",
            "vco2",
            "lev",
            unique_observation_days,
        )
        if output_ds is None:
            return None

        logger.debug("Processing LevelEdgeDiags files")
        level_edge_ds = self._match_history_files(
            self.run_directory_level_edge,
            "GEOSChem.LevelEdgeDiags.*_0000z.nc4"
            if self.is_subset_hourly
            else "GEOSChem.LevelEdgeDiagsDaily.*.nc4",
            "Met_PEDGE",
            "pressure_edge",
            "ilev",
            # NOTE(mgnb): pass this instead of self.observation_days here,
            # because level edge comes from base run and so there are many more
            np.unique(output_ds["time"].values),
        )
        output_ds["pressure_edge"] = level_edge_ds["pressure_edge"]

        return output_ds[
            [
                "time",
                "observation_latitude",
                "observation_longitude",
                "model_latitude",
                "model_longitude",
                "vco2",
                "pressure_edge",
            ]
        ]

    def _match_history_files(
        self,
        run_directory,
        pattern,
        input_variable_name,
        output_variable_name,
        drop_variable_name,
        times_to_match,
    ):
        history_files = np.array(
            glob.glob(os.path.join(run_directory, "output", pattern))
        )
        history_files.sort()

        if self.is_subset_hourly:
            history_dates = np.array(
                [
                    to_iso8601_date(
                        re.match(r"^.*(\d{8})_0000z\.nc4$", history_file).group(1)
                    )
                    for history_file in history_files
                ],
                dtype="datetime64[D]",
            )
        else:
            history_dates = np.array(
                [
                    to_iso8601_date(
                        re.match(r"^.*(\d{6})\.nc4$", history_file).group(1)
                    )
                    for history_file in history_files
                ],
                dtype="datetime64[M]",
            )

        _, history_included, times_included = np.intersect1d(
            history_dates,
            times_to_match.astype("datetime64[D]")
            if self.is_subset_hourly
            else times_to_match.astype("datetime64[M]"),
            return_indices=True,
        )

        if len(history_included) == 0:
            return None

        parts = (
            process_history_file(
                filename,
                file_time,
                not self.is_subset_hourly,
                input_variable_name,
                output_variable_name,
                drop_variable_name,
                self.observation_ds,
                self.observation_days,
            )
            for filename, file_time in zip(
                history_files[history_included].tolist(),
                history_dates[history_included].tolist(),
            )
        )

        logger.debug("Concatenating parts")
        return xarray.concat(parts, dim="sounding_id")


__all__ = ["GEOSChemSubsetter", "match_to_grid"]
