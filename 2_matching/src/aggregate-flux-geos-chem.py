import argparse
import concurrent.futures
import logging
import os
import pathlib
import xarray

from geoschem.grid import GEOS_CHEM_GRID
from geoschem.utils import get_runs, get_run_attributes

logging.basicConfig(format="[%(levelname)s] [%(asctime)s] %(message)s", level=logging.INFO)

parser = argparse.ArgumentParser(
    description="""
Aggregate GEOS-Chem fluxes to monthly.
"""
)

# Positional arguments
parser.add_argument(
    "--runs-directory",
    type=str,
    help=("Base directory of runs (typically 1_transport/intermediates/" "GEOS_Chem/runs)"),
    required=True,
)
parser.add_argument("--output-directory", type=str, help="Output directory", required=True)
max_workers_default = os.environ.get("WOMBAT_MAX_WORKERS", None)
parser.add_argument(
    "--max-workers",
    type=int,
    help="Number of workers",
    default=None if max_workers_default is None else int(max_workers_default),
)
skip_existing_default = os.environ.get("SKIP_EXISTING", None)
parser.add_argument(
    "--skip-existing",
    default=skip_existing_default is not None,
    help="Skip existing files",
    action="store_true",
)

args = parser.parse_args()


def get_run_full_path(run_path):
    return os.path.join(args.runs_directory, run_path)


def aggregate_run(run):
    def log_info(fmt, *args, **kwargs):
        logging.info("[%s] " + fmt, run["short_path"], *args, **kwargs)

    base_output_path = os.path.join(args.output_directory, run["short_path"])
    log_info("Creating output path")
    pathlib.Path(base_output_path).mkdir(parents=True, exist_ok=True)
    output_path = os.path.join(base_output_path, "monthly-fluxes.nc")
    if os.path.exists(output_path) and args.skip_existing:
        log_info("Skipping because %s exists", output_path)
        return

    emission_files = os.path.join(
        os.path.join(run["full_path"], "output"), "HEMCO_diagnostics.*.nc"
    )

    log_info("Loading %d files", len(emission_files))
    ds = xarray.open_mfdataset(
        emission_files,
        combine="by_coords",
        data_vars="minimal",
        coords="minimal",
        compat="override",
        preprocess=lambda ds0: ds0[
            [
                "time",
                "AREA",
                "EmisCO2_Total",
                "EmisCO2_Biomass",
                "EmisCO2_Biofuel",
                "EmisCO2_BalBiosph",
                "EmisCO2_Ocean",
                "EmisCO2_FossilFuel",
                "EmisCO2_Ship",
            ]
        ],
    ).rename({"AREA": "area", "lat": "latitude", "lon": "longitude"})

    log_info("Computing monthly fluxes")
    ds["flux_density_total"] = ds["EmisCO2_Total"].sum("lev")
    ds["flux_density_biomass"] = ds["EmisCO2_Biomass"]
    ds["flux_density_biofuel"] = ds["EmisCO2_Biofuel"]
    ds["flux_density_bal_biosph"] = ds["EmisCO2_BalBiosph"]
    ds["flux_density_ocean"] = ds["EmisCO2_Ocean"]
    ds["flux_density_fossil_fuel"] = ds["EmisCO2_FossilFuel"]
    ds["flux_density_ship"] = ds["EmisCO2_Ship"]
    ds["month_start"] = ds["time"].astype("datetime64[M]")
    grouped_by_ds = ds.drop_vars(
        [
            "area",
            "EmisCO2_Total",
            "EmisCO2_Biomass",
            "EmisCO2_Biofuel",
            "EmisCO2_BalBiosph",
            "EmisCO2_Ocean",
            "EmisCO2_FossilFuel",
            "EmisCO2_Ship",
        ]
    ).groupby("month_start")

    output = grouped_by_ds.mean("time")
    output["duration"] = (
        60
        * 60
        * grouped_by_ds.count()["flux_density_total"][:, 0, 0].drop_vars(
            ["latitude", "longitude"]
        )
    )
    output["duration"].attrs["units"] = "seconds"
    for name in [
        "flux_density_total",
        "flux_density_biomass",
        "flux_density_biofuel",
        "flux_density_bal_biosph",
        "flux_density_ocean",
        "flux_density_fossil_fuel",
        "flux_density_ship",
    ]:
        output[name].attrs["units"] = ds["EmisCO2_Total"].attrs["units"]
    output["area"] = ds["area"]
    output.attrs = get_run_attributes(run)

    output["longitude_width"] = GEOS_CHEM_GRID["longitude"]["widths"]
    output["latitude_height"] = GEOS_CHEM_GRID["latitude"]["widths"]

    log_info("Saving results")
    output.to_netcdf(output_path)

    log_info("Done")

    return None


runs = reversed(get_runs(args.runs_directory))

with concurrent.futures.ProcessPoolExecutor(max_workers=args.max_workers) as executor:
    for x in executor.map(aggregate_run, runs):
        # HACK(mgnb): done to raise exceptions
        pass
