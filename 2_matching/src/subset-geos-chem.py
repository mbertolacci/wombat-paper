import argparse
import concurrent.futures
import logging
import os
import pathlib
import xarray

from geoschem.obspack import subset_for_obspack
from geoschem.oco2 import subset_for_oco2
from geoschem.tccon import subset_for_tccon, read_tccon_observations
from geoschem.utils import get_runs, get_run_attributes

logging.basicConfig(format="[%(levelname)s] [%(asctime)s] %(message)s", level=logging.INFO)

parser = argparse.ArgumentParser(
    description="""
Subset OCO-2, TCCON and ObsPack observations to a full set of runs of
GEOS-Chem.
"""
)

# Positional arguments
parser.add_argument(
    "--runs-directory",
    type=str,
    help=("Base directory of runs (typically 1_transport/intermediates/GEOS_Chem/runs)"),
    required=True,
)
parser.add_argument(
    "--meteorology-run", type=str, help="Path to meteorology run", required=True
)
parser.add_argument(
    "--oco2-observations", type=str, help="OCO2 observation NetCDF file", required=True
)
parser.add_argument(
    "--tccon-observation-directory",
    type=str,
    help="TCCON observation directory",
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


tccon_observations = read_tccon_observations(args.tccon_observation_directory)


def get_run_full_path(run_path):
    return os.path.join(args.runs_directory, run_path)


def should_run(path):
    if not args.skip_existing:
        return True
    return not os.path.exists(path)


def subset_run(run):
    def log_info(fmt, *args, **kwargs):
        logging.info("[%s] " + fmt, run["short_path"], *args, **kwargs)

    base_output_path = os.path.join(args.output_directory, run["short_path"])
    log_info("Creating output path")
    pathlib.Path(base_output_path).mkdir(parents=True, exist_ok=True)

    run_attributes = get_run_attributes(run)

    output_path = os.path.join(base_output_path, "subsetted-oco2-hourly.nc")
    if should_run(output_path):
        log_info("Subsetting for hourly OCO-2")
        subset_for_oco2(
            args.oco2_observations,
            run["full_path"],
            base_run_full_path,
            output_path,
            run_attributes,
            True,
        )
    else:
        log_info("Skipping OCO-2 hourly subsetting")

    output_path = os.path.join(base_output_path, "subsetted-oco2-daily.nc")
    if should_run(output_path):
        log_info("Subsetting for daily OCO-2")
        subset_for_oco2(
            args.oco2_observations,
            run["full_path"],
            base_run_full_path,
            output_path,
            run_attributes,
            False,
        )
    else:
        log_info("Skipping OCO-2 daily subsetting")

    output_path = os.path.join(base_output_path, "subsetted-tccon-hourly.nc")
    if should_run(output_path):
        log_info("Subsetting for hourly TCCON")
        subset_for_tccon(
            tccon_observations,
            run["full_path"],
            base_run_full_path,
            output_path,
            run_attributes,
            True,
        )
    else:
        log_info("Skipping TCCON hourly subsetting")

    output_path = os.path.join(base_output_path, "subsetted-tccon-daily.nc")
    if should_run(output_path):
        log_info("Subsetting for daily TCCON")
        subset_for_tccon(
            tccon_observations,
            run["full_path"],
            base_run_full_path,
            output_path,
            run_attributes,
            False,
        )
    else:
        log_info("Skipping TCCON daily subsetting")

    output_path = os.path.join(base_output_path, "subsetted-obspack.nc")
    if should_run(output_path):
        log_info("Subsetting for ObsPack")
        subset_for_obspack(run["full_path"], output_path, run_attributes)
    else:
        log_info("Skipping ObsPack subsetting")

    return None


runs = get_runs(args.runs_directory)
base_run_full_path = args.meteorology_run
logging.info("Running with %d workers", args.max_workers)
with concurrent.futures.ProcessPoolExecutor(max_workers=args.max_workers) as executor:
    for x in executor.map(subset_run, runs):
        # HACK(mgnb): done to raise exceptions
        pass
