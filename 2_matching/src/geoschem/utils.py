import datetime
import os
import re
import socket
import subprocess


def get_run_attributes(run):
    """Gives the dictionary of attributes to associate with an output file"""
    match = re.match(r"(\d\d\d\d)?\/?run\.v(\d+\.\d+\.\d+)\.(\w+)", run["short_path"])

    git_commit = (
        subprocess.check_output(["git", "rev-parse", "HEAD"]).decode("utf-8").strip()
    )
    if match.group(1) is None:
        if match.group(3) == "base":
            run_type = "base"
        else:
            run_type = "other"
    else:
        run_type = "sensitivity"
    run_attributes = {
        "run_type": run_type,
        "geoschem_version": match.group(2),
        "git_commit": git_commit,
        "date_created": datetime.date.today().isoformat(),
        "hostname": socket.gethostname(),
    }
    if run_attributes["run_type"] == "sensitivity":
        run_attributes["year"] = int(match.group(1))

        match2 = re.match(r"R(\d+)M(\d+)", match.group(3))
        run_attributes["region"] = int(match2.group(1))
        run_attributes["month"] = int(match2.group(2))

    return run_attributes


def get_runs(runs_directory):
    """List all GEOS-Chem runs in the provided directory"""
    if os.path.basename(runs_directory).startswith("run."):
        # A single run
        return [
            {
                "short_path": os.path.join(os.path.basename(runs_directory)),
                "full_path": runs_directory,
            }
        ]

    run_paths = []
    for root, dirs, files in os.walk(runs_directory):
        to_remove = []
        for dirname in dirs:
            if dirname.startswith("run."):
                run_paths.append(
                    os.path.relpath(os.path.join(root, dirname), runs_directory)
                )
                to_remove.append(dirname)
        for dirname in to_remove:
            dirs.remove(dirname)
    return [
        {
            "short_path": run_path,
            "full_path": os.path.join(runs_directory, run_path),
        }
        for run_path in sorted(run_paths)
    ]
