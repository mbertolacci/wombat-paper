# WOMBAT: A fully Bayesian global flux-inversion framework

This repository contains code to reproduce the results in the paper [WOMBAT: A fully Bayesian global flux-inversion framework](link). This README assumes at least a passing familiarity with the WOMBAT framework, or of Bayesian-synthesis inversions more generally.

Unless stated otherwise, all commands are to be run in the root directory of the repository.

# Installation/setting up an environment

This workflow requires both Python version 3+ and R version 4+, along with a variety of dependency packages in both languages. The easiest way to set up an environment in which to run this code is to use [Anaconda](https://www.anaconda.com/). Instructions for setting up an appropriate conda environment are provided below. If, for some reason, you don't want to do that, you can adapt the instructions below to a local installation using `pip` and your local copy of R - everything should work okay. For what remains, we assume you have Anaconda installed.

Create a conda environment and set it to use [conda-forge](https://conda-forge.org/):

```
conda create --yes --prefix .conda_env
conda activate ./.conda_env
conda config --env --add channels conda-forge
conda config --env --set channel_priority strict
```

Install conda packages

```
conda install python==3.8 r-base pkg-config udunits2 libgdal xarray scipy \
  netcdf4 numpy==1.19.2 jinja2 netcdf-fortran cdo nco fortran-compiler==1.0.4 \
  gfortran_linux-64==7.3.0
```

Set your CRAN mirror. WOMBAT was made in Australia, so we choose the mirror run by CSIRO, but you can pick your favourite:

```
cat > .conda_env/lib/R/etc/Rprofile.site <<- EOF
local({
  r <- getOption('repos')
  r['CRAN'] <- 'https://cran.csiro.au'
  options(repos = r)
})
EOF
```

Install CRAN dependencies (this might take awhile):

```
Rscript -e "install.packages(c(
  'devtools', 'raster', 'argparser', 'codetools', 'ncdf4', 'fst', 'matrixStats',
  'readr', 'argparse', 'scoringRules', 'patchwork', 'lifecycle', 'sf',
  'ggplot2', 'dplyr', 'tidyr', 'RcppEigen', 'rnaturalearth',
  'rnaturalearthdata', 'rgeos'
))"
```

Install the required local R packages:

```
Rscript -e "devtools::install('fastsparse'); devtools::install('wombat')"
```

Check out the required version of GEOS-Chem:

```
(cd external/GEOS_Chem && git clone --branch 12.3.2 https://github.com/geoschem/geos-chem.git Code.v12.3.2)
```

## Install tensorflow

If you have access to a GPU, it's worth installing tensorflow to speed up inversions considerably. This can be done with

```
pip3 install tensorflow-gpu
Rscript -e "install.packages('tensorflow')""
```

# Getting data

All input datasets go into the `data` directory. There are a few files already there, but the rest will need to be retrieved from their primary sources.

## Data required for all steps

Several publicly-available datasets are required to run any part of the code.

- The OCO-2 MIPv7 data files:
  - The OCO-2 10 s averages, in a file named `OCO2_b70b_10sec_WL15_GOOD_v10.nc`. These are available from the [OCO-2 MIPv7 website](https://www.esrl.noaa.gov/gmd/ccgg/OCO2/). Place this file in the `data` directory.
  - TCCON 30 minute averages, available from the [OCO-2 MIPv7 website](https://www.esrl.noaa.gov/gmd/ccgg/OCO2/). The NetCDF files should be placed in the directory `data/TCCON/20170706`.
  - The ObsPack measurements used by the MIPv7. At present, these are not available from the MIPv7 website. Please ask the OCO-2 MIPv7 team for assistance, or reach out to us and we can help. The two relevant datasets are `obspack_co2_1_CT2016_prepped_inputs_RESTRICTED_2017-04-26` and `obspack_co2_1_NRT_v3.3_prepped_inputs_2017-04-26`, and go into the `data` directory.
- Some of the results of OCO-2 MIPv7:
  - The posterior fluxes, available from the [OCO-2 MIPv7 website](https://www.esrl.noaa.gov/gmd/ccgg/OCO2/) under "Level 4 product download". These should be placed in the `data/mip-fluxes` directory.
  - The posterior fits to TCCON retrievals. These are available from the MIPv7 website, and can be downloaded by running `bash data/download-mip-tccon-fits.sh`

## Data required for the GEOS-Chem runs

GEOS-Chem requires meteorological fields and CO2 emission to run. These go into the `data/GEOS_Chem` directory. There are instructions for how to download these files on the [GEOS-Chem wiki](http://wiki.seas.harvard.edu/geos-chem/index.php/Downloading_data_from_Compute_Canada).

The directories you need to download are:

- `ExtData/GEOS_2x2.5/GEOS_FP`
- `ExtData/CHEM_INPUTS/MODIS_LAI_201204`
- `ExtData/CHEM_INPUTS/Olson_Land_Map_201203`
- `ExtData/HEMCO/AEIC/v2015-01`
- `ExtData/HEMCO/CO2/v2015-04/OCEAN`
- `ExtData/HEMCO/CO2/v2015-04/BIOFUEL`
- `ExtData/HEMCO/QFED/v2018-07/2014`
- `ExtData/HEMCO/QFED/v2018-07/2015`
- `ExtData/HEMCO/QFED/v2018-07/2016`
- `ExtData/HEMCO/QFED/v2018-07/2017`

There are some other datasets required. These are

- The CarbonTracker 2019 biosphere prior fluxes. These are available at the [CarbonTracker website](https://www.esrl.noaa.gov/gmd/ccgg/carbontracker/download.php). The files in particular you want are named `CT2019.prior_b4_3hrly_YYYY.nc`, where `YYYY` is a year. These should be placed into `data/CT2019_b4_prior`.
- ODIAC 2018 fossil-fuel fluxes. These are available from the [ODIAC website](https://db.cger.nies.go.jp/dataset/ODIAC/DL_odiac2018.html). Place the yearly files `odiac2018_1x1d_YYYY.nc` into `data/ODIAC_Dataset_2018`.

## Intermediate output of matching step to run inversions

# Running the workflow

First, get the required data as listed in [Data required for all steps](#data-required-for-all-steps).

There are four main steps in the workflow, corresponding to four numbered directories. These are:

1. `1_transport`: Sets up and performs GEOS-Chem runs for the prior mean of the CO2 mole-fraction field, and for the mole-fraction basis functions which arise from perturbing the flux field. The outputs of these runs are netcdf files containing the space/time field of the CO2 fluxes and the space/height/time field of the CO2 mole fractions. This is the most computationally intensive step in the workflow by far.
2. `2_matching`: Postprocesses the outputs in the previous step. In particular, monthly averages of the flux field are computed, and the mole-fraction field is subsetted to certain locations and times of interest (corresponding to locations/times with data).
3. `3_inversion`: The meat of the inversions. Using the reduced fields computed in the previous step, constructs a transport-model matrix. Preprocesses data to a suitable form for inversions. Performs the inversions using the WOMBAT statistical framework, and calculates posterior fluxes in a usable form.
4. `4_results`: Generates the figures and tables presented in the WOMBAT paper.

If you want to reproduce all the results, you need to run all the steps. The slowest is the first, running the transport model. If you just want to run inversions, you can find a link the required intermediate files in the [Intermediate output of matching step to run inversions](#intermediate-output-of-matching-step-to-run-inversions) section of this document. These contain the results of the `2_matching` step. If for some reason you need the results of the full GEOS-Chem runs, which are several terabytes, please either run them yourself (see below, preferable), or get in contact and we will see if we can arrange access to them from our archives.

## Completing the GEOS-Chem runs

This step will complete the GEOS-Chem runs, and is by far the most computationally intensive part of the workflow. If you just want to do inversions on the same time period and data as we did, you can skip this step! That aside...

First, follow the steps in [Data required for all steps](#data-required-for-all-steps) and [Data required for the GEOS-Chem runs](#data-required-for-the-geos-chem-runs). Then run

```
make setup_runs
```

will create the directories for the base and sensitivities runs of GEOS-Chem. These exist as

- `1_transport/intermediates/GEOS_Chem/runs/run.v12.3.2.base` - the base run.
- `1_transport/intermediates/GEOS_Chem/runs/yyyy/run.v12.3.2.RrrMmm` - sensitivity runs over years (yyyy), regions (rr), and months (mm).

The base run needs to be completed before the sensitivity runs can complete, because the latter need restart files produced by the former.

How you actually run these depends on your system. On a single core machine, completing these runs takes several years of computation. For the WOMBAT paper, we completed the runs partly on our local HPC, and partly on the [NCI's](http://nci.org.au/) Gadi supercomputer. The absolute simplest way is to change into the run directory and run:

```
bash run_geos.mp
```

Alternatively, if your system has SLURM, you can change into the directory and schedule a single run with

```
sbatch run_geos.sbatch
```

or multiple runs with

```
make submit_geos_chem_base_run
```

for the base run and

```
make submit_geos_chem_sensitivity_runs
```

for the sensitivity runs. Keep in mind, you may need to edit the file `run_geos.sbatch` for compatibility with your system. You can do this for all runs by editing `1_transport/src/GEOS_Chem/run.v12.3.2.base.template/run_geos.sbatch` and `1_transport/src/GEOS_Chem/run.v12.3.2.sensitivity.template/run_geos.sbatch` and running `make setup_runs` again.

Finally, for the sensitivity runs there is a script `run_geos.gadi` for NCI's Gadi, which may be useful if you want to use the same system.

Once all the runs are complete, run

```
make mark_runs_complete
```

which will allow subsequent steps to run.

## Running the matching step

If you have completed the GEOS-Chem runs, you can run the matching steps with

```
WOMBAT_MAX_WORKERS=8 make -j2 2_matching_targets
```

You can modify the `WOMBAT_MAX_WORKERS` variable and the `-j` option to suit your local system.

Alternatively, you can skip this step by downloading the intermediate files, see [Intermediate output of matching step to run inversions](#intermediate-output-of-matching-step-to-run-inversions).

## Running inversions and generating the figures

If you have completed the matching step, or downloaded the required intermediate files (see [Intermediate output of matching step to run inversions](#intermediate-output-of-matching-step-to-run-inversions) above), you can run

```
make -j4 4_results_targets
```

You can modify the `-j` option to suit your local system. If you have access to a GPU and have installed `tensorflow` (see [Install tensorflow](#install-tensorflow)), you can run inversions using

```
WOMBAT_TENSORFLOW=1 make -j1 3_inversion
make -j4 4_results_targets
```

Note that the above is split into two parts, the first with make parallelism turned off, because most GPUs only have enough memory to run one inversion at a time. So we complete the inversions before generating the figures with parallelism turned back on.
