Install fastsparse and wombat.

# Setting up an environment

This workflow requires both Python version 3+ and R version 4+, along with a variety of dependency packages in both languages. The easiest way to set up an environment in which to run this code is to use [Anaconda](https://www.anaconda.com/). Instructions for setting up an appropriate conda environment are provided below. If, for some reason, you don't want to do that, you can adapt the instructions below to a local installation using `pip` and your local copy of R - everything should work okay. For what remains, we assume you have Anaconda installed.

Create a conda environment and set it to use [conda-forge](https://conda-forge.org/):

```
conda create --prefix .conda_env
conda activate .conda_env
conda config --env --add channels conda-forge
conda config --env --set channel_priority strict
```

Install conda packages

```
conda install r-base pkg-config udunits2 libgdal xarray scipy netcdf4
```

Set your CRAN mirror. WOMBAT was made in Australia, so we choose the mirror run by CSIRO, but you can pick your favourite:

```
cat > .conda_env/lib/R/etc/Rprofile.site <<- EOF
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.csiro.au"
    options(repos = r)
})
EOF
```

Install CRAN dependencies (this might take awhile):

```
Rscript -e "install.packages(c('devtools', 'raster', 'argparser', 'codetools', 'ncdf4', 'fst'))"
Rscript -e "install.packages(c('fst'))"
```

Install the required local R packages:

```
Rscript -e "devtools::install('fastsparse'); devtools::install('wombat')"
```
