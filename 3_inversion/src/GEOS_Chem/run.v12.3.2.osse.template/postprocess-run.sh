#!/bin/bash

set -e
set -x

# Concatenate hourly HEMCO_diagnostics into monthly files to save inodes
cdo -L -z zip_6 \
    -splityearmon \
    -cat output/HEMCO_diagnostics\*nc \
    output/HEMCO_diagnostics.

# Remove hourly HEMCO files
find output/ \
    -name "HEMCO_diagnostics.????????????.nc" \
    -delete

# Compress log files
gzip -f output/HEMCO.log output/geos.mp.log

# Remove trac_avg file
find output/ \
    -name trac_avg* \
    -delete
