#!/bin/bash

set -e
set -x

SITES="ascension \
    bialystok \
    bremen \
    caltech \
    darwin \
    dryden \
    eureka \
    garmisch \
    izana \
    karlsruhe \
    lamont \
    lauder \
    manaus \
    orleans \
    paris \
    parkfalls \
    reunion \
    rikubetsu \
    saga \
    sodankyla \
    tsukuba \
    wollongong"

EXPERIMENTS="IS LN LG"

mkdir -p data/TCCON_MIP_fits
pushd data/TCCON_MIP_fits
for experiment in $EXPERIMENTS; do
    echo $experiment
    for site in $SITES; do
        wget \
            -v \
            -c \
            https://www.esrl.noaa.gov/gmd/webdata/ccgg/OCO2_level4_release/data_dygraphs_tccon/${experiment}/${site}_obs.csv \
            -O ${experiment}_${site}_obs.csv
        wget \
            -v \
            -c \
            https://www.esrl.noaa.gov/gmd/webdata/ccgg/OCO2_level4_release/data_dygraphs_tccon/${experiment}/${site}_res.csv \
            -O ${experiment}_${site}_res.csv
    done
done
popd
