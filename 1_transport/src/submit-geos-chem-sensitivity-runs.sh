#!/usr/bin/env bash

# ------------------------------------------------------------------------------
# Written by Yi Cao@ 15/06/2020
#
# find the slurm sbatch job in the given directory and submit jobs to slurm
# ------------------------------------------------------------------------------


if [[ -z $1 ]]
then
    loc=.
else
    loc=$1
fi

job_dir=$(find $loc -name run_geos.sbatch | xargs -l dirname | xargs -l realpath)  

for i in ${job_dir}
do
    echo "Submitting ${i}"
    (cd ${i} && sbatch run_geos.sbatch)
done
