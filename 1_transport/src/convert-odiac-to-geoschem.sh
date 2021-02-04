#!/bin/bash

set -e

version=2018

startyear=2000
endyear=2017
years=$(seq ${startyear} ${endyear})

perltool=external/GEOS_Chem/Code.v12.3.2/NcdfUtil/perl

inputdir=$1
output_file=$2
# mergefile=ODIAC_merge.monthly.generic.1x1.nc
# CO2land_file=ODIAC_CO2.monthly.generic.1x1.nc
# CO2intlbunker_file=ODIAC_CO2_intl_bunker.monthly.generic.1x1.nc

tmp_file=$(mktemp odiac-XXXXXXXXXX)
tmp_dir=$(mktemp -d -t odiac-XXXXXXXXXX)

# ODIAC files are yearly files, in which each file stores 12 months's data
for year in ${years}
do
    base_filename=odiac${version}_1x1d_${year}.nc
    converted_filename=${tmp_dir}/${base_filename}
    echo "Converting ${base_filename}"

    # convert the file format from Classic to netCDF-4 classic model
    ncks -7 --no_abc ${inputdir}/${base_filename} ${converted_filename}

    # rename the variable "month" to "time" as well as the dimension variable "month"
    ncrename -v month,time -d month,time ${converted_filename}

    # remove the attribute time:definition, add attribute units & set reference time
    ncatted -a definition,time,d,, -a units,time,c,c,"months since ${year}-01-01 00:00:00" ${converted_filename}

    # shift time by 1 month to correct start time
    ncap2 -O -s 'time=time-1' ${converted_filename} ${tmp_file}
    #OR cdo -expr,'time=time-1' ${converted_filename} ${tmp_file}
    mv -f ${tmp_file} ${converted_filename}

    # change the reference time to 1985-01-01 00:00:00 as in the GEOSChem odiac file
    shiftyear=$((15+year-2000))
    cdo -s shifttime,${shiftyear}years -settaxis,1985-01-01,00:00:00,1month ${converted_filename} ${tmp_file}
    mv -f ${tmp_file} ${converted_filename}

    # set time:units to hours since
    cdo -s settunits,hours ${converted_filename} ${tmp_file}
    mv -f ${tmp_file} ${converted_filename}

    # delete the unnecessary attribut in variable timee
    ncatted -a long_name,time,d,, ${converted_filename}

    # change data type of lon and lat to "float" to match the data typee in the GEOSChem odiac file
    ncap2 -O -s 'lon=float(lon);lat=float(lat)' ${converted_filename} ${tmp_file}
    mv -f ${tmp_file} ${converted_filename}

    # converting the units of land and intl_bunker from gC/m2/d to kg/m2/s
    ncap2 -O -s 'land=land/1000*44/12/24/3600' -s 'intl_bunker=intl_bunker/1000*44/12/24/3600' ${converted_filename} ${tmp_file}
    #or cdo -expr,'land=land/1000*44/12/24/3600; intl_bunker=intl_bunker/1000*44/12/24/3600' ${converted_filename} ${tmp_file}
    mv -f ${tmp_file} ${converted_filename}

    # modify the attribute of CO2
    ncatted -a definition,land,d,, -a units,land,o,c,"kg/m2/s" -a long_name,land,o,c,"ODIAC Fossil Fuel CO2 from Tom Oda" ${converted_filename}
    ncatted -a definition,intl_bunker,d,, -a units,intl_bunker,o,c,"kg/m2/s" -a long_name,intl_bunker,o,c,"ODIAC CO2 of intl aviation, marine bunker, plus antarctic fishery from Tom Oda" ${converted_filename}

    # make time dimension record dimension
    ncks -O --mk_rec_dmn time ${converted_filename} ${tmp_file}
    mv -f ${tmp_file} ${converted_filename}
done

# merge the files
# rm -vf ${mergefile} ${CO2land_file} ${CO2intlbunker_file}
ncrcat -O ${tmp_dir}/odiac${version}_1x1d_*.nc ${output_file}

rm -r ${tmp_dir}

# apply chunk and deflating using GEOSChem utility nc_chunk.pl
${perltool}/nc_chunk.pl ${output_file} 1

# modify the Global attribute
ncatted -a global_total,global,d,, -a citation,global,d,, -a email,global,d,, -a url,global,d,, -a nco_openmp_thread_number,global,d,, -a version,global,d,, ${output_file}
ncatted -a Title,global,c,c,"ODIAC file created from TOM ODA ODIAC${version} Dataset" -a Model,global,c,c,"GENERIC" -a Delta_Lon,global,c,f,1 -a Delta_Lat,global,c,f,1 -a NLayers,global,c,l,1 -a Start_Date,global,c,l,${startyear}0101 -a Start_Time,global,c,l,0 -a End_Date,global,c,l,${endyear}1201 -a End_Time,global,c,l,0 -a Delta_Time,global,c,l,0   ${output_file}

# # Generate the land and intl_bunker
# ncks --no_abc -v land ${mergefile} ${CO2land_file}
# ncks --no_abc -v intl_bunker ${mergefile} ${CO2intlbunker_file}

# # rename the land and intl_bunker to CO2
# ncrename -v land,CO2 ${CO2land_file}
# ncrename -v intl_bunker,CO2 ${CO2intlbunker_file}

# # Coards check
# ${perltool}/isCoards ${CO2land_file}
# ${perltool}/isCoards ${CO2intlbunker_file}
