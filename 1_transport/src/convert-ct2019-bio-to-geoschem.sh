#!/bin/bash

set -ex

GEOSChemPerltool=external/GEOS_Chem/Code.v12.3.2/NcdfUtil/perl
type=b4

rm -f $2

# merge files
ncrcat $1/*.nc $2

# rename variables, some versions of netCDF has bug, which cause coordinates filled with missing value when renaming
# one workaround is to convert the file to netcdf3 then rename and convert back to netcdf4,
# The netcdf version used here is 4.6.1 with nco 4.7.2, which can work well
ncrename -d latitude,lat -d longitude,lon -v latitude,lat -v longitude,lon -v ${type},CO2 $2

tmpfile=$(mktemp /tmp/convert-ct2019.XXXXXX)
# convert the units from mol/m2/s to kg/m2/s *0.04401 kg/mol
ncap2 -O -s 'CO2=CO2*0.04401' $2 ${tmpfile}
# shift time back 90 minutes as Michael finds a problem about the flux output from HEMCO 11/6/2029
ncap2 -O -s 'time=time-0.0625' ${tmpfile} $2
rm ${tmpfile}

# modify variables' attributes
ncatted -a units,CO2,o,c,"kg/m2/s" -a long_name,CO2,c,c,"CO2 from CT2019 prior ${type} 3hourly" -hO $2
ncatted -a standard_name,time,c,c,"time" -a calendar,time,c,c,"standard" -a axis,time,c,c,"T" -hO $2
ncatted -a long_name,lon,c,c,"Longitude" -a axis,lon,c,c,"X" -a long_name,lat,c,c,"Latitude" -a axis,lat,c,c,"Y" -hO $2

# modify the Global attribute
ncatted -a Title,global,c,c,"BIOSPHERE netCDF file derived from CT2019 prior ${type} 3 hourly files" -hO $2

# apply chunk and deflating using GEOSChem utility nc_chunk.pl
${GEOSChemPerltool}/nc_chunk.pl $2 1

# Coards check
${GEOSChemPerltool}/isCoards $2
