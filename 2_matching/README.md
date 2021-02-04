# Overview

This involves three procedures:

- Subsetting:
  * For OCO-2 and TCCON, this means extracting the vertical column from the model output at the time and location of the measurement;
  * For ObsPack, this mean combining the GEOS-Chem ObsPack daily diagnostic output into one file. The averaging interval for some observations span two days, so the samples from the two days are combined.
- Compute XCO2: for OCO-2 and TCCON, calculate the XCO2 based on the applying the OCO-2 averaging kernel
- Combining: merge the OCO-2 and TCCON XCO2 output with the ObsPack output into a single file

These are done separately, with results stored in a directory structure that parallels that of `1_transport`. This is under `intermediates/matched-runs/GEOS_Chem` and involves directories like `run.v12.3.2.base` and `2017/run.v12.3.2.R03M02`. Within a run, the following files are created:

- `subsetted-{oco2,tccon,obspack}.nc` as the output of subsetting
- `xco2-{oco2,tccon}.nc` as the output of column averaging
- `combined.nc` as the output of combining
