## Variables

1_TRANSPORT_SOURCE = $(1_TRANSPORT)/src
1_TRANSPORT_INTERMEDIATES = $(1_TRANSPORT)/intermediates

$(shell mkdir -p $(1_TRANSPORT_INTERMEDIATES))

EXT_DATA = $(DATA)/GEOS_Chem/ExtData
RESTART_FILE = $(DATA)/GEOSChem.Restart.20140901_0000z.nc4
OBSPACK_DATA = $(DATA)/obspack_co2_1_CT2016_prepped_inputs_RESTRICTED_2017-04-26/data/nc \
	$(DATA)/obspack_co2_1_NRT_v3.3_prepped_inputs_2017-04-26/data/nc
CT2019_PRIOR_INPUT = $(DATA)/CT2019_b4_prior
ODIAC2018_PRIOR_INPUT = $(DATA)/ODIAC_Dataset_2018

CT2019_PRIOR_HEMCO = $(1_TRANSPORT_INTERMEDIATES)/CT2019.prior_b4_3hrly.nc
ODIAC2018_PRIOR_HEMCO = $(1_TRANSPORT_INTERMEDIATES)/ODIAC_CO2.monthly.generic.1x1.nc

GEOS_CHEM_OBSPACK = $(1_TRANSPORT_INTERMEDIATES)/geos_chem_obspack
GEOS_CHEM_OBSPACK_LAST_UPDATED = $(1_TRANSPORT_INTERMEDIATES)/geos_chem_obspack/LAST_UPDATED

GEOS_CHEM_DIR = $(1_TRANSPORT_INTERMEDIATES)/GEOS_Chem
GEOS_CHEM_RUNS = $(GEOS_CHEM_DIR)/runs
GEOS_CHEM_BASE_RUN = $(GEOS_CHEM_RUNS)/run.v12.3.2.base
GEOS_CHEM_BASE_RUN_THREADS = 16
GEOS_CHEM_BASE_RUN_LAST_UPDATED = $(GEOS_CHEM_BASE_RUN)/LAST_UPDATED
GEOS_CHEM_BASE_RUN_TEMPLATE = $(1_TRANSPORT_SOURCE)/GEOS_Chem/run.v12.3.2.base.template

GEOS_CHEM_SENSITIVITY_RUN_THREADS = 2
GEOS_CHEM_SENSITIVITY_RUNS_LAST_UPDATED = $(GEOS_CHEM_RUNS)/LAST_UPDATED_SENSITIVITY_RUNS
GEOS_CHEM_SENSITIVITY_RUN_TEMPLATE = $(1_TRANSPORT_SOURCE)/GEOS_Chem/run.v12.3.2.sensitivity.template

## Additions to global target lists

1_TRANSPORT_TARGETS += $(GEOS_CHEM_OBSPACK_LAST_UPDATED) $(GEOS_CHEM_BASE_RUN_LAST_UPDATED) $(GEOS_CHEM_SENSITIVITY_RUNS_LAST_UPDATED)
LINT_TARGETS += lint_1_transport
CLEAN_TARGETS += clean_1_transport

## Targets

$(GEOS_CHEM_OBSPACK_LAST_UPDATED): $(1_TRANSPORT_SOURCE)/prepare-geos-chem-obspack.py
	rm -rf $(GEOS_CHEM_OBSPACK)
	mkdir -p $(GEOS_CHEM_OBSPACK)
	python3 $< \
		--obspack $(OBSPACK_DATA) \
		--start-date $(INVERSION_START_DATE) \
		--end-date $(INVERSION_END_DATE) \
		--output-directory $(GEOS_CHEM_OBSPACK)
	touch $@

$(GEOS_CHEM_BASE_RUN_LAST_UPDATED): $(1_TRANSPORT_SOURCE)/prepare-geos-chem-base-run.py $(GEOS_CHEM_OBSPACK_LAST_UPDATED) $(CT2019_PRIOR_HEMCO) $(ODIAC2018_PRIOR_HEMCO) $(GEOS_CHEM_BASE_RUN_TEMPLATE)/*
	python3 $< \
	    --code-directory $(GEOS_CHEM_SOURCE) \
	    --ext-data $(EXT_DATA) \
	    --ct2019-b4 $(CT2019_PRIOR_HEMCO) \
	    --odiac2018 $(ODIAC2018_PRIOR_HEMCO) \
	    --restart-file $(RESTART_FILE) \
	    --obspack-data $(GEOS_CHEM_OBSPACK) \
	    --template $(GEOS_CHEM_BASE_RUN_TEMPLATE) \
	    --start-date $(INVERSION_START_DATE) \
	    --end-date $(INVERSION_END_DATE) \
	    --threads $(GEOS_CHEM_BASE_RUN_THREADS) \
	    --output $(@D)
	touch $@
	echo "Base run ready to run. If using Slurm, you can run \`make submit_geos_chem_base_run\` to submit the job now."

submit_geos_chem_base_run: $(GEOS_CHEM_BASE_RUN_LAST_UPDATED)
	cd $(GEOS_CHEM_BASE_RUN)
	sbatch run_geos.sbatch

$(GEOS_CHEM_SENSITIVITY_RUNS_LAST_UPDATED): $(1_TRANSPORT_SOURCE)/prepare-geos-chem-sensitivity-runs.py $(GEOS_CHEM_OBSPACK_LAST_UPDATED) $(CT2019_PRIOR_HEMCO) $(ODIAC2018_PRIOR_HEMCO) $(GEOS_CHEM_SENSITIVITY_RUN_TEMPLATE)/*
	python3 $< \
	    --code-directory $(GEOS_CHEM_SOURCE) \
	    --ext-data $(EXT_DATA) \
	    --ct2019-b4 $(CT2019_PRIOR_HEMCO) \
	    --odiac2018 $(ODIAC2018_PRIOR_HEMCO) \
	    --transcom-mask $(TRANSCOM_MASK) \
	    --base-run $(GEOS_CHEM_BASE_RUN) \
	    --obspack-data $(GEOS_CHEM_OBSPACK) \
	    --template $(GEOS_CHEM_SENSITIVITY_RUN_TEMPLATE) \
	    --start-date $(INVERSION_START_DATE) \
	    --end-date $(INVERSION_END_DATE) \
	    --region $(SENSITIVITY_REGIONS) \
	    --duration $(SENSITIVITY_DURATION) \
	    --threads $(GEOS_CHEM_SENSITIVITY_RUN_THREADS) \
	    --output $(@D)
	touch $@
	echo "Sensitivity run directories ready. They will be ready to run when the base run is completed. To submit all jobs using Slurm, run \`make submit_geos_chem_sensitivity_runs\`."

submit_geos_chem_sensitivity_runs: $(1_TRANSPORT_SOURCE)/submit-geos-chem-sensitivity-runs.sh $(GEOS_CHEM_SENSITIVITY_RUNS_LAST_UPDATED)
	bash $< $(GEOS_CHEM_RUNS)

setup_runs: $(GEOS_CHEM_BASE_RUN_LAST_UPDATED) $(GEOS_CHEM_SENSITIVITY_RUNS_LAST_UPDATED)

$(CT2019_PRIOR_HEMCO): $(1_TRANSPORT_SOURCE)/convert-ct2019-bio-to-geoschem.sh
	bash $< $(CT2019_PRIOR_INPUT) $@

$(ODIAC2018_PRIOR_HEMCO): $(1_TRANSPORT_SOURCE)/convert-odiac-to-geoschem.sh
	bash $< $(ODIAC2018_PRIOR_INPUT) $@

mark_runs_complete:
	touch $(GEOS_CHEM_RUNS)/RUNS_COMPLETE

lint_1_transport:
	pycodestyle $(1_TRANSPORT_SOURCE)/*.py $(1_TRANSPORT_SOURCE)/geoschem

clean_1_transport:
	rm -rf $(GEOS_CHEM_RUNS) $(GEOS_CHEM_OBSPACK)
