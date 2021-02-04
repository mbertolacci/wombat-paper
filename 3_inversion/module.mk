3_INVERSION_SOURCE = $(3_INVERSION)/src
3_INVERSION_INTERMEDIATES = $(3_INVERSION)/intermediates
3_INVERSION_FIGURES = $(3_INVERSION)/figures

$(shell mkdir -p $(3_INVERSION_INTERMEDIATES) $(3_INVERSION_FIGURES))

# Partials: exported so scripts don't duplicate the paths
INVERSION_BASE_PARTIAL = $(3_INVERSION_SOURCE)/partials/base.R
export INVERSION_BASE_PARTIAL
INVERSION_DISPLAY_PARTIAL = $(3_INVERSION_SOURCE)/partials/display.R
export INVERSION_DISPLAY_PARTIAL
INVERSION_OSSE_PARTIAL = $(3_INVERSION_SOURCE)/partials/osse.R
export INVERSION_OSSE_PARTIAL
INVERSION_TRANSCOM_UTILS_PARTIAL = $(3_INVERSION_SOURCE)/partials/transcom-utils.R
export INVERSION_TRANSCOM_UTILS_PARTIAL

MIP_FLUXES_DIRECTORY = $(DATA)/mip-fluxes
MIP_TCCON_DIRECTORY = $(DATA)/TCCON_MIP_fits

CONTROL_EMISSIONS = $(3_INVERSION_INTERMEDIATES)/control-emissions.fst
PERTURBATIONS = $(3_INVERSION_INTERMEDIATES)/perturbations.fst

CONTROL_MOLE_FRACTION = $(3_INVERSION_INTERMEDIATES)/control-mole-fraction.fst
SENSITIVITIES = $(3_INVERSION_INTERMEDIATES)/sensitivities.fst

OBSERVATIONS = $(3_INVERSION_INTERMEDIATES)/observations.fst

PROCESS_MODEL = $(3_INVERSION_INTERMEDIATES)/process-model.rds
MEASUREMENT_MODEL = $(3_INVERSION_INTERMEDIATES)/measurement-model.rds

MIP_FLUXES = $(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst
MIP_TCCON_RESIDUAL = $(3_INVERSION_INTERMEDIATES)/mip-tccon-residual.fst

FLUX_AGGREGATORS = $(3_INVERSION_INTERMEDIATES)/flux-aggregators.rds

OSSE_THREADS = 8
OSSE_ANOMALY = $(3_INVERSION_INTERMEDIATES)/osse-anomaly.fst
OSSE_BASE_CASES = random1 random2
OSSE_CASES = random1-LG-TRUEBIAS-FIXEDAO-FIXEDWO5 \
	random1-LG-TRUEBIAS-FIXEDCORR-FIXEDAO-FIXEDWO5 \
	random1-LG-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5 \
	random1-LG-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5 \
	random1-LG-TRUEBIAS-NOBIAS-FIXEDCORR-FIXEDAO-FIXEDWO5 \
	random1-LG-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5 \
	random1-LN-TRUEBIAS-FIXEDAO-FIXEDWO5 \
	random1-LN-TRUEBIAS-FIXEDCORR-FIXEDAO-FIXEDWO5 \
	random1-LN-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5 \
	random1-LN-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5 \
	random1-LN-TRUEBIAS-NOBIAS-FIXEDCORR-FIXEDAO-FIXEDWO5 \
	random1-LN-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5 # \

OSSE_RUN_BASE = $(3_INVERSION_INTERMEDIATES)/GEOS_Chem/runs/run.v12.3.2.osse
OSSE_RUNS = $(foreach OSSE_BASE_CASE,$(OSSE_BASE_CASES),$(OSSE_RUN_BASE).$(OSSE_BASE_CASE)/LAST_PREPARED)

OSSE_MATCHED_RUN_BASE = $(3_INVERSION_INTERMEDIATES)/GEOS_Chem/matched-runs/run.v12.3.2.osse
OSSE_SUBSETTED_RUNS = $(foreach OSSE_BASE_CASE,$(OSSE_BASE_CASES),$(OSSE_MATCHED_RUN_BASE).$(OSSE_BASE_CASE)/LAST_SUBSETTED)
OSSE_XCO2_RUNS = $(foreach OSSE_BASE_CASE,$(OSSE_BASE_CASES),$(OSSE_MATCHED_RUN_BASE).$(OSSE_BASE_CASE)/LAST_COMPUTED_XCO2)
OSSE_COMBINED_RUNS = $(foreach OSSE_BASE_CASE,$(OSSE_BASE_CASES),$(OSSE_MATCHED_RUN_BASE).$(OSSE_BASE_CASE)/LAST_COMBINED)


OSSE_MODEL_BASE = $(3_INVERSION_INTERMEDIATES)/osse-model
OSSE_MODEL_CASES = $(foreach OSSE_CASE,$(OSSE_CASES),$(OSSE_MODEL_BASE)-$(OSSE_CASE).rds)
OSSE_MCMC_SAMPLES_BASE = $(3_INVERSION_INTERMEDIATES)/osse-mcmc-samples
OSSE_MCMC_SAMPLES_CASES = $(foreach OSSE_CASE,$(OSSE_CASES),$(OSSE_MCMC_SAMPLES_BASE)-$(OSSE_CASE).rds)
OSSE_FLUX_AGGREGATES_SAMPLES_BASE = $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples
OSSE_FLUX_AGGREGATES_SAMPLES_CASES = $(foreach OSSE_CASE,$(OSSE_CASES),$(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-$(OSSE_CASE).rds)
OSSE_TRACEPLOTS_BASE = $(3_INVERSION_FIGURES)/osse-traceplots
OSSE_TRACEPLOTS = $(foreach OSSE_CASE,$(OSSE_CASES),$(OSSE_TRACEPLOTS_BASE)-$(OSSE_CASE).png)

REAL_CASES = LGr-FIXEDAO-FIXEDWO5 \
	LNr-FIXEDAO-FIXEDWO5 \
	LG-FIXEDAO-FIXEDWO5-NOBIAS \
	LN-FIXEDAO-FIXEDWO5-NOBIAS \
	LG-RHO0-FIXEDAO-FIXEDWO5-NOBIAS \
	LN-RHO0-FIXEDAO-FIXEDWO5-NOBIAS \
	LG-FIXEDAO-FIXEDWO5 \
	LN-FIXEDAO-FIXEDWO5

REAL_MODEL_BASE = $(3_INVERSION_INTERMEDIATES)/real-model
REAL_MODEL_CASES = $(foreach REAL_CASE,$(REAL_CASES),$(REAL_MODEL_BASE)-$(REAL_CASE).rds)
REAL_MCMC_SAMPLES_BASE = $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples
REAL_MCMC_SAMPLES_CASES = $(foreach REAL_CASE,$(REAL_CASES),$(REAL_MCMC_SAMPLES_BASE)-$(REAL_CASE).rds)
REAL_FLUX_AGGREGATES_SAMPLES_BASE = $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples
REAL_FLUX_AGGREGATES_SAMPLES = $(foreach REAL_CASE,$(REAL_CASES),$(REAL_FLUX_AGGREGATES_SAMPLES_BASE)-$(REAL_CASE).rds)
REAL_TCCON_SAMPLES_BASE = $(3_INVERSION_INTERMEDIATES)/real-tccon-samples
REAL_TCCON_SAMPLES = $(foreach REAL_CASE,$(REAL_CASES),$(REAL_TCCON_SAMPLES_BASE)-$(REAL_CASE).rds)
REAL_TRACEPLOTS_BASE = $(3_INVERSION_FIGURES)/real-traceplots
REAL_TRACEPLOTS = $(foreach REAL_CASE,$(REAL_CASES),$(REAL_TRACEPLOTS_BASE)-$(REAL_CASE).png)

OSSE_PARTS = $(OSSE_TRACEPLOTS)

REAL_RESULTS_PARTS = $(REAL_TRACEPLOTS)

3_INVERSION_TARGETS = $(OSSE_PARTS) \
	$(REAL_RESULTS_PARTS) \
	$(MIP_TCCON_RESIDUAL)

SECONDARY_TARGETS += $(REAL_MODEL_CASES) \
	$(REAL_MCMC_SAMPLES_CASES) \
	$(REAL_FLUX_AGGREGATES_SAMPLES) \
	$(OSSE_RUNS) \
	$(OSSE_MODEL_CASES) \
	$(OSSE_MCMC_SAMPLES_CASES) \
	$(OSSE_MODEL_CASES) \
	$(OSSE_MCMC_SAMPLES_CASES) \
	$(OSSE_FLUX_AGGREGATES_SAMPLES_CASES)

osse_parts: $(OSSE_PARTS)

real_parts: $(REAL_RESULTS_PARTS)

$(OSSE_TRACEPLOTS_BASE)-%.png: $(3_INVERSION_SOURCE)/traceplots.R $(OSSE_MCMC_SAMPLES_BASE)-%.rds $(INVERSION_DISPLAY_PARTIAL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--samples $(OSSE_MCMC_SAMPLES_BASE)-$*.rds \
		--output $@

$(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-%.rds: $(3_INVERSION_SOURCE)/flux-aggregates-samples.R $(FLUX_AGGREGATORS) $(OSSE_MODEL_BASE)-%.rds $(OSSE_MCMC_SAMPLES_BASE)-%.rds $(PROCESS_MODEL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--flux-aggregators $(FLUX_AGGREGATORS) \
		--model-case $(OSSE_MODEL_BASE)-$*.rds \
		--process-model $(PROCESS_MODEL) \
		--samples $(OSSE_MCMC_SAMPLES_BASE)-$*.rds \
		--output $@

$(OSSE_MCMC_SAMPLES_BASE)-%.rds: $(3_INVERSION_SOURCE)/mcmc-samples.R $(OSSE_MODEL_BASE)-%.rds $(PROCESS_MODEL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--process-model $(PROCESS_MODEL) \
		--model-case $(OSSE_MODEL_BASE)-$*.rds \
		--output $@

$(OSSE_MODEL_BASE)-%.rds: $(3_INVERSION_SOURCE)/osse-model-case.R $(OSSE_ANOMALY) $(PROCESS_MODEL) $(MEASUREMENT_MODEL) $(INVERSION_OSSE_PARTIAL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--case $* \
		--osse-anomaly $(OSSE_ANOMALY) \
		--measurement-model $(MEASUREMENT_MODEL) \
		--process-model $(PROCESS_MODEL) \
		--output $@

$(OSSE_ANOMALY): $(3_INVERSION_SOURCE)/osse-anomaly.R $(OSSE_COMBINED_RUNS) $(CONTROL_MOLE_FRACTION) $(INVERSION_BASE_PARTIAL) $(UTILS_PARTIAL)
	Rscript $< \
		--control $(CONTROL_MOLE_FRACTION) \
		--matched-runs-directory $(3_INVERSION_INTERMEDIATES)/GEOS_Chem/matched-runs \
		--output $@

$(OSSE_MATCHED_RUN_BASE).%/LAST_COMBINED: $(2_MATCHING_SOURCE)/combine-geos-chem.R $(OSSE_MATCHED_RUN_BASE).%/LAST_COMPUTED_XCO2 $(OSSE_MATCHED_RUN_BASE).%/LAST_SUBSETTED
	Rscript $< \
		--matched-runs-directory $(OSSE_MATCHED_RUN_BASE).$*
	touch $@

$(OSSE_MATCHED_RUN_BASE).%/LAST_COMPUTED_XCO2: $(2_MATCHING_SOURCE)/compute-xco2-geos-chem.R $(OSSE_MATCHED_RUN_BASE).%/LAST_SUBSETTED
	Rscript $< \
		--oco2-observations $(OCO2_DATA) \
		--tccon-observation-directory $(TCCON_DATA) \
		--matched-runs-directory $(OSSE_MATCHED_RUN_BASE).$*
	touch $@

$(OSSE_MATCHED_RUN_BASE).%/LAST_SUBSETTED: $(2_MATCHING_SOURCE)/subset-geos-chem.py $(OSSE_RUN_BASE).%/RUN_COMPLETE
	python3 $< \
		--meteorology-run $(GEOS_CHEM_BASE_RUN) \
		--oco2-observations $(OCO2_DATA) \
		--tccon-observation-directory $(TCCON_DATA) \
		--runs-directory $(OSSE_RUN_BASE).$* \
		--output-directory $(3_INVERSION_INTERMEDIATES)/GEOS_Chem/matched-runs
	touch $@

$(OSSE_RUN_BASE).%/RUN_COMPLETE: $(OSSE_RUN_BASE).%/LAST_PREPARED
	(cd $(OSSE_RUN_BASE).$* && bash run_geos.mp)
	touch $@

$(OSSE_RUN_BASE).%/LAST_PREPARED: $(3_INVERSION_SOURCE)/osse-run.R
	Rscript $< \
		--name $* \
		--ext-data $(EXT_DATA) \
		--base-run $(GEOS_CHEM_BASE_RUN) \
		--obspack-data $(GEOS_CHEM_OBSPACK) \
		--threads $(OSSE_THREADS) \
		--template $(3_INVERSION_SOURCE)/GEOS_Chem/run.v12.3.2.osse.template \
		--restart-file $(RESTART_FILE) \
		--code-directory $(GEOS_CHEM_SOURCE) \
		--output $(3_INVERSION_INTERMEDIATES)/GEOS_Chem/runs
	touch $@

# Real

## Figures

$(REAL_TRACEPLOTS_BASE)-%.png: $(3_INVERSION_SOURCE)/traceplots.R $(REAL_MCMC_SAMPLES_BASE)-%.rds $(INVERSION_DISPLAY_PARTIAL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--samples $(REAL_MCMC_SAMPLES_BASE)-$*.rds \
		--output $@

## Intermediates

$(REAL_TCCON_SAMPLES_BASE)-%.rds: $(3_INVERSION_SOURCE)/tccon-samples.R $(OBSERVATIONS) $(REAL_MODEL_BASE)-%.rds $(REAL_MCMC_SAMPLES_BASE)-%.rds $(PROCESS_MODEL) $(INVERSION_TRANSCOM_UTILS_PARTIAL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--observations $(OBSERVATIONS) \
		--model-case $(REAL_MODEL_BASE)-$*.rds \
		--process-model $(PROCESS_MODEL) \
		--samples $(REAL_MCMC_SAMPLES_BASE)-$*.rds \
		--output $@

$(REAL_FLUX_AGGREGATES_SAMPLES_BASE)-%.rds: $(3_INVERSION_SOURCE)/flux-aggregates-samples.R $(FLUX_AGGREGATORS) $(PROCESS_MODEL) $(REAL_MODEL_BASE)-%.rds $(REAL_MCMC_SAMPLES_BASE)-%.rds $(INVERSION_TRANSCOM_UTILS_PARTIAL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--flux-aggregators $(FLUX_AGGREGATORS) \
		--model-case $(REAL_MODEL_BASE)-$*.rds \
		--process-model $(PROCESS_MODEL) \
		--samples $(REAL_MCMC_SAMPLES_BASE)-$*.rds \
		--output $@

$(REAL_MCMC_SAMPLES_BASE)-%.rds: $(3_INVERSION_SOURCE)/mcmc-samples.R $(REAL_MODEL_BASE)-%.rds $(PROCESS_MODEL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--process-model $(PROCESS_MODEL) \
		--model-case $(REAL_MODEL_BASE)-$*.rds \
		--output $@

$(REAL_MODEL_BASE)-%.rds: $(3_INVERSION_SOURCE)/real-model-case.R $(MEASUREMENT_MODEL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--case $* \
		--measurement-model $(MEASUREMENT_MODEL) \
		--process-model $(PROCESS_MODEL) \
		--output $@

# Misc

## Intermediates

$(FLUX_AGGREGATORS): $(3_INVERSION_SOURCE)/flux-aggregators.R $(PROCESS_MODEL) $(TRANSCOM_MASK) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--process-model $(PROCESS_MODEL) \
		--transcom-mask $(TRANSCOM_MASK) \
		--output $@

$(MIP_TCCON_RESIDUAL): $(3_INVERSION_SOURCE)/mip-tccon-residual.R $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--mip-tccon-directory $(MIP_TCCON_DIRECTORY) \
		--output $@

$(MIP_FLUXES): $(3_INVERSION_SOURCE)/mip-fluxes.R $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--mip-fluxes-directory $(MIP_FLUXES_DIRECTORY) \
		--output $@

$(MEASUREMENT_MODEL): $(3_INVERSION_SOURCE)/measurement-model.R $(OBSERVATIONS) $(PROCESS_MODEL) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--observations $(OBSERVATIONS) \
		--process-model $(PROCESS_MODEL) \
		--output $@

$(PROCESS_MODEL): $(3_INVERSION_SOURCE)/process-model.R $(CONTROL_MOLE_FRACTION) $(SENSITIVITIES) $(CONTROL_EMISSIONS) $(PERTURBATIONS) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--control-emissions $(CONTROL_EMISSIONS) \
		--perturbations $(PERTURBATIONS) \
		--control-mole-fraction $(CONTROL_MOLE_FRACTION) \
		--sensitivities $(SENSITIVITIES) \
		--output $@

$(OBSERVATIONS): $(3_INVERSION_SOURCE)/observations.R $(CONTROL_MOLE_FRACTION) $(OCO2_DATA) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--control $(CONTROL_MOLE_FRACTION) \
		--oco2-observations $(OCO2_DATA) \
		--tccon-observation-directory $(TCCON_DATA) \
		--obspack-directory $(OBSPACK_DATA) \
		--start-date $(INVERSION_START_DATE) \
		--end-date $(INVERSION_END_DATE) \
		--output $@

$(SENSITIVITIES): $(3_INVERSION_SOURCE)/sensitivities.R $(CONTROL_MOLE_FRACTION) $(GEOS_CHEM_MATCHED_RUNS_LAST_COMBINED) $(INVERSION_BASE_PARTIAL) $(UTILS_PARTIAL)
	Rscript $< \
		--control $(CONTROL_MOLE_FRACTION) \
		--matched-runs-directory $(GEOS_CHEM_MATCHED_RUNS) \
		--output $@

$(CONTROL_MOLE_FRACTION): $(3_INVERSION_SOURCE)/control-mole-fraction.R $(GEOS_CHEM_MATCHED_RUNS_LAST_COMBINED) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--matched-runs-directory $(GEOS_CHEM_MATCHED_RUNS) \
		--obspack-directory $(OBSPACK_DATA) \
		--start-date $(INVERSION_START_DATE) \
		--end-date $(INVERSION_END_DATE) \
		--output $@

$(PERTURBATIONS): $(3_INVERSION_SOURCE)/perturbations.R $(CONTROL_EMISSIONS) $(GEOS_CHEM_MATCHED_RUNS_LAST_FLUX_AGGREGATED) $(INVERSION_BASE_PARTIAL) $(UTILS_PARTIAL)
	Rscript $< \
		--control $(CONTROL_EMISSIONS) \
		--matched-runs-directory $(GEOS_CHEM_MATCHED_RUNS) \
		--output $@

$(CONTROL_EMISSIONS): $(3_INVERSION_SOURCE)/control-emissions.R $(GEOS_CHEM_MATCHED_RUNS_LAST_FLUX_AGGREGATED) $(INVERSION_BASE_PARTIAL)
	Rscript $< \
		--matched-runs-directory $(GEOS_CHEM_MATCHED_RUNS) \
		--start-date $(INVERSION_START_DATE) \
		--end-date $(INVERSION_END_DATE) \
		--output $@