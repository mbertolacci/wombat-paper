4_RESULTS_INTERMEDIATES = $(4_RESULTS)/intermediates
4_RESULTS_SOURCE = $(4_RESULTS)/src
4_RESULTS_FIGURES = $(4_RESULTS)/figures

$(shell mkdir -p $(4_RESULTS_INTERMEDIATES) $(4_RESULTS_FIGURES))

RESULTS_BASE_PARTIAL = $(4_RESULTS_SOURCE)/partials/base.R
export RESULTS_BASE_PARTIAL
RESULTS_DISPLAY_PARTIAL = $(4_RESULTS_SOURCE)/partials/display.R
export RESULTS_DISPLAY_PARTIAL
RESULTS_TABLES_PARTIAL = $(4_RESULTS_SOURCE)/partials/tables.R
export RESULTS_TABLES_PARTIAL
RESULTS_FLUX_AGGREGATES_PARTIAL = $(4_RESULTS_SOURCE)/partials/flux-aggregates.R
export RESULTS_FLUX_AGGREGATES_PARTIAL

4_RESULTS_TARGETS = $(4_RESULTS_FIGURES)/osse-bias-correlated-land-ocean.pdf \
	$(4_RESULTS_FIGURES)/osse-bias-correlated-tropics.pdf \
	$(4_RESULTS_FIGURES)/osse-bias-correlated-t04-t06.pdf \
	$(4_RESULTS_FIGURES)/osse-bias-correlated-table.tex \
	$(4_RESULTS_FIGURES)/flux-aggregates-table.txt \
	$(4_RESULTS_FIGURES)/flux-aggregates-globals.pdf \
	$(4_RESULTS_FIGURES)/flux-aggregates-zonal.pdf \
	$(4_RESULTS_FIGURES)/tccon-time-series.pdf \
	$(4_RESULTS_FIGURES)/tccon-metrics.pdf \
	$(4_RESULTS_FIGURES)/tccon-mse-table.tex \
	$(4_RESULTS_FIGURES)/tccon-mse-table-raw.txt \
	$(4_RESULTS_FIGURES)/oco2-bias-correction.pdf \
	$(4_RESULTS_FIGURES)/sensitivities.pdf \
	$(4_RESULTS_FIGURES)/hyperparameter-kappa-tau.pdf \
	$(4_RESULTS_FIGURES)/hyperparameter-table-raw.txt \
	$(4_RESULTS_FIGURES)/hyperparameter-table-rho-ell-gamma.tex \
	$(4_RESULTS_FIGURES)/region-map.pdf \
	$(4_RESULTS_FIGURES)/region-table.tex \
	$(4_RESULTS_FIGURES)/flux-aggregates-bias-globals.pdf \
	$(4_RESULTS_FIGURES)/flux-aggregates-rho0-globals.pdf \
	$(4_RESULTS_FIGURES)/flux-aggregates-rho0-mip-globals.pdf

SECONDARY_TARGETS += $(4_RESULTS_INTERMEDIATES)/tccon-metric-df.fst \
	$(4_RESULTS_INTERMEDIATES)/tccon-samples.rds \
	$(4_RESULTS_INTERMEDIATES)/mip-tccon.fst

## Figures

$(4_RESULTS_FIGURES)/sensitivities.pdf: \
	$(4_RESULTS_SOURCE)/sensitivities.R \
	$(CONTROL_EMISSIONS) \
	$(PERTURBATIONS) \
	$(4_RESULTS_INTERMEDIATES)/xco2-daily-base.nc \
	$(4_RESULTS_INTERMEDIATES)/xco2-daily-201601R02.nc \
	$(4_RESULTS_INTERMEDIATES)/xco2-daily-201601R06.nc \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL)
	Rscript $< \
		--control-emissions $(CONTROL_EMISSIONS) \
		--perturbations $(PERTURBATIONS) \
		--xco2-daily-base $(4_RESULTS_INTERMEDIATES)/xco2-daily-base.nc \
		--xco2-daily-201601r02 $(4_RESULTS_INTERMEDIATES)/xco2-daily-201601R02.nc \
		--xco2-daily-201601r06 $(4_RESULTS_INTERMEDIATES)/xco2-daily-201601R06.nc \
		--output $@

$(4_RESULTS_FIGURES)/region-map.pdf: \
	$(4_RESULTS_SOURCE)/region-map.R \
	$(TRANSCOM_MAP) \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL)
	Rscript $< \
		--transcom-map $(TRANSCOM_MAP) \
		--output $@

$(4_RESULTS_FIGURES)/region-table.tex: \
	$(4_RESULTS_SOURCE)/region-table.R \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL)
	Rscript $< \
		--output $@

$(4_RESULTS_FIGURES)/oco2-bias-correction.pdf: \
	$(4_RESULTS_SOURCE)/oco2-bias-correction.R \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LGr-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LNr-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LG-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LN-FIXEDAO-FIXEDWO5.rds \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL)
	Rscript $< \
		--mcmc-samples-lg-online-corrected $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LGr-FIXEDAO-FIXEDWO5.rds \
		--mcmc-samples-ln-online-corrected $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LNr-FIXEDAO-FIXEDWO5.rds \
		--mcmc-samples-lg-offline-online-corrected $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LG-FIXEDAO-FIXEDWO5.rds \
		--mcmc-samples-ln-offline-online-corrected $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LN-FIXEDAO-FIXEDWO5.rds \
		--output $@

$(4_RESULTS_FIGURES)/hyperparameter-kappa-tau.pdf: \
	$(4_RESULTS_SOURCE)/hyperparameter-kappa-tau.R \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL)
	Rscript $< \
		--mcmc-samples-lg $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--mcmc-samples-ln $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--output $@

$(4_RESULTS_FIGURES)/hyperparameter-table-raw.txt: \
	$(4_RESULTS_SOURCE)/hyperparameter-table-raw.R \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(RESULTS_BASE_PARTIAL)
	Rscript $< \
		--mcmc-samples-lg $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--mcmc-samples-ln $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--output $@

$(4_RESULTS_FIGURES)/hyperparameter-table-rho-ell-gamma.tex: \
	$(4_RESULTS_SOURCE)/hyperparameter-table-rho-ell-gamma.R \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(RESULTS_BASE_PARTIAL)
	Rscript $< \
		--mcmc-samples-lg $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--mcmc-samples-ln $(3_INVERSION_INTERMEDIATES)/real-mcmc-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--output $@

# OSSE plots

$(4_RESULTS_FIGURES)/osse-bias-correlated-land-ocean.pdf: \
	$(4_RESULTS_SOURCE)/osse-bias-correlated-bivariate.R \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL)
	Rscript $< \
		--region1 "Global land" \
		--region2 "Global oceans" \
		--flux-samples-lg-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-no-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-no-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-no-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-no-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--height 14 \
		--output $@

$(4_RESULTS_FIGURES)/osse-bias-correlated-tropics.pdf: \
	$(4_RESULTS_SOURCE)/osse-bias-correlated-bivariate.R \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL)
	Rscript $< \
		--region1 "S tropics (-23.5 - 0)" \
		--region2 "N tropics (0 - 23.5)" \
		--flux-samples-lg-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-no-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-no-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-no-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-no-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--height 18 \
		--output $@

$(4_RESULTS_FIGURES)/osse-bias-correlated-t04-t06.pdf: \
	$(4_RESULTS_SOURCE)/osse-bias-correlated-bivariate.R \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL)
	Rscript $< \
		--region1 "T04" \
		--region2 "T06" \
		--flux-samples-lg-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-no-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-no-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-no-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-no-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--height 18.5 \
		--output $@

$(4_RESULTS_FIGURES)/osse-bias-correlated-table.tex: \
	$(4_RESULTS_SOURCE)/osse-bias-correlated-table.R \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL)
	Rscript $< \
		--flux-samples-lg-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-no-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-no-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-lg-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LG-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-no-bias-correlated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-no-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-NOBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-bias-uncorrelated $(3_INVERSION_INTERMEDIATES)/osse-flux-aggregates-samples-random1-LN-TRUEBIAS-RHO0-FIXEDAO-FIXEDWO5.rds \
		--output $@

# TCCON related plots

$(4_RESULTS_FIGURES)/tccon-metrics.pdf: \
	$(4_RESULTS_SOURCE)/tccon-metrics.R \
	$(4_RESULTS_INTERMEDIATES)/tccon-metric-df.fst \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL)
	Rscript $< \
		--tccon-metric-df $(4_RESULTS_INTERMEDIATES)/tccon-metric-df.fst \
		--output $@

$(4_RESULTS_FIGURES)/tccon-time-series.pdf: \
	$(4_RESULTS_SOURCE)/tccon-time-series.R \
	$(4_RESULTS_INTERMEDIATES)/tccon-samples.rds \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL)
	Rscript $< \
		--tccon-samples $(4_RESULTS_INTERMEDIATES)/tccon-samples.rds \
		--output $@

$(4_RESULTS_FIGURES)/tccon-mse-table.tex: $(4_RESULTS_SOURCE)/tccon-mse-table.R \
	$(4_RESULTS_INTERMEDIATES)/tccon-metric-df.fst \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL)
	Rscript $< \
		--tccon-metric-df $(4_RESULTS_INTERMEDIATES)/tccon-metric-df.fst \
		--output $@	

$(4_RESULTS_FIGURES)/tccon-mse-table-raw.txt: $(4_RESULTS_SOURCE)/tccon-mse-table-raw.R \
	$(4_RESULTS_INTERMEDIATES)/mip-tccon.fst \
	$(4_RESULTS_INTERMEDIATES)/tccon-samples.rds \
	$(RESULTS_BASE_PARTIAL)
	Rscript $< \
		--mip-tccon $(4_RESULTS_INTERMEDIATES)/mip-tccon.fst \
		--tccon-samples $(4_RESULTS_INTERMEDIATES)/tccon-samples.rds \
		--output $@

# Flux aggregates

$(4_RESULTS_FIGURES)/flux-aggregates-table.txt: \
	$(4_RESULTS_SOURCE)/flux-aggregates-table.R \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst \
	$(RESULTS_BASE_PARTIAL)
	Rscript $< \
		--flux-samples-lg $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-ln $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--mip-fluxes $(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst \
		--output $@

$(4_RESULTS_FIGURES)/flux-aggregates-globals.pdf: \
	$(4_RESULTS_SOURCE)/flux-aggregates.R \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL) \
	$(RESULTS_FLUX_AGGREGATES_PARTIAL)
	Rscript $< \
		--region Global "Global land" "Global oceans" \
		--flux-samples-lg $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-ln $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--show-mip-fluxes \
		--mip-fluxes $(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst \
		--height 19 \
		--output $@

$(4_RESULTS_FIGURES)/flux-aggregates-zonal.pdf: \
	$(4_RESULTS_SOURCE)/flux-aggregates.R \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL) \
	$(RESULTS_FLUX_AGGREGATES_PARTIAL)
	Rscript $< \
		--region 'N extratropics (23.5 - 90)' 'N tropics (0 - 23.5)' 'S tropics (-23.5 - 0)' 'S extratropics (-90 - -23.5)' \
		--flux-samples-lg $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-ln $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--show-mip-fluxes \
		--mip-fluxes $(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst \
		--height 20.5 \
		--output $@

$(4_RESULTS_FIGURES)/flux-aggregates-rho0-globals.pdf: \
	$(4_RESULTS_SOURCE)/flux-aggregates-rho0.R \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL) \
	$(RESULTS_FLUX_AGGREGATES_PARTIAL)
	Rscript $< \
		--region Global "Global land" "Global oceans" \
		--flux-samples-lg $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-ln $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-lg-uncorrelated $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-ln-uncorrelated $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--height 19 \
		--output $@

$(4_RESULTS_FIGURES)/flux-aggregates-rho0-mip-globals.pdf: \
	$(4_RESULTS_SOURCE)/flux-aggregates-rho0.R \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL) \
	$(RESULTS_FLUX_AGGREGATES_PARTIAL)
	Rscript $< \
		--region Global "Global land" "Global oceans" \
		--flux-samples-lg $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-ln $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-lg-uncorrelated $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-ln-uncorrelated $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--show-mip-fluxes \
		--mip-fluxes $(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst \
		--height 19 \
		--output $@

$(4_RESULTS_FIGURES)/flux-aggregates-bias-globals.pdf: \
	$(4_RESULTS_SOURCE)/flux-aggregates-bias.R  \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LGr-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LNr-FIXEDAO-FIXEDWO5.rds \
	$(3_INVERSION_INTERMEDIATES)/mip-fluxes.fst \
	$(RESULTS_BASE_PARTIAL) \
	$(RESULTS_DISPLAY_PARTIAL) \
	$(RESULTS_TABLES_PARTIAL) \
	$(RESULTS_FLUX_AGGREGATES_PARTIAL)
	Rscript $< \
		--region Global "Global land" "Global oceans" \
		--flux-samples-lg $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-ln $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--flux-samples-lg-online-corrected $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LGr-FIXEDAO-FIXEDWO5.rds \
		--flux-samples-ln-online-corrected $(3_INVERSION_INTERMEDIATES)/real-flux-aggregates-samples-LNr-FIXEDAO-FIXEDWO5.rds \
		--height 19 \
		--output $@

## Intermediates

$(4_RESULTS_INTERMEDIATES)/tccon-metric-df.fst: \
	$(4_RESULTS_SOURCE)/tccon-metric-df.R \
	$(4_RESULTS_INTERMEDIATES)/tccon-samples.rds \
	$(4_RESULTS_INTERMEDIATES)/mip-tccon.fst \
	$(RESULTS_BASE_PARTIAL)
	Rscript $< \
		--tccon-samples $(4_RESULTS_INTERMEDIATES)/tccon-samples.rds \
		--mip-tccon $(4_RESULTS_INTERMEDIATES)/mip-tccon.fst \
		--output $@

$(4_RESULTS_INTERMEDIATES)/tccon-samples.rds: \
	$(4_RESULTS_SOURCE)/tccon-samples.R \
	$(3_INVERSION_INTERMEDIATES)/real-tccon-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-tccon-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-tccon-samples-LG-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(3_INVERSION_INTERMEDIATES)/real-tccon-samples-LN-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
	$(RESULTS_BASE_PARTIAL)
	Rscript $< \
		--tccon-samples-lg $(3_INVERSION_INTERMEDIATES)/real-tccon-samples-LG-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--tccon-samples-ln $(3_INVERSION_INTERMEDIATES)/real-tccon-samples-LN-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--tccon-samples-lg-uncorrelated $(3_INVERSION_INTERMEDIATES)/real-tccon-samples-LG-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--tccon-samples-ln-uncorrelated $(3_INVERSION_INTERMEDIATES)/real-tccon-samples-LN-RHO0-FIXEDAO-FIXEDWO5-NOBIAS.rds \
		--output $@

$(4_RESULTS_INTERMEDIATES)/mip-tccon.fst: \
	$(4_RESULTS_SOURCE)/mip-tccon.R \
	$(3_INVERSION_INTERMEDIATES)/mip-tccon-residual.fst \
	$(RESULTS_BASE_PARTIAL)
	Rscript $< \
		--mip-tccon-residual $(3_INVERSION_INTERMEDIATES)/mip-tccon-residual.fst \
		--output $@

$(4_RESULTS_INTERMEDIATES)/xco2-daily-base.nc: 2_matching/intermediates/GEOS_Chem/matched-runs/run.v12.3.2.base/xco2-3hr.nc
	cdo -z zip_6 -daymean $< $@

$(4_RESULTS_INTERMEDIATES)/xco2-daily-201601R02.nc: 2_matching/intermediates/GEOS_Chem/matched-runs/2016/run.v12.3.2.R02M01/xco2-3hr.nc
	cdo -z zip_6 -daymean $< $@

$(4_RESULTS_INTERMEDIATES)/xco2-daily-201601R06.nc: 2_matching/intermediates/GEOS_Chem/matched-runs/2016/run.v12.3.2.R06M01/xco2-3hr.nc
	cdo -z zip_6 -daymean $< $@
