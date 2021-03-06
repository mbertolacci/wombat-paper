# Each module adds to this
RUN_TARGETS=
LINT_TARGETS=
CLEAN_TARGETS=

# The start date is inclusive, the end date is not
INVERSION_START_DATE = 2014-09-01
INVERSION_END_DATE = 2017-04-01
SENSITIVITY_REGIONS = `seq 1 22`
# In months
SENSITIVITY_DURATION = 4

DATA = data
EXTERNAL = external
1_TRANSPORT = 1_transport
2_MATCHING = 2_matching
3_INVERSION = 3_inversion
4_RESULTS = 4_results

TRANSCOM_MAP = $(DATA)/TRANSCOM_Map_MZT.nc
TRANSCOM_MASK = $(DATA)/TRANSCOM_mask.mozart.1.875x2.5.nc
GEOS_CHEM_SOURCE = $(EXTERNAL)/GEOS_Chem/Code.v12.3.2

UTILS_PARTIAL = partials/utils.R
export UTILS_PARTIAL

all: 1_transport_targets 2_matching_targets 3_inversion_targets 4_results_targets

include $(1_TRANSPORT)/module.mk
include $(2_MATCHING)/module.mk
include $(3_INVERSION)/module.mk
include $(4_RESULTS)/module.mk

1_transport_targets: $(1_TRANSPORT_TARGETS)
2_matching_targets: $(2_MATCHING_TARGETS)
3_inversion_targets: $(3_INVERSION_TARGETS)
4_results_targets: $(4_RESULTS_TARGETS)

wombat-inversion-intermediates.tar.gz:
	tar -c -z -f $@ \
	    3_inversion/intermediates/control-emissions.fst \
	    3_inversion/intermediates/control-mole-fraction.fst \
	    3_inversion/intermediates/perturbations.fst \
	    3_inversion/intermediates/osse-anomaly.fst \
	    3_inversion/intermediates/sensitivities.fst

wombat-blinded-code.tar.gz:
	rm -rf wombat-paper
	rsync \
	    --exclude .git \
	    --exclude .DS_Store \
	    --exclude intermediates \
	    --exclude figures \
	    -a \
	    ./ \
	    wombat-paper/
	find \
		wombat-paper \
		-type f \
		-not -path \*.png \
		-not -path \*.nc \
		-not -path \*.nc4 \
		| xargs sed -i "" \
        -e "s/mgnb/anon/g; \
			s/Michael/Anon/g; \
			s/Bertolacci/Anon/g; \
			s/m.bertolacci@gmail.com//g; \
			s/Andrew/Anon/g; \
			s/Zammit-Mangion/Anon/g; \
			s/Noel/Anon/g; \
			s/Cressie/Anon/g; \
			s/Yi/Anon/g; \
			s/Cao/Anon/g; \
			s/WOllongong//g; \
			s/0000-0003-0317-5941//g; \
			s/https:\/\/arxiv\.org\/abs\/2102.04004//g; \
			s/.*BLIND_LINE.*//g"
	tar czf $@ wombat-paper
	rm -r wombat-paper

lint: $(LINT_TARGETS)
clean: $(CLEAN_TARGETS)

.SECONDARY: $(SECONDARY_TARGETS)
