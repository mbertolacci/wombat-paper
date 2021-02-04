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

lint: $(LINT_TARGETS)
clean: $(CLEAN_TARGETS)

.SECONDARY: $(SECONDARY_TARGETS)
