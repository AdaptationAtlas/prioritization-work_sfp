
RESULTS_DIR = data/results
SRC_DIR = src

# R script outputs
BOUNDS   = $(RESULTS_DIR)/bounds_area.parquet
URGENCY  = $(RESULTS_DIR)/urgency.parquet
GRAPE    = $(RESULTS_DIR)/grape.parquet
AOW1     = $(RESULTS_DIR)/aow1_need.parquet
AOW2     = $(RESULTS_DIR)/aow2_shocks.parquet
AOW3     = $(RESULTS_DIR)/aow3_capacity.parquet

R_OUTPUTS = $(BOUNDS) $(URGENCY) $(GRAPE) $(AOW1) $(AOW2) $(AOW3)

# QMD outputs
QMD_OUTPUTS = country_analysis.pdf sfp_report_REFACTOR.pdf

# Default target
all: $(R_OUTPUTS) $(QMD_OUTPUTS)

# Rules for R scripts
$(BOUNDS): $(SRC_DIR)/01_preprocess_bounds.r
	Rscript $< -o $@ >/dev/null

$(URGENCY): $(SRC_DIR)/02_preprocess_urgency.r $(BOUNDS)
	Rscript $< -o $@ >/dev/null

$(GRAPE): $(SRC_DIR)/03_preprocess_grape.r $(URGENCY)
	Rscript $< -o $@ >/dev/null

$(AOW1): $(SRC_DIR)/04_preprocess_aow1.r $(BOUNDS)
	Rscript $< -o $@ >/dev/null

$(AOW2): $(SRC_DIR)/05_preprocess_aow2.r $(BOUNDS)
	Rscript $< -o $@ >/dev/null

$(AOW3): $(SRC_DIR)/06_preprocess_aow3.r $(BOUNDS)
	Rscript $< -o $@ >/dev/null

country_analysis.pdf: country_analysis.qmd $(R_OUTPUTS)
	quarto render $<

sfp_report.pdf: sfp_report.qmd $(R_OUTPUTS)
	quarto render $<

# Utility
clean:
	rm -f $(RESULTS_DIR)/*.parquet

.PHONY: all clean
