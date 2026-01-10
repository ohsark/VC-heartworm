# =============================================================================
# Title:        Centralized Path Configuration
# Author:       [Your Name]
# Date:         [Date]
# Description:  Central configuration file for all file paths used in the
#               heartworm analysis pipeline. Modify paths here to adapt the
#               pipeline to different environments.
#
# Usage:        source("src/config/paths.R") at the start of each script
# =============================================================================

# -----------------------------------------------------------------------------
# Base Directories
# -----------------------------------------------------------------------------
# Adjust these base paths according to your local environment

BASE_DIR <- getwd()  # Assumes scripts run from FINAL_CODE directory

# Raw data directory (input CSVs, Excel files)
DATA_RAW_DIR <- file.path(BASE_DIR, "data", "raw")

# Processed data directory (intermediate RDA files)
DATA_PROCESSED_DIR <- file.path(BASE_DIR, "data", "processed")

# Output directory (JSON, Excel, figures)
DATA_OUTPUT_DIR <- file.path(BASE_DIR, "data", "outputs")

# Shapefiles directory
SHAPEFILES_DIR <- file.path(DATA_RAW_DIR, "shapefiles")

# -----------------------------------------------------------------------------
# Input File Paths
# -----------------------------------------------------------------------------

# Phase 1 consultation data
PHASE1A_PATH <- file.path(DATA_RAW_DIR, "154_SEF003G200_20231123_Phase1a.csv")
PHASE1B_PATH <- file.path(DATA_RAW_DIR, "154_SEF003G200_20231123_Phase1b.csv")

# Phase 2 preventative/follow-up data
PHASE2A_PATH <- file.path(DATA_RAW_DIR, "154_SEF003H200_20231128_Phase2a.csv")
PHASE2B_PATH <- file.path(DATA_RAW_DIR, "154_SEF003H200_20231128_Phase2b.csv")

# Clinic codes lookup
CLINIC_CODES_PATH <- file.path(DATA_RAW_DIR, "VCA_ClinicCode-LGA.xlsx")

# Deceased dates
DECEASED_DATES_PATH <- file.path(DATA_RAW_DIR, "154_SEF003H200_20231128_Phase1_PatientDeceasedDate.csv")

# Shapefiles
POSTCODE_SHAPEFILE <- file.path(SHAPEFILES_DIR, "Postcode", "POA_2016_AUST.shp")
LGA_SHAPEFILE <- file.path(SHAPEFILES_DIR, "LGA", "LGA_2020_AUST.shp")

# -----------------------------------------------------------------------------
# Output File Paths
# -----------------------------------------------------------------------------

# Cleaned data outputs
MASTER_HW_DATA_PATH <- file.path(DATA_PROCESSED_DIR, "master_HW_data.rda")
MASTER_HW_PREV_DATA_PATH <- file.path(DATA_PROCESSED_DIR, "master_HW_prev_data.rda")

# Classified data
HW_CLASSIFIED_PATH <- file.path(DATA_PROCESSED_DIR, "hw_data_classified_FINAL.rda")

# Final processed data
HW_FINAL_PATH <- file.path(DATA_OUTPUT_DIR, "hw_final.rda")

# JSON outputs for dashboard
JSON_POSTCODE_SUMMARIES <- file.path(DATA_OUTPUT_DIR, "PostcodeSummaries.json")
JSON_YEARLY_TOTALS <- file.path(DATA_OUTPUT_DIR, "yearly_totals.json")
JSON_POSTCODE_TOTALS <- file.path(DATA_OUTPUT_DIR, "postcode_totals.json")

# Excel output
EXCEL_HW_POSTCODE <- file.path(DATA_OUTPUT_DIR, "hw_postcode.xlsx")

# -----------------------------------------------------------------------------
# Helper: Create directories if they don't exist
# -----------------------------------------------------------------------------
create_dirs <- function() {
  dirs <- c(DATA_RAW_DIR, DATA_PROCESSED_DIR, DATA_OUTPUT_DIR, SHAPEFILES_DIR)
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE)
      message("Created directory: ", d)
    }
  }
}

# Uncomment to auto-create directories when sourcing this file
# create_dirs()
