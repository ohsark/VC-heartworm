# =============================================================================
# Title:        Data Cleaning and Preprocessing
# Author:       Sahil Arora
# Description:  Ingests raw Phase 1/2 consultation CSVs, removes duplicates,
#               collapses repeated text fields, builds patient-level IDs, and
#               retains post-1999 consultations. Adds weekly grouping,
#               time-between-consults metrics, and TestTakenBefore counts.
#
# Inputs:       - Phase 1a/1b CSVs (raw consultation data)
#               - Phase 2a/2b CSVs (preventative/follow-up data)
#               - VCA_ClinicCode-LGA.xlsx (clinic metadata)
#
# Outputs:      - master_HW_data.rda (main heartworm test data)
#               - master_HW_prev_data.rda (preventative care data)
#
# Dependencies: tidyverse, readxl, lubridate, janitor
# =============================================================================

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library(janitor)
})

# -----------------------------------------------------------------------------
# Source Configuration (Paths)
# -----------------------------------------------------------------------------
source("./config/paths.R")

# -----------------------------------------------------------------------------
# File Paths
# -----------------------------------------------------------------------------
# NOTE: Using centralized paths from config. Adjust paths.R for your environment.

phase1_paths <- c(PHASE1A_PATH, PHASE1B_PATH)
phase2_paths <- c(PHASE2A_PATH, PHASE2B_PATH)
clinic_codes_path <- CLINIC_CODES_PATH
output_dir <- DATA_PROCESSED_DIR

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Load and combine phase data files
#'
#' Reads multiple CSV files, combines them, removes exact duplicates,
#' and standardizes the patient ID column name.
#'
#' @param paths Character vector of file paths to CSV files
#' @param id_col Name of the patient ID column in source data
#' @return Combined and deduplicated tibble
load_phase <- function(paths, id_col = "patientnumber") {
  paths %>%
    purrr::map(~ readr::read_csv(.x, show_col_types = FALSE)) %>%
    list_rbind() %>%
    distinct() %>%
    rename(PatientNumber = all_of(id_col))
}

#' Clean and process consultation records
#'
#' Applies comprehensive cleaning including:
#' - Date parsing and extraction (Month, Year)
#' - Unique consultation ID creation
#' - Filtering records before year 2000
#' - Collapsing duplicate examination text
#' - Computing time between consultations
#' - Weekly grouping for deduplication
#' - TestTakenBefore counter per patient
#'
#' @param data Raw consultation tibble
#' @return Cleaned tibble with derived fields
clean_consults <- function(data) {
  data %>%
    # Parse dates and create unique consultation identifier
    mutate(
      ConsultationDate = lubridate::as_datetime(ConsultationDate),
      Date = lubridate::date(ConsultationDate),
      Month = lubridate::month(Date),
      Year = lubridate::year(Date),
      id = str_c(DatabaseName, ClinicCode, PatientNumber, ConsultationNumber, sep = "_")
    ) %>%
    # Filter out records before year 2000
    filter(Year > 1999) %>%
    # Collapse repeated text fields within same consultation
    group_by(id) %>%
    mutate(
      ExaminationText = paste0(unique(ExaminationText), collapse = " "),
      ItemName = paste0(unique(ItemName), collapse = " "),
      ItemLabel = paste0(unique(ItemLabel), collapse = " ")
    ) %>%
    arrange(ConsultationDate) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    # Remove item columns (now collapsed into ExaminationText)
    select(-ItemName, -ItemLabel) %>%
    distinct() %>%
    # Calculate time metrics per patient
    group_by(DatabaseName, ClinicCode, PatientNumber) %>%
    arrange(ConsultationDate) %>%
    mutate(
      time_since_last_test = as.numeric(floor(difftime(
        ConsultationDate,
        lag(ConsultationDate, default = first(ConsultationDate)),
        units = "hours"
      ))),
      time_since_last_consult = cumsum(time_since_last_test),
      grp = cut.Date(as.Date(ConsultationDate), breaks = "1 week", labels = FALSE)
    ) %>%
    ungroup() %>%
    distinct() %>%
    # Remove duplicate examination text within weekly windows
    distinct(grp, ExaminationText, .keep_all = TRUE) %>%
    distinct(ExaminationText, .keep_all = TRUE) %>%
    # Further weekly deduplication
    group_by(DatabaseName, ClinicCode, PatientNumber, grp) %>%
    mutate(ExaminationText = paste0(ExaminationText, collapse = " ")) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    # Count prior tests for each patient
    group_by(DatabaseName, ClinicCode, PatientNumber) %>%
    mutate(TestTakenBefore = dplyr::row_number() - 1) %>%
    ungroup() %>%
    # Final deduplication
    group_by(id) %>%
    arrange(ConsultationDate) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    # Remove empty examination text
    filter(!is.na(ExaminationText), ExaminationText != "", ExaminationText != "NULL")
}

# -----------------------------------------------------------------------------
# Main Processing
# -----------------------------------------------------------------------------

# Load raw phase data
hw_data_raw <- load_phase(phase1_paths)
hw_prev_raw <- load_phase(phase2_paths)

# Load clinic codes lookup
clinic_codes <- read_xlsx(clinic_codes_path, sheet = 1) %>%
  rename(ClinicCode = "Clinic Code") %>%
  filter(ClinicCode > 2)  # Filter valid clinic codes

# Clean and join with clinic information
master_hw_data <- clean_consults(hw_data_raw) %>%
  left_join(clinic_codes, by = "ClinicCode")

master_hw_prev_data <- clean_consults(hw_prev_raw) %>%
  left_join(clinic_codes, by = "ClinicCode")

# -----------------------------------------------------------------------------
# Data Quality Checks
# -----------------------------------------------------------------------------

# Check for any remaining duplicates
janitor::get_dupes(master_hw_data, id)
janitor::get_dupes(master_hw_prev_data, id)
janitor::get_dupes(bind_rows(hw_data_raw, hw_prev_raw))

# -----------------------------------------------------------------------------
# Save Outputs
# -----------------------------------------------------------------------------

# Create output directory if needed
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save cleaned datasets
save(master_hw_data, file = MASTER_HW_DATA_PATH)
save(master_hw_prev_data, file = MASTER_HW_PREV_DATA_PATH)

message("Data cleaning complete. Files saved to: ", output_dir)
