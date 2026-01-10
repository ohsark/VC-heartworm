# =============================================================================
# Title:        Heartworm Test Rule-based Classification
# Author:       Sahil Arora
# Description:  Merges cleaned consultations with tokenized text and classifies
#               each record into heartworm status, prevention, treatment,
#               symptoms, and testing flags. Appends age and death data.
#
# Inputs:       - master_hw_data, master_hw_prev_data (from 01_Data_cleaning.R)
#               - tokenized_dat, tokenized_dat_prev (tokenized text - loaded externally)
#               - Deceased dates CSV
#               - Pattern vectors from 02.1_classifications.R
#
# Outputs:      - hw_data_classified_FINAL.rda (fully classified dataset)
#
# Dependencies: tidyverse
#
# Logic:        1. Merge consultation data with tokenized text
#               2. Apply regex-based classification rules
#               3. Derive heartworm status from positive/negative/mf phrases
#               4. Impute indeterminate/suspect/probable/treated status
#               5. Calculate patient age and merge deceased dates
# =============================================================================

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))

# -----------------------------------------------------------------------------
# Source Dependencies
# -----------------------------------------------------------------------------
source("./config/paths.R")
source("./config/02.1_classifications.R")

# NOTE: The following objects must be loaded in memory before running:
# - master_hw_data (from 01_Data_cleaning.R)
# - master_hw_prev_data (from 01_Data_cleaning.R)
# - tokenized_dat (tokenized examination text for Phase 1)
# - tokenized_dat_prev (tokenized examination text for Phase 2)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Merge tokenized text with consultation data
#'
#' Combines Phase 1 and Phase 2 data with their respective tokenized text,
#' avoiding duplicate records. Cleans the examination text by removing
#' the word "low" which can cause classification issues.
#'
#' @return Combined tibble with tokenized text
merge_tokenized <- function() {
  bind_rows(
    # Phase 1 data
    master_hw_data %>%
      select(-DeceasedDate, -ConsultationType) %>%
      left_join(tokenized_dat, by = "id"),
    # Phase 2 data (excluding records already in Phase 1)
    master_hw_prev_data %>%
      filter(!(id %in% master_hw_data$id)) %>%
      select(-PatientDeceasedDate, -ConsultationDeceasedDate) %>%
      left_join(tokenized_dat_prev, by = "id")
  ) %>%
    # Clean text: remove "low" which interferes with classification
    mutate(cleaned_examin_text = stringr::str_squish(stringr::str_replace_all(cleaned_examin_text, "low", "")))
}

#' Classify heartworm records
#'
#' Applies multi-step classification logic to determine:
#' - hw_test: Whether a heartworm test was conducted
#' - preventative: ProHeart injection detection
#' - treatment: Immiticide vs SlowKill protocols
#' - symptoms: Caval syndrome/severe/moderate/mild scoring
#' - heartworm: Final status (positive/negative/indeterminate/suspect/probable/treated)
#'
#' @param data Tibble with cleaned_examin_text column
#' @return Classified tibble with new status columns
classify_records <- function(data) {
  data %>%
    mutate(
      # ---------------------------------------------------------------------
      # Detect if heartworm test was conducted
      # ---------------------------------------------------------------------
      hw_test = if_else(
        stringr::str_detect(cleaned_examin_text, paste0(hw_test, collapse = "|")),
        "conducted",
        "NA"
      ),

      # ---------------------------------------------------------------------
      # Classify preventative treatment (ProHeart injection)
      # ---------------------------------------------------------------------
      preventative = case_when(
        # Exclude advisory/recommendation contexts
        stringr::str_detect(cleaned_examin_text, paste0(no_preventions, collapse = "|")) ~ "NA",
        # Detect ProHeart or similar preventative injections
        stringr::str_detect(
          cleaned_examin_text,
          "(?<!advised\\s|advise\\s|discuss\\s|discussed\\s|option\\s|recommend\\s|recheck in month for\\s|recommended\\s)heartworm_preventative|pro heart|subcutaneous heartworm injection|heartworm injection subcutaneous|(?<!advised\\s|advise\\s|discuss\\s|discussed\\s|option\\s|recommend\\s|recommended\\s)heartworm injection|heartworm prevention injection"
        ) ~ "proheart",
        .default = "NA"
      ),

      # ---------------------------------------------------------------------
      # Classify treatment protocol
      # ---------------------------------------------------------------------
      treatment = case_when(
        # Immiticide (melarsomine) treatment
        stringr::str_detect(
          cleaned_examin_text,
          "(?<!advised\\s|advise\\s|problems\\s|to start\\s|to have\\s|consider\\|order\\s|had\\s|after\\s|history treated\\s|suggested\\s|wish to treat\\s|considering\\s|before\\s|need\\s|hospitalize for\\s|previous\\s|previous treatment\\s|followup\\s|mentioned\\s|swelling\\s|prob\\s|since\\s|after\\s|after last\\s|discuss\\s|discussed\\s|option\\s|to start\\s|post\\s|before\\s|hospitalize for\\s|had\\s)immiticide(?!\\s(react|reaction|in wk|protocol|finished))|(?<!advised\\s|advise\\s|discuss\\s|discussed\\s|option\\s|to start\\s|post\\s|before\\s|hospitalize for\\s|had\\s)melarsomine(?!\\s(react|reaction))"
        ) ~ "immiticide",
        # Slow-kill protocol (doxycycline-based)
        stringr::str_detect(
          cleaned_examin_text,
          "(?<!advised\\s|advise\\s|discuss\\s|discussed\\s|protocol\\s|history\\s|on\\s|option\\s)slowkill|(?<!advised\\s|advise\\s|discuss\\s|discussed\\s|protocol\\s|history\\s|on\\s|option\\s)slow kill|(?<!advised\\s|advise\\s|discuss\\s|discussed\\s|protocol\\s|history\\s|on\\s|option\\s)doxycycline"
        ) ~ "slowkill",
        .default = "NA"
      ),

      # ---------------------------------------------------------------------
      # Classify symptom severity
      # ---------------------------------------------------------------------
      symptoms = case_when(
        # Most severe: caval syndrome (life-threatening)
        stringr::str_detect(cleaned_examin_text, paste0(caval_syndrome, collapse = "|")) ~ "caval syndrome",
        # Mild symptoms (coughing)
        stringr::str_detect(cleaned_examin_text, paste0(mild_symptoms, collapse = "|")) ~ "mild",
        # Moderate symptoms (exercise intolerance, lethargy)
        stringr::str_detect(cleaned_examin_text, paste0(moderate_symptoms, collapse = "|")) ~ "moderate",
        # Severe symptoms (cardiac/respiratory compromise)
        stringr::str_detect(cleaned_examin_text, paste0(severe_symptoms, collapse = "|")) ~ "severe",
        .default = "NA"
      ),

      # ---------------------------------------------------------------------
      # Initial heartworm status from test results
      # ---------------------------------------------------------------------
      heartworm = case_when(
        # Explicit positive test results
        stringr::str_detect(cleaned_examin_text, paste0(c("heartworm test positive", "heartworm test antigen positive", mf_positive_phrases), collapse = "|")) ~ "positive",
        # Explicit negative test results
        stringr::str_detect(cleaned_examin_text, paste0(c("heartworm test negative", "heartworm test antigen negative", mf_negative_phrases), collapse = "|")) ~ "negative",
        # Other positive indicators
        stringr::str_detect(cleaned_examin_text, paste0(hw_positive_phrases, collapse = "|")) ~ "positive",
        # Other negative indicators
        stringr::str_detect(cleaned_examin_text, paste0(hw_negative_phrases, collapse = "|")) ~ "negative",
        .default = "NA"
      ),

      # ---------------------------------------------------------------------
      # Impute heartworm status when explicit test result missing
      # ---------------------------------------------------------------------
      heartworm = case_when(
        # No evidence at all -> indeterminate
        heartworm == "NA" & symptoms == "NA" & preventative == "NA" & treatment == "NA" ~ "indeterminate",
        # Symptoms only -> suspect
        heartworm == "NA" & symptoms != "NA" & preventative == "NA" & treatment == "NA" ~ "suspect",
        # Symptoms + prevention/treatment -> probable
        heartworm == "NA" & symptoms != "NA" & preventative != "NA" & treatment != "NA" ~ "probable",
        heartworm == "NA" & symptoms != "NA" & preventative == "NA" & treatment != "NA" ~ "probable",
        heartworm == "NA" & symptoms != "NA" & preventative != "NA" & treatment == "NA" ~ "probable",
        # Treatment/prevention without symptoms -> treated
        heartworm == "NA" & symptoms == "NA" & preventative != "NA" & treatment != "NA" ~ "treated",
        heartworm == "NA" & symptoms == "NA" & preventative != "NA" & treatment == "NA" ~ "treated",
        heartworm == "NA" & symptoms == "NA" & preventative == "NA" & treatment != "NA" ~ "treated",
        # Keep original classification
        TRUE ~ heartworm
      )
    )
}

# -----------------------------------------------------------------------------
# Main Processing
# -----------------------------------------------------------------------------

# Merge tokenized data
hw_data <- merge_tokenized()

# Apply classification rules
hw_data_classified <- classify_records(hw_data)

# -----------------------------------------------------------------------------
# Add Deceased Date and Calculate Age
# -----------------------------------------------------------------------------

# Load deceased dates
deceased_dates <- readr::read_csv(
  file = DECEASED_DATES_PATH,
  show_col_types = FALSE
) %>%
  rename(PatientNumber = patientnumber) %>%
  filter(PatientDeceasedDate != "NULL") %>%
  # Keep most recent deceased date per patient
  group_by(PatientNumber) %>%
  arrange(desc(PatientDeceasedDate)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Join deceased dates and calculate age at consultation
hw_data_classified <- hw_data_classified %>%
  left_join(deceased_dates, by = "PatientNumber") %>%
  mutate(
    ConsultationDate = as.Date(ConsultationDate),
    PatientDateofBirth = as.Date(PatientDateofBirth),
    # Age in years at time of consultation
    age = floor(as.numeric(difftime(ConsultationDate, PatientDateofBirth, units = "days")) / 365.25)
  )

# -----------------------------------------------------------------------------
# Save Output
# -----------------------------------------------------------------------------

# Create output directory if needed
if (!dir.exists(dirname(HW_CLASSIFIED_PATH))) {
  dir.create(dirname(HW_CLASSIFIED_PATH), recursive = TRUE)
}

save(hw_data_classified, file = HW_CLASSIFIED_PATH)

message("Classification complete. Output saved to: ", HW_CLASSIFIED_PATH)
