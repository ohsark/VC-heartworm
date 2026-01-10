# =============================================================================
# Title:        Classification Analysis and Visualization
# Author:       Sahil Arora
# Description:  Descriptive analyses on the classified heartworm dataset
#               including survival windows, time-to-negative conversion,
#               and yearly trend visualizations.
#
# Inputs:       - hw_data_classified_FINAL.rda (from 02_HW_test_classification.R)
#
# Outputs:      - In-memory plots and tibbles for analysis
#               - Survival analysis by time period
#               - Time series of positive/negative cases and tests
#
# Dependencies: tidyverse, DT, gridExtra, scales
#
# Time Periods: Pre-2008, 2008-2015, Post-2015
# =============================================================================

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(DT)
  library(gridExtra)
  library(scales)
})

# -----------------------------------------------------------------------------
# Source Configuration
# -----------------------------------------------------------------------------
source("./config/paths.R")

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------
load(HW_CLASSIFIED_PATH)

# -----------------------------------------------------------------------------
# Identify Patient Cohorts
# -----------------------------------------------------------------------------

# Unique patients with positive heartworm results
positive_ids <- unique(hw_data_classified$PatientNumber[hw_data_classified$heartworm == "positive"])

# Unique patients who received immiticide treatment
treated_ids <- unique(hw_data_classified$PatientNumber[hw_data_classified$treatment == "immiticide"])

# Cohort comparisons
only_treat_testing <- setdiff(treated_ids, positive_ids)   # Treated but never tested positive
positive_no_treat <- setdiff(positive_ids, treated_ids)     # Positive but no immiticide
positive_treated <- intersect(positive_ids, treated_ids)    # Positive and received treatment

# -----------------------------------------------------------------------------
# Interactive Table Functions (for manual review)
# -----------------------------------------------------------------------------

#' Display examination text in interactive datatable
#'
#' @param df Dataframe to display
#' @return DT datatable widget
openTable <- function(df) {
  datatable(
    df,
    options = list(scrollX = TRUE, pageLength = 1000, autoWidth = TRUE),
    rownames = FALSE
  ) %>%
    formatStyle(columns = c("ExaminationText", "cleaned_examin_text"), fontSize = "18px")
}

#' Save dataframe to CSV
#'
#' @param df Dataframe to save
#' @param fileName Base filename (without extension)
saveTable <- function(df, fileName) {
  readr::write_csv(df, file = file.path(DATA_OUTPUT_DIR, paste0(fileName, ".csv")))
}

# -----------------------------------------------------------------------------
# Survival Analysis Functions
# -----------------------------------------------------------------------------

#' Calculate survival time brackets after positive diagnosis
#'
#' For patients with positive heartworm diagnosis, calculates time from
#' first positive to death (if deceased date available).
#'
#' @param data Classified heartworm dataset
#' @return Tibble with survival time bracket counts and percentages
survival_time_brackets <- function(data) {
  data %>%
    filter(heartworm == "positive") %>%
    # Get first positive consultation per patient
    group_by(PatientNumber) %>%
    arrange(ConsultationDate) %>%
    slice_head(n = 1) %>%
    # Calculate time until death
    mutate(timeTillDeath = as.numeric(difftime(as.Date(PatientDeceasedDate), as.Date(ConsultationDate), units = "days"))) %>%
    ungroup() %>%
    # Only include valid positive survival times
    filter(timeTillDeath >= 0) %>%
    # Categorize into time brackets
    mutate(month_brackets = cut(
      timeTillDeath,
      breaks = c(0, 30, 60, 90, 185, 365, 730, Inf),
      labels = c("1 month", "2 months", "3 months", "6 months", "1 year", "2 years", "Over 2 years"),
      include.lowest = TRUE
    )) %>%
    count(month_brackets) %>%
    mutate(Percentage = (n / sum(n)) * 100)
}

#' Calculate time from positive to first negative result
#'
#' For patients who converted from positive to negative status,
#' calculates the time interval between first positive and first negative.
#'
#' @param data Classified heartworm dataset
#' @return Tibble with time-to-negative bracket counts
time_to_negative <- function(data) {
  # Get first positive consultation per patient
  first_positive <- data %>%
    filter(heartworm == "positive") %>%
    group_by(PatientNumber) %>%
    arrange(ConsultationDate) %>%
    slice_head(n = 1) %>%
    ungroup()

  # Find first negative result after the positive
  negative_after <- data %>%
    filter(PatientNumber %in% first_positive$PatientNumber) %>%
    arrange(ConsultationDate) %>%
    group_by(PatientNumber) %>%
    filter(ConsultationDate > first_positive$ConsultationDate[match(PatientNumber, first_positive$PatientNumber)] & heartworm == "negative") %>%
    slice_head(n = 1) %>%
    ungroup()

  # Calculate time to conversion
  first_positive %>%
    inner_join(negative_after, by = "PatientNumber", suffix = c("_positive", "_negative")) %>%
    mutate(timeToNegative = as.numeric(difftime(as.Date(ConsultationDate_negative), as.Date(ConsultationDate_positive), units = "days"))) %>%
    mutate(month_brackets = cut(
      timeToNegative,
      breaks = c(0, 30, 60, 90, 185, 365, 730, Inf),
      labels = c("1 month", "2 months", "3 months", "6 months", "1 year", "2 years", "Over 2 years"),
      include.lowest = TRUE
    )) %>%
    count(month_brackets)
}

# -----------------------------------------------------------------------------
# Visualization Functions
# -----------------------------------------------------------------------------

#' Plot survival time by historical period
#'
#' @param data Survival data with Period column
#' @return ggplot object
plot_survival_by_period <- function(data) {
  data %>%
    group_by(Period) %>%
    mutate(Percentage = (n / sum(n)) * 100) %>%
    ggplot(aes(x = Percentage, y = month_brackets, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Survival time after diagnosis",
      x = "Percentage of patients",
      y = "Time brackets",
      fill = "Period"
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    scale_x_continuous(labels = percent_format(scale = 1))
}

#' Plot time to negative conversion
#'
#' @param data Time-to-negative data with month_brackets
#' @return ggplot object
plot_time_to_negative <- function(data) {
  ggplot(data, aes(x = n, y = month_brackets)) +
    geom_col(fill = "#4E79A7", color = "black") +
    labs(
      title = "Time till negative after diagnosis",
      subtitle = "Patients with first negative after first positive",
      x = "Number of patients",
      y = "Time brackets"
    ) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
}

# -----------------------------------------------------------------------------
# Period-Based Analysis
# -----------------------------------------------------------------------------

# Split data by historical periods
period_split <- list(
  before_2008 = hw_data_classified %>% filter(Year < 2008),
  between_2008_2015 = hw_data_classified %>% filter(Year >= 2008 & Year <= 2015),
  after_2015 = hw_data_classified %>% filter(Year > 2015)
)

# Calculate survival by period
survival_by_period <- bind_rows(
  survival_time_brackets(period_split$before_2008) %>% mutate(Period = "2000-2007"),
  survival_time_brackets(period_split$between_2008_2015) %>% mutate(Period = "2008-2015"),
  survival_time_brackets(period_split$after_2015) %>% mutate(Period = "2016-2023")
)

# Calculate time-to-negative by period
time_to_negative_by_period <- bind_rows(
  time_to_negative(period_split$before_2008) %>% mutate(Period = "2000-2007"),
  time_to_negative(period_split$between_2008_2015) %>% mutate(Period = "2008-2015"),
  time_to_negative(period_split$after_2015) %>% mutate(Period = "2016-2023")
)

# -----------------------------------------------------------------------------
# Generate Visualizations
# -----------------------------------------------------------------------------

# Survival by period plot
plot_survival_by_period(survival_by_period)

# Time to negative plot (all data)
plot_time_to_negative(time_to_negative(hw_data_classified))

# -----------------------------------------------------------------------------
# Time Series Trend Analysis
# -----------------------------------------------------------------------------

# Negative cases by year
neg_ts <- hw_data_classified %>%
  filter(Year < 2023, heartworm == "negative") %>%
  distinct(PatientNumber, Year, .keep_all = TRUE) %>%
  count(Year) %>%
  mutate(date = as.Date(paste(Year, "01", "01"), format = "%Y %m %d"))

# Positive cases by year
pos_ts <- hw_data_classified %>%
  filter(Year < 2023, heartworm == "positive") %>%
  distinct(PatientNumber, Year, .keep_all = TRUE) %>%
  count(Year) %>%
  mutate(date = as.Date(paste(Year, "01", "01"), format = "%Y %m %d"))

# Tests conducted by year
tests_ts <- hw_data_classified %>%
  filter(Year < 2023, hw_test == "conducted") %>%
  distinct(PatientNumber, Year, .keep_all = TRUE) %>%
  count(Year) %>%
  mutate(date = as.Date(paste(Year, "01", "01"), format = "%Y %m %d"))

# -----------------------------------------------------------------------------
# Time Series Plots
# -----------------------------------------------------------------------------

# Positive cases trend
pos_plot <- ggplot(pos_ts, aes(x = date, y = n)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkblue", linetype = "dashed") +
  labs(title = "Yearly heartworm positive cases", x = "Year", y = "Diagnoses") +
  theme_minimal()

# Negative cases trend
neg_plot <- ggplot(neg_ts, aes(x = date, y = n)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linetype = "dashed") +
  labs(title = "Yearly heartworm negative cases", x = "Year", y = "Diagnoses") +
  theme_minimal()

# Tests conducted trend
tests_plot <- ggplot(tests_ts, aes(x = date, y = n)) +
  geom_line(color = "green", linewidth = 1) +
  geom_point(color = "green", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen", linetype = "dashed") +
  labs(title = "Yearly heartworm tests conducted", x = "Year", y = "Tests") +
  theme_minimal()

# Combined display
gridExtra::grid.arrange(pos_plot, neg_plot, tests_plot, ncol = 1)
