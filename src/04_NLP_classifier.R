# =============================================================================
# Title:        NLP text classifier for heartworm diagnosis
# Author:       Sahil Arora
# Description:  Trains a Gradient Boosting Machine (GBM) text classifier on
#               cleaned examination text to predict positive vs negative
#               heartworm status. Uses tokenization, stemming, and
#               document-term matrix construction.
#
# Inputs:       - hw_data_classified (positive/negative cases only)
#
# Outputs:      - Trained GBM model
#               - Confusion matrix and AUC score
#               - ROC curve visualization
#               - Predictions tibble for held-out test data
#
# Dependencies: gbm, e1071, caret, tidyverse, SnowballC, pROC
#
# Model Parameters:
#               - n_trees: 200
#               - shrinkage: 0.01
#               - interaction.depth: 3
#               - cv.folds: 5
#               - Probability cutoff: 0.59
# =============================================================================

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(gbm)
  library(e1071)
  library(caret)
  library(tidyverse)
  library(SnowballC)
  library(pROC)
})

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

set.seed(123)  # For reproducibility

# Minimum word frequency threshold for inclusion in model
MIN_WORD_FREQ <- 70

# -----------------------------------------------------------------------------
# Data Preparation
# -----------------------------------------------------------------------------
# NOTE: hw.data.classified must be loaded in memory before running

# Select relevant columns and filter to confirmed cases only
my_data <- hw.data.classified %>%
  select(id, cleaned_examin_text, heartworm) %>%
  filter(heartworm %in% c("positive", "negative"))

# Create train/test split (80/20)
train_index <- createDataPartition(y = my_data$heartworm, p = 0.8, list = FALSE)
train_data <- my_data[train_index, ]
test_data <- my_data[-train_index, ]

# -----------------------------------------------------------------------------
# Text Processing Functions
# -----------------------------------------------------------------------------

#' Tokenize and stem examination text
#'
#' Splits text into words, removes NA/empty/numeric tokens,
#' and applies Porter stemming algorithm.
#'
#' @param data Dataframe with id and cleaned_examin_text columns
#' @return Dataframe with id and stemmed word columns
stem_tokenize <- function(data) {
  data %>%
    select(id, cleaned_examin_text) %>%
    # Split text into individual words
    mutate(word = strsplit(as.character(cleaned_examin_text), " ")) %>%
    tidyr::unnest(word) %>%
    # Filter out invalid tokens
    filter(!is.na(word), word != "na", word != "", !grepl("[[:digit:]]", word)) %>%
    # Apply Porter stemming
    mutate(replaced_word = SnowballC::wordStem(word)) %>%
    select(id, replaced_word)
}

# Apply tokenization
train_tokens <- stem_tokenize(train_data)
test_tokens <- stem_tokenize(test_data)

# -----------------------------------------------------------------------------
# Feature Selection
# -----------------------------------------------------------------------------

# Select words that appear frequently in positive cases
# This focuses the model on discriminative vocabulary
final_word_count <- train_tokens %>%
  filter(id %in% train_data$id[train_data$heartworm == "positive"]) %>%
  count(replaced_word) %>%
  filter(n >= MIN_WORD_FREQ, nchar(replaced_word) > 2)

# -----------------------------------------------------------------------------
# Document-Term Matrix Construction
# -----------------------------------------------------------------------------

#' Create wide-format document-term matrix
#'
#' Converts tokenized data to a matrix where each row is a document
#' and each column is a word frequency count.
#'
#' @param tokens Tokenized dataframe with animal_id and word columns
#' @return Wide dataframe with word frequency columns
widen_data <- function(tokens) {
  tokens %>%
    rename(animal_id = id, word = replaced_word) %>%
    # Keep only selected features
    filter(word %in% final_word_count$replaced_word) %>%
    # Count word occurrences per document
    group_by(animal_id, word) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    distinct() %>%
    # Pivot to wide format
    pivot_wider(
      id_cols = animal_id,
      names_from = word,
      values_from = total,
      values_fill = 0
    )
}

# Build document-term matrices
train_matrix <- widen_data(train_tokens)
test_matrix <- widen_data(test_tokens)

# -----------------------------------------------------------------------------
# Prepare Model Data
# -----------------------------------------------------------------------------

# Join with outcome labels and convert to binary
model_data <- train_matrix %>%
  left_join(train_data %>% rename(probscore = heartworm), by = join_by(animal_id == id)) %>%
  mutate(probscore = if_else(probscore == "positive", 1, 0)) %>%
  select(-cleaned_examin_text)

model_test_data <- test_matrix %>%
  left_join(test_data %>% rename(probscore = heartworm), by = join_by(animal_id == id)) %>%
  mutate(probscore = if_else(probscore == "positive", 1, 0)) %>%
  select(-cleaned_examin_text)

# -----------------------------------------------------------------------------
# Model Training Function
# -----------------------------------------------------------------------------

#' Train GBM classifier with evaluation metrics
#'
#' Trains a gradient boosting model for binary classification,
#' generates predictions, and computes performance metrics.
#'
#' @param model_data Training data with probscore outcome
#' @param test_data Test data for evaluation
#' @param prob_cutoff Probability threshold for classification (default: 0.5)
#' @param n_trees Number of trees (default: 200)
#' @param min_obs Minimum observations per node (default: 5)
#' @param shrinkage Learning rate (default: 0.01)
#' @param int_depth Interaction depth (default: 3)
#' @param bag_fraction Bagging fraction (default: 1)
#' @return List with model, predictions, confusion matrix, and AUC
train_gbm <- function(model_data,
                      test_data,
                      prob_cutoff = 0.5,
                      n_trees = 200,
                      min_obs = 5,
                      shrinkage = 0.01,
                      int_depth = 3,
                      bag_fraction = 1) {

  # Separate outcomes from features
  training_outcomes <- model_data$probscore
  testing_outcomes  <- test_data$probscore

  model_data$probscore <- NULL
  test_data$probscore  <- NULL

  # Remove ID column for modeling
  training_features <- model_data %>% select(-animal_id)
  testing_features  <- test_data %>% select(-animal_id)

  # Train GBM model
  gbm_mod <- gbm(
    training_outcomes ~ .,
    data = cbind(training_outcomes, training_features),
    distribution = "bernoulli",
    n.trees = n_trees,
    interaction.depth = int_depth,
    shrinkage = shrinkage,
    n.minobsinnode = min_obs,
    bag.fraction = bag_fraction,
    cv.folds = 5,
    train.fraction = 0.75,
    verbose = TRUE
  )

  # Generate predictions
  preds <- predict(
    gbm_mod,
    newdata = testing_features,
    n.trees = n_trees,
    type = "response"
  )

  # Apply probability cutoff
  class_preds <- if_else(preds >= prob_cutoff, 1, 0)

  # Compute confusion matrix
  output_table <- confusionMatrix(
    factor(class_preds, levels = c(0, 1)),
    factor(testing_outcomes, levels = c(0, 1)),
    positive = "1",
    mode = "everything"
  )

  # Compute ROC/AUC
  roc_obj <- pROC::roc(testing_outcomes, preds)

  list(
    model = gbm_mod,
    preds = preds,
    confusion = output_table,
    auc = auc(roc_obj)
  )
}

# -----------------------------------------------------------------------------
# Train and Evaluate Model
# -----------------------------------------------------------------------------

model_info <- train_gbm(model_data, model_test_data)

# Display results
message("AUC: ", model_info$auc)
print(model_info$confusion)

# -----------------------------------------------------------------------------
# ROC Curve Visualization
# -----------------------------------------------------------------------------

roc_obj <- pROC::roc(
  model_test_data$probscore,
  model_info$preds,
  levels = c(0, 1),
  direction = "<",
  ci = TRUE
)

pROC::plot.roc(roc_obj)

# -----------------------------------------------------------------------------
# Cutoff Sensitivity Analysis
# -----------------------------------------------------------------------------
# Evaluate performance across different probability cutoffs

cutoffs <- seq(0.2, 0.95, by = 0.05)

cutoff_analysis <- map_df(cutoffs, function(cut) {
  preds <- if_else(model_info$preds >= cut, 1, 0)
  cm <- confusionMatrix(
    factor(preds, levels = c(0, 1)),
    factor(model_test_data$probscore, levels = c(0, 1)),
    positive = "1"
  )

  tibble(
    cutoff = cut,
    specificity = cm$byClass["Specificity"],
    precision = cm$byClass["Pos Pred Value"],
    sensitivity = cm$byClass["Sensitivity"]
  )
})

print(cutoff_analysis)

# -----------------------------------------------------------------------------
# Generate Final Predictions
# -----------------------------------------------------------------------------
# Apply optimized cutoff (0.59) to test data

prediction_matrix <- test_matrix
prediction_matrix$probscore <- NA
preds <- predict(model_info$model, newdata = prediction_matrix, type = "response")
class_preds <- if_else(preds >= 0.59, "positive", "negative")

predictions <- tibble(id = prediction_matrix$animal_id, heartworm = class_preds)

# -----------------------------------------------------------------------------
# Merge Predictions Back to Master Dataset
# -----------------------------------------------------------------------------
# NOTE: master_hw_data must be loaded in memory

hw_data_classified <- predictions %>%
  left_join(master_hw_data, by = "id") %>%
  filter(Year != 2023)

message("NLP classification complete. ", nrow(predictions), " records classified.")
