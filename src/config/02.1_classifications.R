# =============================================================================
# Title:        Classification Patterns Dictionary
# Author:       Sahil Arora
# Description:  Central dictionary of regex patterns used for classifying
#               heartworm test results, microfilaria status, treatments,
#               preventatives, and symptom severity levels.
#
# Inputs:       None (constants only)
#
# Outputs:      Pattern vectors used by classification scripts:
#               - hw_positive_phrases, hw_negative_phrases
#               - mf_positive_phrases, mf_negative_phrases
#               - hw_test patterns
#               - no_treatments, no_preventions
#               - mild_symptoms, moderate_symptoms, severe_symptoms
#               - caval_syndrome
#
# Usage:        source("src/config/02.1_classifications.R")
#
# Notes:        - Uses negative lookbehind assertions to avoid false positives
#               - Extend vectors here when refining classification rules
# =============================================================================

# -----------------------------------------------------------------------------
# Confirmed Heartworm Test Results
# -----------------------------------------------------------------------------
# Explicit test result phrases with high confidence

confirmed_hw_positive_phrases <- c(
  "heartworm test positive",
  "heartworm test antigen positive",
  "heartworm test high antigen",
  "heartworm blood test positive",
  "heartworm rapid test positive",
  "heartworm antigen elisa test positive",
  "heartworm elisa test positive",
  "heartworm antigen elisa positive",
  "heartworm test smear positive",
  "heartworm antigen test positive"
)

confirmed_hw_negative_phrases <- character()

# -----------------------------------------------------------------------------
# Heartworm Positive Terms
# -----------------------------------------------------------------------------
# Additional positive indicators beyond explicit test results

hw_positive_terms <- c(
  "heartworm positive",
  "positive heartworm",
  "positive for heartworm",
  "positive heartworm antigen",
  "positive heartworm antigen test",
  "positive dirofilaria",
  "positive dirofilaria test",
  "smear positive heartworm",
  "heartworm smear positive",
  "positive heartworm smear",
  "heartworm status blood test result antigen positive",
  "heartworm test level positive",
  "heartworm test high positive",
  "heartworm exam positive low antigen",
  "thick smear positive heartworm",
  "heartworm test faint positive",
  "heartworm test wa positive"
)

# -----------------------------------------------------------------------------
# Heartworm Positive Trigger Phrases
# -----------------------------------------------------------------------------
# Combined triggers including treatment-associated positives

hw_positive_triggers <- c(
  confirmed_hw_positive_phrases,
  hw_positive_terms,
  "heartworm antigen positive",
  "dirofilaria positive",
  "antigen test positive.*heartworm|heartworm.*antigen test positive",
  "positive heartworm test antigen",
  "positive heartworm blood test",
  "positive heartworm rapid test",
  "positive heartworm antigen elisa",
  "positive elisa test heartworm",
  "positive eliza test heartworm",
  "positive heartworm test smear",
  # Treatment context suggests prior positive diagnosis
  "heartworm test.*(immiticide|slowkill|slow kill)",
  "dirofilaria test.*(immiticide|slowkill|slow kill)",
  "heartworm antigen test.*(immiticide|slowkill|slow kill)"
)

# -----------------------------------------------------------------------------
# Heartworm Positive Phrases (with lookbehind exclusions)
# -----------------------------------------------------------------------------
# Excludes false positives from hypothetical/historical contexts

hw_positive_phrases <- paste0(
  "(?<!if\\s|potentially\\s|check\\s|discuss\\s|discussed\\s|not\\s|risk\\s|previous\\s|history\\s|to\\s|in\\s|if dog\\s|other dog\\s|neighbour dog\\s|prophylaxis to\\s)",
  hw_positive_triggers
)

# -----------------------------------------------------------------------------
# Heartworm Negative Phrases
# -----------------------------------------------------------------------------

hw_negative_phrases <- c(
  "heartworm test negative",
  "antigen test negative*heartworm|heartworm.*antigen test negative",
  "smear negative heartworm",
  "heartworm blood test negative",
  "heartworm rapid test negative",
  "heartworm negative",
  "heartworm antigen elisa test negative",
  "heartworm elisa test negative",
  "heartworm antigen elisa negative",
  "heartworm test smear negative",
  "heartworm smear negative",
  "not tested positive heartworm",
  "heartworm antigen negative",
  "heartworm antigen test negative",
  "dirofilaria negative",
  "dirofilaria test negative",
  "negative heartworm",
  "negative for heartworm",
  "smear negative for heartworm",
  "negative smear for microfilaria",
  "thick smear negative heartworm",
  "negative for microfilaria",
  "heartworm smear smear negative",
  "heartworm smear negative"
)

# -----------------------------------------------------------------------------
# Microfilaria Test Results
# -----------------------------------------------------------------------------
# Microfilaria (larval stage) detection patterns

mf_positive_phrases <- c(
  "positive microfilaria test",
  "positive microfilaria smear",
  "smear positive microfilaria",
  "positive microfilaria blood test",
  "(?<!no\\s)microfilaria seen",
  "(?<!no\\s)microfilaria on blood smear",
  "(?<!no\\s)microfilaria in blood smear",
  "(?<!no\\s)microfilaria on wet smear",
  "(?<!no\\s)microfilaria detected",
  "positive microfilaria",
  "positive knot",
  "positive knot test",
  "positive microfilaria test smear",
  "positive microfilaria antigen",
  "wet blood smear microfilaria seen",
  "numerous microfilaria",
  "positive wet smear microfilaria",
  "wet smear lot microfilaria",
  "positive for microfilaria"
)

mf_negative_phrases <- c(
  "microfilaria test negative",
  "microfilaria smear negative",
  "microfilaria blood test negative",
  "no microfilaria",
  "no microfilaria seen",
  "no microfilaria on blood smear",
  "no microfilaria in blood smear",
  "no microfilaria on wet smear",
  "no microfilaria detected",
  "microfilaria negative",
  "knot negative",
  "knot test negative",
  "smear negative microfilaria",
  "microfilaria test smear negative",
  "microfilaria antigen negative",
  "wet blood smear microfilaria negative",
  "wet smear negative",
  "negative for microfilaria"
)

# -----------------------------------------------------------------------------
# Generic Heartworm Test Detection
# -----------------------------------------------------------------------------
# Patterns to detect that any heartworm test was conducted

hw_test <- c(
  "heartworm.*test",
  "microfilaria",
  "heartworm.*positive",
  "heartworm.*negative",
  "heartworm.*antigen",
  "test.*heartworm",
  "positive.*heartworm",
  "negative.*heartworm",
  "antigen.*heartworm"
)

# -----------------------------------------------------------------------------
# Treatment Refusal/Absence Patterns
# -----------------------------------------------------------------------------

no_treatments <- c(
  "not to treat for heartworm",
  "owner declines heartworm treatment",
  "decided not to treat heartworm"
)

# -----------------------------------------------------------------------------
# Patient Outcome Patterns
# -----------------------------------------------------------------------------

expired <- "dog expired"

euthanasia <- c(
  "requested euthanasia",
  "elected euthanasia",
  "elected to euthanasia",
  "euthanasia agreed",
  "treatment euthanasia",
  "reason euthanasia",
  "elect euthanasia"
)

# -----------------------------------------------------------------------------
# Prevention Absence/Advisory Patterns
# -----------------------------------------------------------------------------
# Combinations indicating lack of or advice about prevention

no_preventions <- expand.grid(
  c(
    "not on",
    "not been on",
    "no",
    "advise on",
    "advised on",
    "advise start",
    "advised start",
    "unsure"
  ),
  c(
    "heartworm prevention",
    "prevention",
    "preventatives",
    "heartworm_preventative",
    "preventions",
    "heartworm preventions"
  )
) %>%
  mutate(string = paste(Var1, Var2)) %>%
  pull(string)

# -----------------------------------------------------------------------------
# Symptom Severity Classifications
# -----------------------------------------------------------------------------

# Mild symptoms - primarily respiratory
mild_symptoms <- c(
  "reason coughing",
  "(?<!no\\s|not\\s)elicit cough",
  "(?<!no\\s|not\\s)productive cough",
  "check cough",
  "(?<!no\\s|not\\s)dry cough",
  "(?<!no\\s|not\\s)harsh cough",
  "reason cough",
  "(?<!no\\s|not\\s)started coughing",
  "(?<!no\\s|not\\s)honking cough",
  "exam cough",
  "(?<!no\\s|not\\s)mild cough",
  "(?<!no\\s|not\\s)chronic cough",
  "(?<!no\\s|not\\s)report coughing",
  "(?<!no\\s|not\\s)report cough"
)

# Moderate symptoms - exercise intolerance and lethargy
moderate_symptoms <- c(
  "noticed exercise intolerance",
  "report exercise intolerance",
  "panting exercise intolerance",
  "recent exercise intolerance",
  "significant exercise intolerance",
  "reason lethargic",
  "behavior concern",
  "bit lethargic",
  "slightly lethargic",
  "concern lethargic",
  "exam lethargic"
)

# Severe symptoms - cardiac and respiratory compromise
severe_symptoms <- c(
  "syncope",
  "(?<!no\\s|not\\s)dyspepsia|dyspeptic|dysphagia",
  "(?<!no\\s|not\\s|normal\\s)as cite",
  "(?<!no\\s)trouble breathing",
  "(?<!no\\s)issue breathing",
  "(?<!no\\s)heavy breathing",
  "harsh breath",
  "breathing issue",
  "gasping breath",
  "enlarged liver",
  "hepatocyte",
  "suspect congestive heart failure",
  "heart murmur audible",
  "(?<!no\\s|not\\s)audible heart murmur",
  "muffled heart sound"
)

# -----------------------------------------------------------------------------
# Critical/Life-Threatening Condition
# -----------------------------------------------------------------------------
# Caval syndrome indicates severe vena cava obstruction

caval_syndrome <- c("cabal syndrome", "caval syndrome")
