#!/usr/bin/env Rscript

# ===========================================================
# Project: Physiotherapy and Parkinson's Disease
# Script: CreateTableOne.R
# Authors: Christian Isselstein, David Pedrosa
# Created: 2025-02-17
# Last modified: 2026-02-11
# Version: 1.1
#
# Description:
#   Creates a TableOne for all participants in the survey
#
#
# Inputs:
#   - df_processed (data.frame): cleaned and analysis-ready dataset
#
# Usage:
#   From another script:
#       source("CreateTableOne.v1.1.R")
#
# Change log:
#   Version 1.2 (2026-02-11): Code tidied up, English duplicates added,
#                             improved safety and reproducibility
#   Version 1.1 (2025-08-04): Added functionality and debugging
#   Version 1.0 (2025-02-17): Initial refactor and rename
# ===========================================================


# ---- Preconditions ---------------------------------------------------------

if (!exists("df_processed", inherits = TRUE)) {
  stop("df_processed not found. Source tidyup_dataframe.R (or create df_processed) before running CreateTableOne.R")
}
if (!exists("variable_groups", inherits = TRUE)) {
  stop("variable_groups not found. Ensure tidyup_dataframe.R defines variable_groups before running this script.")
}

#TODO: This variable [variable_groups] drops an error, as you renamed 101 at some point. PLease have a look at 
# so that the error is fixed; I recommend putting all the packages into [01_packages.R] and all recoding 
# and stuff into 01_tidyup_dataframe.R

out_file_demo   <- file.path(results_dir, "table1_demographics.csv")
out_file_scales <- file.path(results_dir, "table1_scales.csv")

# ---- TableOne: Demographics -----------------------------------------------

demo_vars <- c("gender", "work_experience", "work_environment", "age", "patient_volume", "colleagues")
demo_cat  <- c("gender", "work_experience", "work_environment")

# TODO: you may consider further information here and merging both tab_demo and tab_scales to one Table1?!

missing_demo <- setdiff(demo_vars, names(df_processed))
if (length(missing_demo) > 0) {
  warning("Demographics vars missing in df_processed: ", paste(missing_demo, collapse = ", "))
}

tab_demo <- CreateTableOne(
  vars       = intersect(demo_vars, names(df_processed)),
  data       = df_processed,
  factorVars = intersect(demo_cat, names(df_processed)),
  addOverall = TRUE
)

print(tab_demo)

demo_export <- print(
  tab_demo,
  quote = FALSE,
  noSpaces = TRUE,
  contDigits = 1,
  printToggle = FALSE,
  showAllLevels = FALSE
)

write.csv(demo_export, out_file_demo, row.names = TRUE)

# ---- Scale scoring ---------------------------------------------------------

# Binary handling rule: values labeled "Ja" -> 1, else -> 0 (NA stays NA)
to_01_if_ja <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    return(if_else(is.na(x), NA_real_, if_else(x == "Ja", 1, 0)))
  }
  as.numeric(x)
}

scale_mean <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(rep(NA_real_, nrow(df)))

  x <- df %>%
    mutate(across(all_of(cols), to_01_if_ja)) %>%
    select(all_of(cols))

  rowMeans(x, na.rm = TRUE)
}

# compute all scales from variable_groups in one shot
# (only those you actually want as "scores")
scale_groups <- variable_groups[c(
  "therapy_IPS",
  "medical_assumptions",
  "treatment_goals",
  "postgraduation",
  "patient_centeredness",
  "interdisciplinary_cooperation",
  "patient_volume",
  "resources",
  "attitudes",
  "intention_behavioral_control",
  "subjective_norm",
  "barriers_patient",
  "barriers_workplace",
  "barriers_personal"
)]

score_df <- imap_dfc(scale_groups, ~ tibble::tibble("{.y}_score" := scale_mean(df_processed, .x)))

df_processed <- bind_cols(df_processed, score_df)


# ---- TableOne: Scales ------------------------------------------------------

scale_vars <- names(score_df)

tab_scales <- CreateTableOne(vars = scale_vars, data = df_processed)
print(tab_scales)

scales_export <- print(
  tab_scales,
  quote = FALSE,
  noSpaces = TRUE,
  contDigits = 2,
  printToggle = FALSE
)

write.csv(scales_export, out_file_scales, row.names = TRUE)

