# ===========================================================
# Project: Physiotherapie and Parkinson's Disease
# Script: tidyup_dataframe.R
# Authors: Christian Isselstein and David Pedrosa
# Date: 2025-17-02
# Version: 1.0
# Description: Initial commit
#
# Change log:
#   Version 1.1 (2025-ß8-04): Added some functionality and code debugging
#   Version 1.0 (2025-17-02): Rename and refactor data
# ===========================================================

# Function to rename data
# ===========================================================
to_numeric <- function(df, cols) {
  df %>% mutate(across(all_of(cols), as.numeric))
}

# Function to apply factor conversion
to_factor <- function(df, cols, levels, labels) {
  # Check that all specified columns are present
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    warning("Skipping missing columns: ", paste(missing_cols, collapse = ", "))
    cols <- intersect(cols, names(df))
  }

  # If any valid columns remain, convert them to factor
  if (length(cols) > 0) {
    df <- df %>%
      mutate(across(all_of(cols), ~ factor(.x, levels = levels, labels = labels)))
  }

  return(df)
}

df_processed <- df_raw %>% select(-c(SERIAL, REF, STARTED, QUESTNNR, MODE, TIME001, TIME002, TIME003, TIME004, TIME005, TIME006, TIME007, TIME_SUM, MAILSENT, LASTDATA, FINISHED, Q_VIEWER, LASTPAGE, MAXPAGE, MISSING, TIME_RSI))

# Grouped variable lists stored in named list
variable_groups <- list(
  medical_assumptions           = c("IA05_01", "IA05_02", "IA05_03", "IA05_04", "IA05_05"),
  treatment_goals              = c("IA06_01", "IA06_02", "IA06_03", "IA06_04", "IA06_05", "IA06_06", "IA06_07", "IA06_08", "IA06_09"),
  patient_centeredness         = c("K104_01", "K104_02", "K104_03", "K104_04", "K104_05", "K104_06"),
  interdisciplinary_cooperation = c("K107_01"),
  patient_volume               = c("K108_01"),
  resources                    = c("K201_01", "K201_02", "K201_03", "K201_04", "K201_05", "K201_06"),
  attitudes                    = c("K202_01", "K202_02", "K202_03", "K202_04", "K202_05", "K202_06"),
  intention_behavioral_control = c("K203_01", "K203_02", "K203_03", "K203_04", "K203_05", "K203_06", "K203_08", "K203_09"),
  subjective_norm              = c("K204_01", "K204_02", "K204_03"),
  barriers_patient             = c("K217_01", "K217_02", "K217_03", "K217_04", "K217_05", "K217_06"),
  barriers_workplace           = c("K218_01", "K218_02", "K218_03", "K218_04", "K218_05", "K218_06", "K218_07", "K218_08", "K218_09"),
  barriers_personal            = c("K219_01", "K219_03", "K219_04", "K219_05", "K219_06")
)

df_processed <- to_numeric(df_processed, unlist(variable_groups))

# Define factor labels in a named list (this is now the single source of truth)
factor_labels <- list(
  referral_information = c("-", "Auswahl"),
  binary              = c("Ja", "Nein"),
  gender              = c("weiblich", "männlich", "divers"),
  edu                 = c("Auszubildende/Auszubildender", "Studentin/Student", "Abschluss Ausbildung/Staatsexamen", 
                          "Studienabschluss Bachelor", "Studienabschluss Master/Diplom", "Promotion"),
  work_env            = c("freie Praxis", "Krankenhaus", "Rehabilitationseinrichtung", "Universitätsklinikum"),
  employment          = c("Auszubildende/Auszubildender_Studentin/Student", "Arbeitnehmerin/Arbeitnehmer", 
                          "selbstständig", "freiberuflich"),
  work_intensity      = c("≤ 19 Stunden/Woche", "20-29 Stunden/Woche", "30-39 Stunden/Woche", "≥ 40 Stunden/Woche"),
  work_experience     = c("≤ 4 Jahre", "5-9 Jahre", "10-14 Jahre", "15-19 Jahre", "20-24 Jahre", 
                          "25-29 Jahre", "30-34 Jahre", "≥35 Jahre")
)

# This list maps column groups to their corresponding label sets
factor_columns <- list(
  referral_information = c("IA01_01", "IA01_02", "IA01_03", "IA01_04", "IA01_05", "IA01_07", "IA01_08", "IA01_09"),
  binary = c("K101", "K105", "SD05", "SD06", "SD07", "SD14"),
  gender = "SD01",
  edu = "SD04",
  work_env = "SD08",
  employment = "SD09",
  work_intensity = "SD10",
  work_experience = "SD11"
)

# Apply factor conversion for all defined groups
for (group in names(factor_columns)) {
  cols   <- factor_columns[[group]]
  labels <- factor_labels[[group]]
  df_processed <- to_factor(df_processed, cols, levels = seq_along(labels), labels = labels)
}
