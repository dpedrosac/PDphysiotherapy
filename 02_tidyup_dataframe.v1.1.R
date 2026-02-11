#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Physiotherapy in Parkinson's Disease (PD)
#
# Description:
#   Cleans and transforms the raw survey dataset into an analysis-ready
#   dataframe. Applies type conversions, factor labelling, and derives
#   key variables (e.g., age, colleagues). Optionally adds English-labelled
#   duplicate columns for publication outputs.
#
# Authors:
#   Christian Isselstein
#   David Pedrosa
#
# Last updated:
#   2026-02-11
#
# R version:
#   >= 4.3.1
#
# Notes:
#   - This script is typically sourced by preamble.R
#   - Code optimized using LLM assistance
# -------------------------------------------------------------------------

#---- Helper functions  ---------------------------------------------------

to_numeric <- function(df, cols) {
  cols <- intersect(cols, names(df))
  df %>%
    mutate(across(all_of(cols), ~ as.numeric(as.character(.x))))
}

to_factor_safe <- function(df, cols, levels, labels) {
  cols_present <- intersect(cols, names(df))
  cols_missing <- setdiff(cols, names(df))

  if (length(cols_missing) > 0) {
    warning("Skipping missing columns: ", paste(cols_missing, collapse = ", "))
  }

  if (length(cols_present) == 0) return(df)

  df %>%
    mutate(across(all_of(cols_present), ~ factor(.x, levels = levels, labels = labels)))
}

#---- configuration (source of truth) --------------------------------------

drop_cols <- c(
  "SERIAL", "REF", "STARTED", "QUESTNNR", "MODE",
  paste0("TIME00", 1:7), "TIME_SUM", "MAILSENT", "LASTDATA", "FINISHED",
  "Q_VIEWER", "LASTPAGE", "MAXPAGE", "MISSING", "TIME_RSI"
)

variable_groups <- list(
  therapy_IPS                  = c("BB02"),
  medical_assumptions          = c("IA05_01", "IA05_02", "IA05_03", "IA05_04", "IA05_05"),
  treatment_goals              = c("IA06_01", "IA06_02", "IA06_03", "IA06_04", "IA06_05", "IA06_06", "IA06_07", "IA06_08", "IA06_09"),
  postgraduation               = c("K101"),
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

variable_groups_engl <- list(
  ips_therapy                    = c("BB02"),
  medical_assumptions            = c("IA05_01", "IA05_02", "IA05_03", "IA05_04", "IA05_05"),
  treatment_goals                = c("IA06_01", "IA06_02", "IA06_03", "IA06_04", "IA06_05", "IA06_06", "IA06_07", "IA06_08", "IA06_09"),
  postgraduate_training          = c("K101"),
  patient_centeredness           = c("K104_01", "K104_02", "K104_03", "K104_04", "K104_05", "K104_06"),
  interdisciplinary_collaboration = c("K107_01"),
  patient_volume                 = c("K108_01"),
  resources                      = c("K201_01", "K201_02", "K201_03", "K201_04", "K201_05", "K201_06"),
  attitudes                      = c("K202_01", "K202_02", "K202_03", "K202_04", "K202_05", "K202_06"),
  intention_behavioral_control   = c("K203_01", "K203_02", "K203_03", "K203_04", "K203_05", "K203_06", "K203_08", "K203_09"),
  subjective_norm                = c("K204_01", "K204_02", "K204_03"),
  barriers_patient               = c("K217_01", "K217_02", "K217_03", "K217_04", "K217_05", "K217_06"),
  barriers_workplace             = c("K218_01", "K218_02", "K218_03", "K218_04", "K218_05", "K218_06", "K218_07", "K218_08", "K218_09"),
  barriers_personal              = c("K219_01", "K219_03", "K219_04", "K219_05", "K219_06")
)

factor_labels <- list(
  referral_information = c("-", "Auswahl"), #TODO: What does Auswahl/Selection mean?
  binary              = c("Ja", "Nein"),
  gender              = c("weiblich", "männlich", "divers"),
  edu                 = c(
    "Auszubildende/Auszubildender", "Studentin/Student", "Abschluss Ausbildung/Staatsexamen",
    "Studienabschluss Bachelor", "Studienabschluss Master/Diplom", "Promotion"
  ),
  work_env            = c("freie Praxis", "Krankenhaus", "Rehabilitationseinrichtung", "Universitätsklinikum"),
  employment          = c("Auszubildende/Auszubildender_Studentin/Student", "Arbeitnehmerin/Arbeitnehmer", "selbstständig", "freiberuflich"),
  work_intensity      = c("≤ 19 Stunden/Woche", "20-29 Stunden/Woche", "30-39 Stunden/Woche", "≥ 40 Stunden/Woche"),
  work_experience     = c("≤ 4 Jahre", "5-9 Jahre", "10-14 Jahre", "15-19 Jahre", "20-24 Jahre", "25-29 Jahre", "30-34 Jahre", "≥35 Jahre")
)

factor_labels_engl <- list(
  referral_information = c("-", "Selection"),
  binary              = c("Yes", "No"),
  gender              = c("female", "male", "diverse"),
  edu                 = c(
    "trainee/apprentice",
    "university student",
    "completed vocational training / state examination",
    "Bachelor's degree",
    "Master's degree / diploma",
    "PhD"
  ),
  work_env            = c("private practice", "hospital", "rehabilitation facility", "university hospital"),
  employment          = c("trainee/apprentice or student", "employee", "self-employed", "freelancer"),
  work_intensity      = c("≤ 19 hours/week", "20–29 hours/week", "30–39 hours/week", "≥ 40 hours/week"),
  work_experience     = c("≤ 4 years", "5–9 years", "10–14 years", "15–19 years", "20–24 years", "25–29 years", "30–34 years", "≥ 35 years")
)

factor_columns <- list(
  referral_information = c("IA01_01", "IA01_02", "IA01_03", "IA01_04", "IA01_05", "IA01_07", "IA01_08", "IA01_09"),
  binary              = c("BB02", "K101", "K105", "SD05", "SD06", "SD07", "SD14"),
  gender              = "SD01",
  edu                 = "SD04",
  work_env            = "SD08",
  employment          = "SD09",
  work_intensity      = "SD10",
  work_experience     = "SD11"
)

edu_map <- c(
  "1" = "Auszubildende/Auszubildender",
  "2" = "Studentin/Student",
  "3" = "Abschluss Ausbildung/Staatsexamen",
  "5" = "Studienabschluss Bachelor",	
  "6" = "Studienabschluss Master/Diplom",
  "7" = "Promotion"
)

edu_map_engl <- c(
  "1" = "trainee/apprentice",
  "2" = "university student",
  "3" = "completed vocational training / state examination",
  "5" = "Bachelor's degree",
  "6" = "Master's degree / diploma",
  "7" = "PhD"
)

year_map <- c(
  "72" = 2008, "71" = 2007, "06" = 2006, "07" = 2005, "08" = 2004, "09" = 2003,
  "10" = 2002, "11" = 2001, "12" = 2000, "13" = 1999, "14" = 1998, "15" = 1997,
  "16" = 1996, "17" = 1995, "18" = 1994, "19" = 1993, "20" = 1992, "21" = 1991,
  "22" = 1990, "23" = 1989, "24" = 1988, "25" = 1987, "26" = 1986, "27" = 1985,
  "28" = 1984, "29" = 1983, "30" = 1982, "31" = 1981, "32" = 1980, "33" = 1979,
  "34" = 1978, "35" = 1977, "36" = 1976, "37" = 1975, "38" = 1974, "39" = 1973,
  "40" = 1972, "41" = 1971, "42" = 1970, "43" = 1969, "44" = 1968, "45" = 1967,
  "46" = 1966, "47" = 1965, "48" = 1964, "49" = 1963, "50" = 1962, "51" = 1961,
  "52" = 1960, "53" = 1959, "54" = 1958, "55" = 1957, "56" = 1956, "57" = 1955,
  "58" = 1954, "59" = 1953, "60" = 1952, "61" = 1951, "62" = 1950, "63" = 1949,
  "64" = 1948, "65" = 1947, "66" = 1946, "67" = 1945, "68" = 1944, "69" = 1943
)

#---- main transform --------------------------------------------------------

tidyup_dataframe <- function(df_raw, survey_year = 2024, add_english_dupes = TRUE) {

  df_processed <- df_raw %>%
    select(-any_of(drop_cols)) %>%
    mutate(
      gender_code          = as.character(SD01),
      education_code       = as.character(SD04),
      work_env_code        = as.character(SD08),
      employment_code      = as.character(SD09),
      work_intensity_code  = as.character(SD10),
      work_experience_code = as.character(SD11)
    ) %>%
    to_numeric(unlist(variable_groups)) %>%
    mutate(
      birthyear = as.numeric(year_map[as.character(SD02)]),
      age       = survey_year - birthyear,
      colleagues_raw = as.numeric(as.character(SD15)),
      colleagues     = if_else(colleagues_raw == 42, NA_real_, colleagues_raw - 1)
    )

  df_processed <- purrr::reduce(
    .x = names(factor_columns),
    .init = df_processed,
    .f = function(acc, group) {
      cols <- factor_columns[[group]]
      labels <- factor_labels[[group]]
      levels_vec <- seq_along(labels)
      if (group == "gender") levels_vec <- c("2", "3", "4")
      to_factor_safe(acc, cols, levels = levels_vec, labels = labels)
    }
  )

  df_processed <- df_processed %>%
    rename(
      gender                  = SD01,
      education               = SD04,
      teaching_role           = SD05,
      managerial_position     = SD06,
      scientific_authorship   = SD07,
      work_environment        = SD08,
      employment_status       = SD09,
      workload                = SD10,
      work_experience         = SD11,
      parkinson_focus         = SD14,
      postgraduate_training   = K101,
      physician_collaboration = K107_01
    ) %>%
    mutate(
      physician_collaboration_bin = if_else(physician_collaboration >= 4, 1, 0),
      education_label = unname(edu_map[education_code])
    )

  code_to_label <- function(code, labels, levels = NULL) {
    if (is.null(levels)) levels <- as.character(seq_along(labels))
    as.character(factor(code, levels = levels, labels = labels))
  }

  if (isTRUE(add_english_dupes)) {
    df_processed <- df_processed %>%
      mutate(
        en_gender = code_to_label(gender_code, factor_labels_engl$gender, levels = c("2", "3", "4")),
        en_education = unname(edu_map_engl[education_code]),
        en_work_environment = code_to_label(work_env_code, factor_labels_engl$work_env),
        en_employment_status = code_to_label(employment_code, factor_labels_engl$employment),
        en_workload = code_to_label(work_intensity_code, factor_labels_engl$work_intensity),
        en_work_experience = code_to_label(work_experience_code, factor_labels_engl$work_experience)
      )
  }

  df_processed
}
#---- runner (executed on source) ------------------------------------------

if (exists("df_raw", inherits = TRUE)) {
  df_processed <- tidyup_dataframe(df_raw, survey_year = 2024, add_english_dupes = TRUE)
} else {
  message("tidyup_data.R sourced: df_raw not found; function tidyup_dataframe() is available.")
}



################################################################################
# Original code that is maintained in case of debugging issues

old_code <- FALSE
if (old_code){
df_processed <- df_raw %>% select(-c(SERIAL, REF, STARTED, QUESTNNR, MODE, TIME001, TIME002, TIME003, TIME004, TIME005, TIME006, TIME007, TIME_SUM, MAILSENT, LASTDATA, FINISHED, Q_VIEWER, LASTPAGE, MAXPAGE, MISSING, TIME_RSI))

#function to rename data
to_numeric <- function(df, cols) {
  df %>% mutate(across(all_of(cols), as.numeric))
}

#function to apply factor conversion +
###if any valid column remain, convert them to factor
to_factor <- function(df, cols, levels, labels) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {	
    warning("Skipping missing columns: ", paste(missing_cols, collapse = ", "))
    cols <- intersect(cols, names(df))
  }
  
  if (length(cols) > 0) {
    df <- df %>%
      mutate(across(all_of(cols), ~ factor(.x, levels = levels, labels = labels)))
  }
  
  return(df)
}

#grouped variable lists stored in name list
variable_groups <- list(
  therapy_IPS                  = c("BB02"),
  medical_assumptions           = c("IA05_01", "IA05_02", "IA05_03", "IA05_04", "IA05_05"),
  treatment_goals              = c("IA06_01", "IA06_02", "IA06_03", "IA06_04", "IA06_05", "IA06_06", "IA06_07", "IA06_08", "IA06_09"),
  postgraduation               = c("K101"),
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


#define factor labels in a named list (this is now the source of truth)
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

# ---- Recode education SD04 (1–7) ----
edu_map <- c(
  "1" = "Auszubildende/Auszubildender",
  "2" = "Studentin/Student",
  "3" = "Abschluss Ausbildung/Staatsexamen",
  "5" = "Studienabschluss Bachelor",
  "6" = "Studienabschluss Master/Diplom",
  "7" = "Promotion"
)

# ---- Recode age SD02 ----
year_map <- c(
  "72" = 2008,
  "71" = 2007,
  "06" = 2006,
  "07" = 2005,
  "08" = 2004,
  "09" = 2003,
  "10" = 2002,
  "11" = 2001,
  "12" = 2000,
  "13" = 1999,
  "14" = 1998,
  "15" = 1997,
  "16" = 1996,
  "17" = 1995,
  "18" = 1994,
  "19" = 1993,
  "20" = 1992,
  "21" = 1991,
  "22" = 1990,
  "23" = 1989,
  "24" = 1988,
  "25" = 1987,
  "26" = 1986,
  "27" = 1985,
  "28" = 1984,
  "29" = 1983,
  "30" = 1982,
  "31" = 1981,
  "32" = 1980,
  "33" = 1979,
  "34" = 1978,
  "35" = 1977,
  "36" = 1976,
  "37" = 1975,
  "38" = 1974,
  "39" = 1973,
  "40" = 1972,
  "41" = 1971,
  "42" = 1970,
  "43" = 1969,
  "44" = 1968,
  "45" = 1967,
  "46" = 1966,
  "47" = 1965,
  "48" = 1964,
  "49" = 1963,
  "50" = 1962,
  "51" = 1961,
  "52" = 1960,
  "53" = 1959,
  "54" = 1958,
  "55" = 1957,
  "56" = 1956,
  "57" = 1955,
  "58" = 1954,
  "59" = 1953,
  "60" = 1952,
  "61" = 1951,
  "62" = 1950,
  "63" = 1949,
  "64" = 1948,
  "65" = 1947,
  "66" = 1946,
  "67" = 1945,
  "68" = 1944,
  "69" = 1943
)
df_processed$birthyear <- as.numeric(
  year_map[as.character(df_processed$SD02)]
)
df_processed$age <- 2024 - df_processed$birthyear
df_processed$age

df_processed$SD15_raw <- as.numeric(df_raw$SD15)
df_processed$colleagues <- df_processed$SD15_raw - 1
df_processed$colleagues[df_processed$SD15_raw == 42] <- NA
#Options: 42 > 41 colleagues (is no concrete number)
##1: chosen: >41 as NA; IGNORING VALUES
##2: 40 as limit; FALSELY ELEVATED VALUES AT 40 (df_processed$colleagues[df_processed$SD15_raw == 42] <- 40)
##3: for descriptive: (df_processed$colleagues_cat <- factor(ifelse(df_processed$SD15_raw == 42, ">40", df_processed$SD15_raw - 1), ordered = TRUE)

#This list maps column groups to their corresponding label sets
factor_columns <- list(
  referral_information = c("IA01_01", "IA01_02", "IA01_03", "IA01_04", "IA01_05", "IA01_07", "IA01_08", "IA01_09"),
  binary = c("BB02", "K101", "K105", "SD05", "SD06", "SD07", "SD14"),
  gender = "SD01",
  edu = "SD04",
  work_env = "SD08",
  employment = "SD09",
  work_intensity = "SD10",
  work_experience = "SD11"
)

#apply factor conversion for all defined groups
for (group in names(factor_columns)) {
  cols   <- factor_columns[[group]]
  labels <- factor_labels[[group]]
  levels_vec <- seq_along(labels)
  if (group == "gender") {
    levels_vec <- c("2", "3", "4")
  }
  df_processed <- to_factor(df_processed, cols, levels = levels_vec, labels = labels)
}


df_processed <- df_processed %>%
  rename(
    gender                  = SD01,
    education               = SD04,
    teaching_role           = SD05,
    managerial_position     = SD06,
    scientific_authorship   = SD07,
    work_environment        = SD08,
    employment_status       = SD09,
    workload                = SD10,
    work_experience         = SD11,
    parkinson_focus         = SD14,
    postgraduate_training   = K101,
    physician_collaboration = K107_01
  ) %>%
mutate(
  physician_collaboration_bin = if_else(physician_collaboration >= 4, 1, 0)
)
variable_groups$postgraduation <- "postgraduate_training"
variable_groups$interdisciplinary_cooperation <- "physician_collaboration"

df_processed$SD04 <- df_raw$SD04
df_processed$SD04_label <- edu_map[as.character(df_processed$SD04)]
}
