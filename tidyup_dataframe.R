# ===========================================================
# Project: Physiotherapie and Parkinson's Disease
# Script: tidyup_dataframe.R
# Authors: Christian Isselstein and David Pedrosa
# Date: 2025-17-02
# Version: 1.0
# Description: Initial commit
#
# Change log:
#   Version 1.0 (2025-17-02): Rename and refactor data
# ===========================================================

# Function to rename data
# ===========================================================
to_numeric <- function(df, cols) {
  df[cols] <- lapply(df[cols], as.numeric)
  return(df)
}

# Function to apply factor conversion
to_factor <- function(df, col, levels, labels) {
  df[[col]] <- factor(df[[col]], levels = levels, labels = labels)
  return(df)
}

# TODO: In general, I would use english language in general e.g. in "factor mappings". Besides, I would start trimming all
# unnecessary columns. For that you may use the "select command in the tidyverse package"

df_processed <- df_raw %>% select(-c(SERIAL, REF, STARTED)) # You do't have to to this but in case you want, this removes everything in the parentheses. Furthermore avoids accidental deletions as new variable [df_processed] is created. Please add further columns that are not necessary

# Grouped variable lists
medical_assumptions 		<- c("IA05_01", "IA05_02", "IA05_03", "IA05_04", "IA05_05")
treatment_goals 		<- c("IA06_01", "IA06_02", "IA06_03", "IA06_04", "IA06_05", "IA06_06", "IA06_07", "IA06_08", "IA06_09")
patient_centeredness 		<- c("K104_01", "K104_02", "K104_03", "K104_04", "K104_05", "K104_06")
interdisciplinary_cooperation 	<- c("K107_01")
patient_volume 			<- c("K108_01")
resources 			<- c("K201_01", "K201_02", "K201_03", "K201_04", "K201_05", "K201_06")
attitudes 			<- c("K202_01", "K202_02", "K202_03", "K202_04", "K202_05", "K202_06")
intention_behavioral_control 	<- c("K203_01", "K203_02", "K203_03", "K203_04", "K203_05", "K203_06", "K203_08", "K203_09")
subjective_norm 		<- c("K204_01", "K204_02", "K204_03")
barriers_patient 		<- c("K217_01", "K217_02", "K217_03", "K217_04", "K217_05", "K217_06")
barriers_workplace 		<- c("K218_01", "K218_02", "K218_03", "K218_04", "K218_05", "K218_06", "K218_07", "K218_08", "K218_09")
barriers_personal 		<- c("K219_01", "K219_03", "K219_04", "K219_05", "K219_06")

# Apply function to convert selected columns to numeric
df_processed <- to_numeric(df_processed, c(medical_assumptions, treatment_goals, patient_centeredness,
                               interdisciplinary_cooperation, patient_volume, resources, attitudes,
                               intention_behavioral_control, subjective_norm, barriers_patient,
                               barriers_workplace, barriers_personal))

# Rename columns for clarity
colnames(df_processed)[match(medical_assumptions, names(df_processed))] <- 
  paste0("medical_assumptions_", 
         c("anamnesis", "progress_report", "workout_plan", "surrounding_people", "communication_with_doctor"))

colnames(df_processed)[match(treatment_goals, names(df_processed))] <- 
  paste0("treatment_goals_", 
         c("gait_training", "balance", "strength_and_stretching", "aerobic_capacity", "ROM", 
           "movement_initiation", "ADL", "movement_strategy", "fall_prevention"))

colnames(df_processed)[match(patient_centeredness, names(df_processed))] <- 
  paste0("patient_centeredness_", 
         c("information_exchange", "shared_decision_making_patient", "shared_decision_making_surrounding_people", 
           "independent_measures", "physical_well_being", "emotional_support"))

colnames(df_processed)[match(interdisciplinary_cooperation, names(df_processed))] <- 
  "interdisciplinary_cooperation_doctor"

colnames(df_processed)[match(patient_volume, names(df_processed))] <- 
  "patient_volume"

colnames(df_processed)[match(resources, names(df_processed))] <- 
  paste0("resources_", 
         c("database_access", "contact_person_help_research", "protected_time_for_research", 
           "support_training_opportunities", "EBP_teaching", "EBP_equipment"))

colnames(df_processed)[match(attitudes, names(df_processed))] <- 
  paste0("attitude_", 
         c("physios_research", "EBP_raising_competence", "evidence_importance_therapy", 
           "evidence_increases_therapy_quality", "experience_matches_evidence", "evidence_individual_customization"))

colnames(df_processed)[match(intention_behavioral_control, names(df_processed))] <- 
  paste0("intention_behavioral_control_", 
         c("evidence_over_intuition", "easy_formulation_research_questions", "easy_interpretation_statistic", 
           "critical_about_research_findings", "understanding_specialist_terms", "informed_about_new_research", 
           "uses_guideline_IPS", "critical_therapy_outcome_monitoring"))

colnames(df_processed)[match(subjective_norm, names(df_processed))] <- 
  paste0("subjective_norm_", 
         c("workplace_EBP_workplace", "EBP_colleague_motivation", "EBP_colleague_discussion"))

colnames(df_processed)[match(barriers_patient, names(df_processed))] <- 
  paste0("barriers_patient_", 
         c("financial_reasons", "being_late", "socio_cultural_religious", "refusal_of_therapy", 
           "missing_insight_into_illness", "individuality_prevents_generalizability"))

colnames(df_processed)[match(barriers_workplace, names(df_processed))] <- 
  paste0("barriers_workplace_", 
         c("shortage_equipment", "missing_therapy_guideline", "lack_of_time_research", "staff_shortage", 
           "poor_duration_therapy", "lack_training_opportunities", "lack_database_access", "lack_support", 
           "insufficient_interprofessional_medical_cooperation"))

colnames(df_processed)[match(barriers_personal, names(df_processed))] <- 
  paste0("barriers_personal_", 
         c("lack_evidence_knowledge", "lack_evidence_interest", "lack_stat_understanding", "lack_foreign_language", 
           "lack_understanding_content"))

# Define factor mappings
binary_labels <- c("Ja", "Nein")
referral_labels <- c("-", "Auswahl")
gender_labels <- c("weiblich", "männlich", "divers")
work_env_labels <- c("freie Praxis", "Krankenhaus", "Rehabilitationseinrichtung", "Universitätsklinikum")
employment_labels <- c("Auszubildende/Auszubildender_Studentin/Student", "Arbeitnehmerin/Arbeitnehmer", "selbstständig", "freiberuflich")
work_intensity_labels <- c("≤ 19 Stunden/Woche", "20-29 Stunden/Woche", "30-39 Stunden/Woche", "≥ 40 Stunden/Woche")
work_experience_labels <- c("≤ 4 Jahre", "5-9 Jahre", "10-14 Jahre", "15-19 Jahre", "20-24 Jahre", "25-29 Jahre", "30-34 Jahre", "≥35 Jahre")
edu_labels <- c("Auszubildende/Auszubildender", "Studentin/Student", "Abschluss Ausbildung/Staatsexamen", "Studienabschluss Bachelor", "Studienabschluss Master/Diplom", "Promotion")

# Apply factor conversion to columns
df_processed <- to_factor(df_processed, "IA01_01", c(1,2), referral_labels)
df_processed <- to_factor(df_processed, "IA01_02", c(1,2), referral_labels)
df_processed <- to_factor(df_processed, "IA01_03", c(1,2), referral_labels)
df_processed <- to_factor(df_processed, "IA01_04", c(1,2), referral_labels)
df_processed <- to_factor(df_processed, "IA01_05", c(1,2), referral_labels)
df_processed <- to_factor(df_processed, "IA01_07", c(1,2), referral_labels)
df_processed <- to_factor(df_processed, "IA01_08", c(1,2), referral_labels)
df_processed <- to_factor(df_processed, "IA01_09", c(1,2), referral_labels)

df_processed <- to_factor(df_processed, "K101", c(1,2), binary_labels)
df_processed <- to_factor(df_processed, "K105", c(1,2), binary_labels)

df_processed <- to_factor(df_processed, "SD01", c(2,3,4), gender_labels)

df_processed <- to_factor(df_processed, "SD04", c(1,2,3,5,6,7), edu_labels)
df_processed <- to_factor(df_processed, "SD05", c(1,2), binary_labels)
df_processed <- to_factor(df_processed, "SD06", c(1,2), binary_labels)
df_processed <- to_factor(df_processed, "SD07", c(1,2), binary_labels)
df_processed <- to_factor(df_processed, "SD08", c(1,2,3,4), work_env_labels)
df_processed <- to_factor(df_processed, "SD09", c(1,2,3,4), employment_labels)
df_processed <- to_factor(df_processed, "SD10", c(1,2,3,4), work_intensity_labels)
df_processed <- to_factor(df_processed, "SD11", c(1,2,3,4,5,6,7,8), work_experience_labels)
df_processed <- to_factor(df_processed, "SD14", c(1,2), binary_labels)

df_processed <- to_numeric(df_processed, "SD02") %>% rename(age = SD02) # different method on how to rename a variable

# Assign postcode and colleagues as-is
df_processed$postcode <- df_processed$SD13_01
df_processed$colleagues <- df_processed$SD15
