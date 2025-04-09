install.packages("readxl")
install.packages("tableone")
install.packages("modeest")
install.packages("tidyverse")

# TODO: so wie Du das schreibst ist das irgendwie doppelt. DU installierst die Pakete oben, definierst dann die 
# gleichen Pakete noch einmal, lädst die Pakete und dann prüfst Du ca. 30 Zeilen weiter unten, ob die Pakete 
# installierst sind und installierst sie dann. Du brauchst „nur“ required_packages = ... und die folgenden Zeilen 
# und das was bei package_check steht. Ich war mal so frei und habe ein wenig kommentiert.

required_packages <- c(
  "modeest" ,
  "readxl" ,
  "tableone" ,
  "tidyverse")

library(readxl)
library(tableone)
library(modeest)
library(tidyverse)

# Functions of interest
to_numeric <- function(df, cols)
  df[cols] <- lapply(df[cols], as.numeric)
#return(df)

to_factor <- function(df, col, levels, labels)
  df[[col]] <- factor(df[[col]], levels = levels, labels = labels)
#return(df)

package.check <- lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installiere Paket:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    message(paste("Paket", pkg, "ist bereits installiert."))
  }
})

wdir <-"C:/Users/chris/Desktop/Klinik/Doktorarbeit/M. Parkinson/data"
results_dir <- "C:/Users/chris/Desktop/Klinik/Doktorarbeit/M. Parkinson/results/"
data <-"Datensatz.xlsx"

filename <- file.path(wdir, data) # Loads filename of interest

df_raw <- read_xlsx(filename)
col_explanations <- df_raw[1,]
df_raw <- df_raw[-1,]
df_processed <- df_raw %>% select(-c(SERIAL, REF, STARTED, QUESTNNR, MODE, TIME001, TIME002, TIME003, TIME004, TIME005, TIME006, TIME007, TIME_SUM, MAILSENT, LASTDATA, FINISHED, Q_VIEWER, LASTPAGE, MAXPAGE, MISSING, TIME_RSI))

class(df_processed)
df_processed <- as.data.frame(df_processed)
class(df_processed)

# Grouped variable lists - to numeric
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

# Grouped variable lists - to factor
referral_information <- c("IA01_01", "IA01_02", "IA01_03", "IA01_04", "IA01_05", "IA01_06", "IA01_07", "IA01_08", "IA01_09")
advanced_training <- c("K101")
referral_criteria <- c("K105")
gender <- c("SD01")
age <- c("SD02")
education_qualification <- c("SD04")
educational_function <- c("SD05")
leading_position <- c("SD06")
authorship <- c("SD07")
work_env <- c("SD08")
work_empl <- c("SD09")
work_intens <- c("SD10")
work_experience <- c("SD11")
focus_centre <- c("SD14")


# Apply function to convert selected columns to numeric
df_processed <- to_numeric(df_processed, c(medical_assumptions, treatment_goals, patient_centeredness,
                                           interdisciplinary_cooperation, patient_volume, resources, attitudes,
                                           intention_behavioral_control, subjective_norm, barriers_patient,
                                           barriers_workplace, barriers_personal))


# Apply function to add/convert selected variables to factor
## Test 1
##df_processed <- df_processed %>%
  ##mutate(new_column = factor(referral_information, advanced_training, referral_criteria, gender, age, education_qualification, educational_function, 
    ##                         leading_position, authorship, work_env, work_empl, work_intens, work_experience, focus_centre))

## Test 2
##df_processed <- to_factor(df_processed, c("referral_information", "advanced_training", "referral_criteria", "gender", "age", "education_qualification", "educational_function", 
  ##                                        "leading_position", "authorship", "work_env", "work_empl", "work_intens", "work_experience", "focus_centre"))

## Test 3
##df_processed <- to_numeric(df_processed, c(medical_assumptions, treatment_goals, patient_centeredness,
  ##                                         interdisciplinary_cooperation, patient_volume, resources, attitudes,
    ##                                       intention_behavioral_control, subjective_norm, barriers_patient,
      ##                                     barriers_workplace, barriers_personal)) + 
  ##to_factor(df_processed, c("referral_information", "advanced_training", "referral_criteria", "gender", "age", "education_qualification", "educational_function", 
    ##                        "leading_position", "authorship", "work_env", "work_empl", "work_intens", "work_experience", "focus_centre"))


class(df_processed)
df_processed <- as.data.frame(df_processed)
class(df_processed)

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

# function to rename data
## to_numeric <- function(df, cols) 
## df[cols] <- lapply(df[cols], as.numeric)
## return(df)

# Function to apply factor conversion
## to_factor <- function(df, col, levels, labels)
## df[[col]] <- factor(df[[col]], levels = levels, labels = labels)
## return(df)

-----------------------------
  
#TableOne - Try
allVars <- c(	"SD01", "SD11", "SD08",
              "age", "patient_volume")

catVars <- c(	"SD01", "SD11", "SD08"	)

NumVars <- c(	"age", "patient_volume"	)

tab1 <- CreateTableOne(vars = allVars,
                       data = df_processed,
                       factorVars = catVars, addOverall = TRUE)
print(tab1)
write.csv(print(tab1, quote = FALSE, test=FALSE, contDigits = 1,
                noSpaces = TRUE, printToggle = FALSE, showAllLevels = FALSE),
          file = file.path(wdir, "results", "table1_demographics.csv"))
