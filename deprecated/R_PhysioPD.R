install.packages("readxl")
install.packages("tableone")
install.packages("modeest")

library(readxl)
library(tableone)
library(modeest)

# General part to define filename and load (raw) data

wdir <-"C:/Users/chris/Desktop/Klinik/Doktorarbeit/M. Parkinson/Datenauswertung"
results_dir <- "C:/Users/chris/Desktop/Klinik/Doktorarbeit/M. Parkinson/results/"
data <-"Datensatz.xlsx"

filename <- file.path(wdir, data) # Loads filename of interest

# TODO: 

df_raw <- read_xlsx(filename)
erklaerugen <- df_raw[1,]
df_raw <- df_raw[-1,]

# Start recording/refactoring

df_raw$medical_assumptions_anamnesis <- as.numeric(df_raw$IA05_01)
df_raw$medical_assumptions_progress_report <- as.numeric(df_raw$IA05_02)
df_raw$medical_assumptions_workout_plan <- as.numeric(df_raw$IA05_03)
df_raw$medical_assumptions_surrounding_people <- as.numeric(df_raw$IA05_04)
df_raw$medical_assumptions_communication_with_doctor <- as.numeric(df_raw$IA05_05)

df_raw$treatment_goals_gait_training <- as.numeric(df_raw$IA06_01)
df_raw$treatment_goals_balance <- as.numeric(df_raw$IA06_02)
df_raw$treatment_goals_strength_and_stretching <- as.numeric(df_raw$IA06_03)
df_raw$treatment_goals_aerobic_capacity <- as.numeric(df_raw$IA06_04)
df_raw$treatment_goals_ROM <- as.numeric(df_raw$IA06_05)
df_raw$treatment_goals_movement_initiation <- as.numeric(df_raw$IA06_06)
df_raw$treatment_goals_ADL <- as.numeric(df_raw$IA06_07)
df_raw$treatment_goals_movement_strategy <- as.numeric(df_raw$IA06_08)
df_raw$treatment_goals_fall_prevention <- as.numeric(df_raw$IA06_09)

df_raw$patient_centeredness_information_exchange <- as.numeric(df_raw$K104_01)
df_raw$patient_centeredness_shared_decision_making_patient <- as.numeric(df_raw$K104_02)
df_raw$patient_centeredness_shared_decision_making_sorrounding_people <- as.numeric(df_raw$K104_03)
df_raw$patient_centeredness_independent_measures <- as.numeric(df_raw$K104_04)
df_raw$patient_centeredness_physical_well_being <- as.numeric(df_raw$K104_05)
df_raw$patient_centeredness_emotional_support <- as.numeric(df_raw$K104_06)

df_raw$interdisciplinary_cooperation_doctor <- as.numeric(df_raw$K107_01)

df_raw$patient_volume <- as.numeric(df_raw$K108_01)

df_raw$reccources_database_access <- as.numeric(df_raw$K201_01)
df_raw$reccources_contact_person_help_research <- as.numeric(df_raw$K201_02)
df_raw$reccources_protected_time_for_research <- as.numeric(df_raw$K201_03)
df_raw$reccources_support_training_opportunities <- as.numeric(df_raw$K201_04)
df_raw$reccources_EBP_teaching <- as.numeric(df_raw$K201_05)
df_raw$reccources_EBP_equipment <- as.numeric(df_raw$K201_06)

df_raw$attitude_physios_research <- as.numeric(df_raw$K202_01)
df_raw$attitude_EBP_raising_competence <- as.numeric(df_raw$K202_02)
df_raw$attitude_evidence_importance_therapy <- as.numeric(df_raw$K202_03)
df_raw$attitude_evidence_increases_therapy_quality <- as.numeric(df_raw$K202_04)
df_raw$attitude_experience_matches_evidence <- as.numeric(df_raw$K202_05)
df_raw$attitude_evidence_individual_customization <- as.numeric(df_raw$K202_06)

df_raw$intention_behavioral_control_evidence_over_intuition <- as.numeric(df_raw$K203_01)
df_raw$intention_behavioral_control_easy_formulation_research_questions <- as.numeric(df_raw$K203_02)
df_raw$intention_behavioral_control_easy_interpretation_statistic <- as.numeric(df_raw$K203_03)
df_raw$intention_behavioral_control_criticial_about_research_findings <- as.numeric(df_raw$K203_04)
df_raw$intention_behavioral_control_understanding_specialist_terms <- as.numeric(df_raw$K203_05)
df_raw$intention_behavioral_control_informated_about_new_research <- as.numeric(df_raw$K203_06)
df_raw$intention_behavioral_control_uses_guideline_IPS <- as.numeric(df_raw$K203_08)
df_raw$intention_behavioral_control_critical_therapy_outcome_monitoring <- as.numeric(df_raw$K203_09)

df_raw$subjective_norm_workplace_EBP_workplace <- as.numeric(df_raw$K204_01)
df_raw$subjective_norm_EBP_colleague_motivation <- as.numeric(df_raw$K204_02)
df_raw$subjective_norm_EBP_colleague_discussion <- as.numeric(df_raw$K204_03)

df_raw$barriers_patient_financial_reasons <- as.numeric(df_raw$K217_01)
df_raw$barriers_patient_being_late <- as.numeric(df_raw$K217_02)
df_raw$barriers_patient_socio_cultural_religious <- as.numeric(df_raw$K217_03)
df_raw$barriers_patient_refusal_of_therapy <- as.numeric(df_raw$K217_04)
df_raw$barriers_patient_missing_insight_into_illness <- as.numeric(df_raw$K217_05)
df_raw$barriers_patient_individuality_prevents_generalizability <- as.numeric(df_raw$K217_06)

df_raw$barriers_workplace_shortage_equipment <- as.numeric(df_raw$K218_01)
df_raw$barriers_workplace_missing_therapy_guideline <- as.numeric(df_raw$K218_02)
df_raw$barriers_workplace_lack_of_time_research <- as.numeric(df_raw$K218_03)
df_raw$barriers_workplace_staff_shortage <- as.numeric(df_raw$K218_04)
df_raw$barriers_workplace_poor_duration_therapy <- as.numeric(df_raw$K218_05)
df_raw$barriers_workplace_lack_training_opportunities <- as.numeric(df_raw$K218_06)
df_raw$barriers_workplace_lack_database_access <- as.numeric(df_raw$K218_07)
df_raw$barriers_workplace_lack_support <- as.numeric(df_raw$K218_08)
df_raw$barriers_workplace_insufficient_interprofessional_medical_cooperation <- as.numeric(df_raw$K218_09)

df_raw$barriers_personal_lack_evidence_knowledge <- as.numeric(df_raw$K219_01)
df_raw$barriers_personal_lack_evidence_interest <- as.numeric(df_raw$K219_03)
df_raw$barriers_personal_lack_stat_understanding <- as.numeric(df_raw$K219_04)
df_raw$barriers_personal_lack_foreign_language <- as.numeric(df_raw$K219_05)
df_raw$barriers_personal_lack_understanding_content <- as.numeric(df_raw$K219_06)

df_raw$referral_reason <- factor(df_raw$IA01_01,
                                 levels = c(1,2),
                                 labels = c("-","Auswahl"))

df_raw$referral_diagnosis_incl._IPS_vs_aPD <- factor(df_raw$IA01_02,
                                                     levels = c(1,2),
                                                     labels = c("-","Auswahl"))

df_raw$referral_diagnosis_yearID_stadium <- factor(df_raw$IA01_03,
                                                   levels = c(1,2),
                                                   labels = c("-","Auswahl"))
df_raw$referral_motor_complications <- factor(df_raw$IA01_04,
                                              levels = c(1,2),
                                              labels = c("-","Auswahl"))
df_raw$referral_cognitive_problems <- factor(df_raw$IA01_05,
                                             levels = c(1,2),
                                             labels = c("-","Auswahl"))
df_raw$referral_secondary_diseases <- factor(df_raw$IA01_06,
                                             levels = c(1,2),
                                             labels = c("-","Auswahl"))
df_raw$referral_common_medis_surgical_treatment <- factor(df_raw$IA01_07,
                                                          levels = c(1,2),
                                                          labels = c("-","Auswahl"))
df_raw$referral_done_treatments_results <- factor(df_raw$IA01_08,
                                                  levels = c(1,2),
                                                  labels = c("-","Auswahl"))
df_raw$referral_expected_results_of_therapy <- factor(df_raw$IA01_09,
                                                      levels = c(1,2),
                                                      labels = c("-","Auswahl"))

df_raw$advanced_training <- factor(df_raw$K101,
                                   levels = c(1,2),
                                   labels = c("Ja", "Nein"))

df_raw$referral_criteria <- factor(df_raw$K105,
                                   levels = c(1,2),
                                   labels = c("Ja", "Nein"))

df_raw$gender <- factor(df_raw$SD01,
                        levels = c(2,3,4),
                        labels = c("weiblich","männlich","divers"))

df_raw$age <- factor(df_raw$SD02,
                     levels = c(72,71,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69),
                     labels = c(2008,2007,2006,2005,2004,2003,2002,2001,2000,1999,1998,1997,1996,1995,1994,1993,1992,1991,1990,1989,1988,1987,1986,1985,1984,1983,1982,1981,1980,1979,1978,1977,1976,1975,1974,1973,1972,1971,1970,1969,1968,1967,1966,1965,1964,1963,1962,1961,1960,1959,1958,1957,1956,1955,1954,1953,1952,1951,1950,1949,1948,1947,1946,1945,1944,1943))

df_raw$educational_qualification <- factor(df_raw$SD04,
                                           levels = c(1,2,3,5,6,7),
                                           labels = c("Auszubildende/Auszubildender", "Studentin/Student", "Abschluss Ausbildung/Staatsexamen", "Studienabschluss Bachelor", "Studienabschluss Master/Diplom", "Promotion"))

df_raw$educational_function <- factor(df_raw$SD05,
                                      levels = c(1,2),
                                      labels = c("Ja", "Nein"))

df_raw$leading_position <- factor(df_raw$SD06,
                                  levels = c(1,2),
                                  labels = c("Ja", "Nein"))

df_raw$authorship <- factor(df_raw$SD07,
                            levels = c(1,2),
                            labels = c("Ja", "Nein"))

df_raw$work_environment <- factor(df_raw$SD08,
                                  levels = c(1,2,3,4),
                                  labels = c("freie Praxis", "Krankenhaus", "Rehabilitationseinrichtung", "Universitätsklinikum" ))

df_raw$employment_relationship <- factor(df_raw$SD09,
                                         levels = c(1,2,3,4),
                                         labels = c("Auszubildende/Auszubildender_Studentin/Studen", "Arbeitnehmerin/Arbeitnehmer", "selbstständig", "freiberuflich"))

df_raw$work_intensity <- factor(df_raw$SD10,
                                levels = c(1,2,3,4),
                                labels = c("≤ 19 Stunden/Woche", "20-29 Stunden/Woche", "30-39 Stunden/Woche", "≥ 40 Stunden/Woche"))

df_raw$work_experience <- factor(df_raw$SD11,
                                 levels = c(1,2,3,4,5,6,7,8),
                                 labels = c("≤ 4 Jahre","5-9 Jahre","10-14 Jahre","15-19 Jahre","20-24 Jahre","25-29 Jahre","30-34 Jahre","≥35 Jahre"))

df_raw$focus_centre <- factor(df_raw$SD14,
                              levels = c(1,2),
                              labels = c("Ja","Nein"))

df_raw$postcode <- df_raw$SD13_01
class(df_raw$postcode)

df_raw$colleagues <- df_raw$SD15
class(df_raw$SD15)
