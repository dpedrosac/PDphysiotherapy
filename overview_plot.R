# ===========================================================
# Project: Physiotherapie and Parkinson's Disease
# Script: overview_plot.R
# Authors: Christian Isselstein and David Pedrosa
# Process: Items to numeric -> Scores per participant -> Outcome dichotomizing -> Define predictors -> univariable log. regr. for each predictor -> OR, 95%CI, p-Values -> Saving results -> Forest plot (Exporting)
# TODO: ...
# DONE: Renaming the predictors, age10: per 10 years, colleagues5: per 5 colleagues, patient_volume5: per 5 patients per year, EDUCATION as ordinal variable per level, same with workload und work experience, redesign plots
df_processed_saved <- df_processed

df_processed <- df_processed %>%
  mutate(
    age10        = age / 10,
    colleagues5  = colleagues / 5, 
    patient_volume5 = patient_volume / 5
  )

df_processed <- df_processed %>%
  mutate(
    education_ord = case_when(
      education == "Auszubildende/Auszubildender" ~ "in Ausbildung",
      education == "Studentin/Student" ~ "im Studium",
      education == "Abschluss Ausbildung/Staatsexamen" ~ "staatl. Abschluss",
      education == "Studienabschluss Bachelor" ~ "Bachelor",
      education == "Studienabschluss Master/Diplom" ~ "Master/Diplom",
      education == "Promotion" ~ "Promotion",
      TRUE ~ NA_character_
    ),
    education_ord = factor(
      education_ord,
      levels = c("in Ausbildung","im Studium","staatl. Abschluss","Bachelor","Master/Diplom","Promotion"),
      ordered = TRUE
    ),
    education_trend = as.numeric(education_ord)
  )

df_processed <- df_processed %>%
  mutate(
    workload_ord = case_when(
      workload == "≤ 19 Stunden/Woche" ~ "≤19",
      workload == "20-29 Stunden/Woche" ~ "20-29",
      workload == "30-39 Stunden/Woche" ~ "30-39",
      workload == "≥ 40 Stunden/Woche" ~ "≥40",
      TRUE ~ NA_character_
    ),
    workload_ord = factor(
      workload_ord,
      levels = c("≤19","20-29","30-39","≥40"),
      ordered = TRUE
    ),
    workload_trend = as.numeric(workload_ord)
  )
df_processed <- df_processed %>%
  mutate(
    workexp_ord = case_when(
      work_experience == "≤ 4 Jahre" ~ "≤4",
      work_experience == "5-9 Jahre" ~ "5-9",
      work_experience == "10-14 Jahre" ~ "10-14",
      work_experience == "15-19 Jahre" ~ "15-19",
      work_experience == "20-24 Jahre" ~ "20-24",
      work_experience == "25-29 Jahre" ~ "25-29",
      work_experience == "30-34 Jahre" ~ "30-34",
      work_experience == "≥ 35 Jahre" ~ "≥35",
      TRUE ~ NA_character_
    ),
    workexp_ord = factor(
      workexp_ord,
      levels = c("≤4","5-9","10-14","15-19","20-24","25-29","30-34","≥35"),
      ordered = TRUE
    ),
    workexp_trend = as.numeric(workexp_ord)
  )

# Defined predictors
predictors <- c("gender", "age10", "education_trend", "teaching_role", "managerial_position", "scientific_authorship", "work_environment", "employment_status", "workload_trend", "workexp_trend", "parkinson_focus", "colleagues5", "postgraduate_training", "physician_collaboration_bin", "patient_volume5")

predictor_labels <- c(gender="Gender", age10="Age (per 10 years)", education_trend="Education (ordinal, per level)", teaching_role="Teaching role / clinical instructor", managerial_position="Managerial position", scientific_authorship="Authorship of scientific publications", work_environment="Work setting", employment_status="Employment status", workload_trend="Workload (per level)", workexp_trend="Work experience (per level)", parkinson_focus="Parkinson focus at workplace", colleagues5="Number of physiotherapy colleagues (per 5)", postgraduate_training="Postgraduate training", physician_collaboration_bin="Physician collaboration", patient_volume5="Patient volume (per 5 patients)")

################################################
################## RESOURCES ###################
################################################

# Univariable OR overview Resources vs all socialdemographics
variable_groups$resources
df_processed <- df_processed %>%
  mutate(across(all_of(variable_groups$resources), ~ as.numeric(as.character(.))))
df_processed <- df_processed %>%
  mutate(
    resources_score = rowMeans(select(., all_of(variable_groups$resources)), na.rm = TRUE)
  )
# Dichotomizing: ≥4 = yes, <4 = no
df_OR_resources <- df_processed %>%
  mutate(
    dv = factor(
      ifelse(resources_score >= 4, "yes", "no"), 
      levels = c("no", "yes")
    )
  ) %>%
  filter(!is.na(dv))

# Check score range
summary(df_OR_resources$resources_score)
# Check dichotomized outcome
table(df_OR_resources$dv)

# define predictors
df_OR_resources$teaching_role <- relevel(factor(df_OR_resources$teaching_role), ref = "Nein")
df_OR_resources$managerial_position <- relevel(factor(df_OR_resources$managerial_position), ref = "Nein")
df_OR_resources$scientific_authorship <- relevel(factor(df_OR_resources$scientific_authorship), ref = "Nein")
df_OR_resources$parkinson_focus <- relevel(factor(df_OR_resources$parkinson_focus), ref = "Nein")
df_OR_resources$postgraduate_training <- relevel(factor(df_OR_resources$postgraduate_training), ref = "Nein")
df_OR_resources$education_trend <- as.numeric(df_OR_resources$education_trend)
df_OR_resources$workexp_trend <- as.numeric(df_OR_resources$workexp_trend)
df_OR_resources$workload_trend <- as.numeric(df_OR_resources$workload_trend)

df_OR_resources$gender <- relevel(
  factor(df_OR_resources$gender),
  ref = "männlich"   # men as reference
)
df_OR_resources$age10 <- as.numeric(df_OR_resources$age10)
df_OR_resources$work_environment <- relevel(factor(df_OR_resources$work_environment),
                                ref = "freie Praxis")  # freie Praxis as reference
df_OR_resources$employment_status <- relevel(
  factor(df_OR_resources$employment_status),
  ref = "Arbeitnehmerin/Arbeitnehmer"   # as reference
)

df_OR_resources$colleagues5 <- as.numeric(df_OR_resources$colleagues5)
# K107_01 Likert-scale -> convert binary
df_OR_resources <- df_OR_resources %>%
  mutate(
    physician_collaboration_bin = factor(
      ifelse(as.numeric(physician_collaboration) >= 4, "Ja", "Nein"),
      levels = c("Nein", "Ja")
    )
  )
df_OR_resources$physician_collaboration_bin <- relevel(
  df_OR_resources$physician_collaboration_bin,
  ref = "Nein"
)
df_OR_resources$patient_volume5 <- as.numeric(df_OR_resources$patient_volume5)
df_OR_resources$K108_01 <- as.numeric(df_OR_resources$K108_01)


### OR, 95 %-CI, p-Wert
univ_results_resources <- map_df(predictors, function(var) {
  
  f <- as.formula(paste("dv ~", var))
  m <- glm(f, data = df_OR_resources, family = binomial)
  
  broom::tidy(m) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96 * std.error),
      CI_high = exp(estimate + 1.96 * std.error),
      predictor = var
    ) %>%
    select(predictor, term, OR, CI_low, CI_high, p.value)
})
print(univ_results_resources, n = Inf)
summary(univ_results_resources$OR)


#TODO: OK, let's stay here for a moment. Just a few comments. It's not perfect to mix continous data with
# ordinal or nominal data. You can see what happens in your plot, e.g. when looking at age. The CI shrinks
# a lot (and the interpretation is fundamentally different). E.g. a statistical significance in age would
# mean that with every year of age the odds for the dependent variable increase/decrease. It's ok, whereas
# it there might be some variable which you cannot understand that easy. For example in your data the num-
# ber of colleagues means that with every other colleague the odds increase significantly. To me, not really
# intuitive. Another reason why you are taking away yourself the variance is that you are having some odd
# comparisons. Instead of testing bachelor against master, auszubildende agains bachelor, etc. why not 
# making it ordinal. You can easily argue that with more education, the odds for feeling more qualified
# should increase. There are some examples I would consider in your case as ordinal data. The interpretation 
# could be that with more education there is a higher/lower resourcess score.
# Minor comment: I would not rename/refactor anything in your source data, as you may only hardly be able 
# to debug problems. It helps, if you add a line at the beginning like: df_processed_saved <- df_processed 

# Plot: order by predictors
plot_df <- univ_results_resources %>%
  mutate(
    predictor = factor(predictor, levels = predictors),
    predictor_label = unname(predictor_labels[predictor]),
    level = str_remove(term, paste0("^", predictor)),
    level = ifelse(level == "" | is.na(level), "", level),
    row_label = ifelse(level == "",
                       predictor_label,
                       paste0(predictor_label, ": ", level))
  ) %>%
  arrange(predictor, term) %>%
  mutate(row_label = factor(row_label, levels = rev(unique(row_label))))
# axis
xmin <- 0.2
xmax <- 10
plot_df <- plot_df %>%
  mutate(
    OR_plot      = pmin(pmax(OR, xmin), xmax),
    CI_low_plot  = pmin(pmax(CI_low, xmin), xmax),
    CI_high_plot = pmin(pmax(CI_high, xmin), xmax), 
    # TODO: if you add this, it looks nicer:
    row_label = fct_reorder(row_label, OR, .desc = TRUE)     
  )
p <- ggplot(plot_df, aes(x = OR_plot, y = row_label)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = CI_low_plot, xmax = CI_high_plot), height = 0.2) +
  geom_point(size = 1.8) +
  scale_x_log10(limits = c(xmin, xmax)) +
  labs(
    x = "Odds Ratio (log scale) [95% CI; capped for readability]",
    y = NULL,
    title = "Forest plot (sorted by pre-specified predictor order): Resources (high vs low)"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())
p
# Exporting
ggsave(file.path(wdir, "results", "forestplot_resources.png"), plot = p, width = 8, height = 10, dpi = 300)
ggsave(file.path(wdir, "results", "forestplot_resources.pdf"), plot = p, width = 8, height = 10)

############################################################
####################### ATTITUDES ##########################
############################################################

variable_groups$attitudes

df_processed <- df_processed %>%
  mutate(across(all_of(variable_groups$attitudes), ~ as.numeric(as.character(.)))) %>%
  mutate(
    attitudes_score = rowMeans(select(., all_of(variable_groups$attitudes)), na.rm = TRUE)
  )

df_OR_attitudes <- df_processed %>%
  mutate(
    dv = factor(ifelse(attitudes_score >= 4, "yes", "no"), levels = c("no", "yes"))
  ) %>%
  filter(!is.na(dv))

# Check
summary(df_OR_attitudes$attitudes_score)
table(df_OR_attitudes$dv)

# define predictors
df_OR_attitudes$teaching_role <- relevel(factor(df_OR_attitudes$teaching_role), ref = "Nein")
df_OR_attitudes$managerial_position <- relevel(factor(df_OR_attitudes$managerial_position), ref = "Nein")
df_OR_attitudes$scientific_authorship <- relevel(factor(df_OR_attitudes$scientific_authorship), ref = "Nein")
df_OR_attitudes$parkinson_focus <- relevel(factor(df_OR_attitudes$parkinson_focus), ref = "Nein")
df_OR_attitudes$postgraduate_training <- relevel(factor(df_OR_attitudes$postgraduate_training), ref = "Nein")
df_OR_attitudes$education_trend <- as.numeric(df_OR_attitudes$education_trend)
df_OR_attitudes$workexp_trend <- as.numeric(df_OR_attitudes$workexp_trend)
df_OR_attitudes$workload_trend <- as.numeric(df_OR_attitudes$workload_trend)


df_OR_attitudes$gender <- relevel(factor(df_OR_attitudes$gender), ref = "männlich")

df_OR_attitudes$age10 <- as.numeric(df_OR_attitudes$age10)

df_OR_attitudes$work_environment <- relevel(factor(df_OR_attitudes$work_environment), ref = "freie Praxis")
df_OR_attitudes$employment_status <- relevel(factor(df_OR_attitudes$employment_status), ref = "Arbeitnehmerin/Arbeitnehmer")

df_OR_attitudes$colleagues5 <- as.numeric(df_OR_attitudes$colleagues5)

# K107_01 Likert-scale -> convert binary
df_OR_attitudes <- df_OR_attitudes %>%
  mutate(
    physician_collaboration_bin = factor(ifelse(as.numeric(physician_collaboration) >= 4, "Ja", "Nein"),
                         levels = c("Nein", "Ja"))
  )
df_OR_attitudes$physician_collaboration_bin <- relevel(df_OR_attitudes$physician_collaboration_bin, ref = "Nein")

df_OR_attitudes$K108_01 <- as.numeric(df_OR_attitudes$K108_01)
df_OR_attitudes$patient_volume5 <- as.numeric(df_OR_attitudes$patient_volume5)

# OR, 95%-CI, p-Wert
univ_results_attitudes <- map_df(predictors, function(var) {
  
  f <- as.formula(paste("dv ~", var))
  m <- glm(f, data = df_OR_attitudes, family = binomial)
  
  broom::tidy(m) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96 * std.error),
      CI_high = exp(estimate + 1.96 * std.error),
      predictor = var
    ) %>%
    select(predictor, term, OR, CI_low, CI_high, p.value)
})

print(univ_results_attitudes, n = Inf)
summary(univ_results_attitudes$OR)

# Forest plot
plot_df <- univ_results_attitudes %>%
  mutate(
    predictor = factor(predictor, levels = predictors),
    predictor_label = unname(predictor_labels[predictor]),
    level = str_remove(term, paste0("^", predictor)),
    level = ifelse(level == "" | is.na(level), "", level),
    row_label = ifelse(level == "",
                       predictor_label,
                       paste0(predictor_label, ": ", level))
  ) %>%
  arrange(predictor, term) %>%
  mutate(row_label = factor(row_label, levels = rev(unique(row_label))))

xmin <- 0.2; xmax <- 10
plot_df <- plot_df %>%
  mutate(
    OR_plot      = pmin(pmax(OR, xmin), xmax),
    CI_low_plot  = pmin(pmax(CI_low, xmin), xmax),
    CI_high_plot = pmin(pmax(CI_high, xmin), xmax), 
    row_label = fct_reorder(row_label, OR, .desc = TRUE)     
  )

p <- ggplot(plot_df, aes(x = OR_plot, y = row_label)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = CI_low_plot, xmax = CI_high_plot), height = 0.2) +
  geom_point(size = 1.8) +
  scale_x_log10(limits = c(xmin, xmax)) +
  labs(
    x = "Odds Ratio (log scale) [95% CI; capped for readability]",
    y = NULL,
    title = "Forest plot (sorted by pre-specified predictor order): Attitudes (high vs low)"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

p

# Export
ggsave(file.path(wdir, "results", "forestplot_attitudes.png"), plot = p, width = 8, height = 10, dpi = 300)
ggsave(file.path(wdir, "results", "forestplot_attitudes.pdf"), plot = p, width = 8, height = 10)

############################################################
########### INTENTION & BEHAVIORAL CONTROL #################
############################################################

variable_groups$intention_behavioral_control
df_processed <- df_processed %>%
  mutate(across(all_of(variable_groups$intention_behavioral_control), ~ as.numeric(as.character(.)))) %>%
  mutate(
    intention_bc_score = rowMeans(
      select(., all_of(variable_groups$intention_behavioral_control)),
      na.rm = TRUE
    )
  )

# Dichotomizing: ≥4 = yes, <4 = no
df_OR_intention_bc <- df_processed %>%
  mutate(
    dv = factor(ifelse(intention_bc_score >= 4, "yes", "no"),
                levels = c("no", "yes"))
  ) %>%
  filter(!is.na(dv))

# Checks
summary(df_OR_intention_bc$intention_bc_score)
table(df_OR_intention_bc$dv)

# define predictors
df_OR_intention_bc$teaching_role <- relevel(factor(df_OR_intention_bc$teaching_role), ref = "Nein")
df_OR_intention_bc$managerial_position <- relevel(factor(df_OR_intention_bc$managerial_position), ref = "Nein")
df_OR_intention_bc$scientific_authorship <- relevel(factor(df_OR_intention_bc$scientific_authorship), ref = "Nein")
df_OR_intention_bc$parkinson_focus <- relevel(factor(df_OR_intention_bc$parkinson_focus), ref = "Nein")
df_OR_intention_bc$postgraduate_training <- relevel(factor(df_OR_intention_bc$postgraduate_training), ref = "Nein")
df_OR_intention_bc$education_trend <- as.numeric(df_OR_intention_bc$education_trend)
df_OR_intention_bc$workexp_trend <- as.numeric(df_OR_intention_bc$workexp_trend)
df_OR_intention_bc$workload_trend <- as.numeric(df_OR_intention_bc$workload_trend)


df_OR_intention_bc$gender <- relevel(factor(df_OR_intention_bc$gender), ref = "männlich")

df_OR_intention_bc$age10 <- as.numeric(df_OR_intention_bc$age10)

df_OR_intention_bc$work_environment <- relevel(factor(df_OR_intention_bc$work_environment), ref = "freie Praxis")
df_OR_intention_bc$employment_status <- relevel(factor(df_OR_intention_bc$employment_status), ref = "Arbeitnehmerin/Arbeitnehmer")

df_OR_intention_bc$colleagues5 <- as.numeric(df_OR_intention_bc$colleagues5)

# K107_01 Likert-scale -> convert binary
df_OR_intention_bc <- df_OR_intention_bc %>%
  mutate(
    physician_collaboration_bin = factor(
      ifelse(as.numeric(physician_collaboration) >= 4, "Ja", "Nein"),
      levels = c("Nein", "Ja")
    )
  )
df_OR_intention_bc$physician_collaboration_bin <- relevel(
  df_OR_intention_bc$physician_collaboration_bin,
  ref = "Nein"
)

df_OR_intention_bc$K108_01 <- as.numeric(df_OR_intention_bc$K108_01)
df_OR_intention_bc$patient_volume5 <- as.numeric(df_OR_intention_bc$patient_volume5)

# OR, 95%-CI, p-Wert
univ_results_intention_bc <- map_df(predictors, function(var) {
  
  f <- as.formula(paste("dv ~", var))
  m <- glm(f, data = df_OR_intention_bc, family = binomial)
  
  broom::tidy(m) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96 * std.error),
      CI_high = exp(estimate + 1.96 * std.error),
      predictor = var
    ) %>%
    select(predictor, term, OR, CI_low, CI_high, p.value)
})

print(univ_results_intention_bc, n = Inf)
summary(univ_results_intention_bc$OR)

# Forest plot
plot_df <- univ_results_intention_bc %>%
  mutate(
    predictor = factor(predictor, levels = predictors),
    predictor_label = unname(predictor_labels[predictor]),
    level = str_remove(term, paste0("^", predictor)),
    level = ifelse(level == "" | is.na(level), "", level),
    row_label = ifelse(level == "",
                       predictor_label,
                       paste0(predictor_label, ": ", level))
  ) %>%
  arrange(predictor, term) %>%
  mutate(row_label = factor(row_label, levels = rev(unique(row_label))))

xmin <- 0.2; xmax <- 10
plot_df <- plot_df %>%
  mutate(
    OR_plot      = pmin(pmax(OR, xmin), xmax),
    CI_low_plot  = pmin(pmax(CI_low, xmin), xmax),
    CI_high_plot = pmin(pmax(CI_high, xmin), xmax), 
    row_label = fct_reorder(row_label, OR, .desc = TRUE)     
  )

p <- ggplot(plot_df, aes(x = OR_plot, y = row_label)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = CI_low_plot, xmax = CI_high_plot), height = 0.2) +
  geom_point(size = 1.8) +
  scale_x_log10(limits = c(xmin, xmax)) +
  labs(
    x = "Odds Ratio (log scale) [95% CI; capped for readability]",
    y = NULL,
    title = "Forest plot (sorted by pre-specified predictor order): Intention & behavioral control (high vs low)"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

p

# Export
ggsave(file.path(wdir, "results", "forestplot_intention_behavioral_control.png"),
       plot = p, width = 8, height = 10, dpi = 300)
ggsave(file.path(wdir, "results", "forestplot_intention_behavioral_control.pdf"),
       plot = p, width = 8, height = 10)

############################################################
#################### SUBJECTIVE NORM ########################
############################################################

# Univariable OR overview Subjective norm vs all socialdemographics
variable_groups$subjective_norm

# Items -> numeric, Score
df_processed <- df_processed %>%
  mutate(across(all_of(variable_groups$subjective_norm), ~ as.numeric(as.character(.)))) %>%
  mutate(
    subjective_norm_score = rowMeans(
      select(., all_of(variable_groups$subjective_norm)),
      na.rm = TRUE
    )
  )

# Dichotomizing: ≥4 = yes, <4 = no
df_OR_subjective_norm <- df_processed %>%
  mutate(
    dv = factor(ifelse(subjective_norm_score >= 4, "yes", "no"),
                levels = c("no", "yes"))
  ) %>%
  filter(!is.na(dv))

# Checks
summary(df_OR_subjective_norm$subjective_norm_score)
table(df_OR_subjective_norm$dv)

# define predictors (IDENTISCH)
df_OR_subjective_norm$teaching_role <- relevel(factor(df_OR_subjective_norm$teaching_role), ref = "Nein")
df_OR_subjective_norm$managerial_position <- relevel(factor(df_OR_subjective_norm$managerial_position), ref = "Nein")
df_OR_subjective_norm$scientific_authorship <- relevel(factor(df_OR_subjective_norm$scientific_authorship), ref = "Nein")
df_OR_subjective_norm$parkinson_focus <- relevel(factor(df_OR_subjective_norm$parkinson_focus), ref = "Nein")
df_OR_subjective_norm$postgraduate_training <- relevel(factor(df_OR_subjective_norm$postgraduate_training), ref = "Nein")
df_OR_subjective_norm$education_trend <- as.numeric(df_OR_subjective_norm$education_trend)
df_OR_subjective_norm$workexp_trend <- as.numeric(df_OR_subjective_norm$workexp_trend)
df_OR_subjective_norm$workload_trend <- as.numeric(df_OR_subjective_norm$workload_trend)


df_OR_subjective_norm$gender <- relevel(factor(df_OR_subjective_norm$gender), ref = "männlich")

df_OR_subjective_norm$age10 <- as.numeric(df_OR_subjective_norm$age10)

df_OR_subjective_norm$work_environment <- relevel(factor(df_OR_subjective_norm$work_environment), ref = "freie Praxis")
df_OR_subjective_norm$employment_status <- relevel(factor(df_OR_subjective_norm$employment_status), ref = "Arbeitnehmerin/Arbeitnehmer")

df_OR_subjective_norm$colleagues5 <- as.numeric(df_OR_subjective_norm$colleagues5)

# K107_01 Likert-scale -> convert binary
df_OR_subjective_norm <- df_OR_subjective_norm %>%
  mutate(
    physician_collaboration_bin = factor(
      ifelse(as.numeric(physician_collaboration) >= 4, "Ja", "Nein"),
      levels = c("Nein", "Ja")
    )
  )
df_OR_subjective_norm$physician_collaboration_bin <- relevel(
  df_OR_subjective_norm$physician_collaboration_bin,
  ref = "Nein"
)

df_OR_subjective_norm$K108_01 <- as.numeric(df_OR_subjective_norm$K108_01)
df_OR_subjective_norm$patient_volume5 <- as.numeric(df_OR_subjective_norm$patient_volume5)

# OR, 95%-CI, p-Wert
univ_results_subjective_norm <- map_df(predictors, function(var) {
  
  f <- as.formula(paste("dv ~", var))
  m <- glm(f, data = df_OR_subjective_norm, family = binomial)
  
  broom::tidy(m) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96 * std.error),
      CI_high = exp(estimate + 1.96 * std.error),
      predictor = var
    ) %>%
    select(predictor, term, OR, CI_low, CI_high, p.value)
})

print(univ_results_subjective_norm, n = Inf)
summary(univ_results_subjective_norm$OR)

# Forest plot (sorted by predictors)
plot_df <- univ_results_subjective_norm %>%
  mutate(
    predictor = factor(predictor, levels = predictors),
    predictor_label = unname(predictor_labels[predictor]),
    level = str_remove(term, paste0("^", predictor)),
    level = ifelse(level == "" | is.na(level), "", level),
    row_label = ifelse(level == "",
                       predictor_label,
                       paste0(predictor_label, ": ", level))
  ) %>%
  arrange(predictor, term) %>%
  mutate(row_label = factor(row_label, levels = rev(unique(row_label))))

xmin <- 0.2; xmax <- 10
plot_df <- plot_df %>%
  mutate(
    OR_plot      = pmin(pmax(OR, xmin), xmax),
    CI_low_plot  = pmin(pmax(CI_low, xmin), xmax),
    CI_high_plot = pmin(pmax(CI_high, xmin), xmax), 
    row_label = fct_reorder(row_label, OR, .desc = TRUE)     
  )

p <- ggplot(plot_df, aes(x = OR_plot, y = row_label)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = CI_low_plot, xmax = CI_high_plot), height = 0.2) +
  geom_point(size = 1.8) +
  scale_x_log10(limits = c(xmin, xmax)) +
  labs(
    x = "Odds Ratio (log scale) [95% CI; capped for readability]",
    y = NULL,
    title = "Forest plot (sorted by pre-specified predictor order): Subjective norm (high vs low)"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

p

# Export
ggsave(file.path(wdir, "results", "forestplot_subjective_norm.png"),
       plot = p, width = 8, height = 10, dpi = 300)
ggsave(file.path(wdir, "results", "forestplot_subjective_norm.pdf"),
       plot = p, width = 8, height = 10)

##############################################################################
###################### EBDM (Resources+Attitudes+IBC+SN) #####################
### Self-reported integration of evidence-based decision-making principles ###

# Items to numeric, Dimension-Scores, EBDM_Score
df_processed <- df_processed %>%
  mutate(across(all_of(variable_groups$resources), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(variable_groups$attitudes), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(variable_groups$intention_behavioral_control), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(variable_groups$subjective_norm), ~ as.numeric(as.character(.)))) %>%
  mutate(
    resources_score = rowMeans(select(., all_of(variable_groups$resources)), na.rm = TRUE),
    attitudes_score = rowMeans(select(., all_of(variable_groups$attitudes)), na.rm = TRUE),
    intention_bc_score = rowMeans(select(., all_of(variable_groups$intention_behavioral_control)), na.rm = TRUE),
    subjective_norm_score = rowMeans(select(., all_of(variable_groups$subjective_norm)), na.rm = TRUE),
    ebdm_score = rowMeans(
      cbind(resources_score, attitudes_score, intention_bc_score, subjective_norm_score),
      na.rm = TRUE
    )
  )

# Dichotomizing: ≥4 = yes, <4 = no
df_OR_ebdm <- df_processed %>%
  mutate(
    dv = factor(ifelse(ebdm_score >= 4, "yes", "no"), levels = c("no", "yes"))
  ) %>%
  filter(!is.na(dv))

# Checks
summary(df_OR_ebdm$ebdm_score)
table(df_OR_ebdm$dv)

# define predictors
df_OR_ebdm$teaching_role <- relevel(factor(df_OR_ebdm$teaching_role), ref = "Nein")
df_OR_ebdm$managerial_position <- relevel(factor(df_OR_ebdm$managerial_position), ref = "Nein")
df_OR_ebdm$scientific_authorship <- relevel(factor(df_OR_ebdm$scientific_authorship), ref = "Nein")
df_OR_ebdm$parkinson_focus <- relevel(factor(df_OR_ebdm$parkinson_focus), ref = "Nein")
df_OR_ebdm$postgraduate_training <- relevel(factor(df_OR_ebdm$postgraduate_training), ref = "Nein")
df_OR_ebdm$education_trend <- as.numeric(df_OR_ebdm$education_trend)
df_OR_ebdm$workexp_trend <- as.numeric(df_OR_ebdm$workexp_trend)
df_OR_ebdm$workload_trend <- as.numeric(df_OR_ebdm$workload_trend)


df_OR_ebdm$gender <- relevel(factor(df_OR_ebdm$gender), ref = "männlich")

df_OR_ebdm$age10 <- as.numeric(df_OR_ebdm$age10)

df_OR_ebdm$work_environment <- relevel(factor(df_OR_ebdm$work_environment), ref = "freie Praxis")
df_OR_ebdm$employment_status <- relevel(factor(df_OR_ebdm$employment_status), ref = "Arbeitnehmerin/Arbeitnehmer")
df_OR_ebdm$colleagues5 <- as.numeric(df_OR_ebdm$colleagues5)

# K107_01 Likert-scale -> convert binary
df_OR_ebdm <- df_OR_ebdm %>%
  mutate(
    physician_collaboration_bin = factor(ifelse(as.numeric(physician_collaboration) >= 4, "Ja", "Nein"),
                         levels = c("Nein", "Ja"))
  )
df_OR_ebdm$physician_collaboration_bin <- relevel(df_OR_ebdm$physician_collaboration_bin, ref = "Nein")

df_OR_ebdm$K108_01 <- as.numeric(df_OR_ebdm$K108_01)
df_OR_ebdm$patient_volume5 <- as.numeric(df_OR_ebdm$patient_volume5)

# OR, 95%-CI, p-Wert
univ_results_ebdm <- map_df(predictors, function(var) {
  
  f <- as.formula(paste("dv ~", var))
  m <- glm(f, data = df_OR_ebdm, family = binomial)
  
  broom::tidy(m) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96 * std.error),
      CI_high = exp(estimate + 1.96 * std.error),
      predictor = var
    ) %>%
    select(predictor, term, OR, CI_low, CI_high, p.value)
})

print(univ_results_ebdm, n = Inf)
summary(univ_results_ebdm$OR)

# Forest plot
plot_df <- univ_results_ebdm %>%
  mutate(
    predictor = factor(predictor, levels = predictors),
    predictor_label = unname(predictor_labels[predictor]),
    level = str_remove(term, paste0("^", predictor)),
    level = ifelse(level == "" | is.na(level), "", level),
    row_label = ifelse(level == "", predictor_label, paste0(predictor_label, ": ", level))
  ) %>%
  arrange(predictor, term) %>%
  mutate(row_label = factor(row_label, levels = rev(unique(row_label))))

xmin <- 0.2; xmax <- 10
plot_df <- plot_df %>%
  mutate(
    OR_plot      = pmin(pmax(OR, xmin), xmax),
    CI_low_plot  = pmin(pmax(CI_low, xmin), xmax),
    CI_high_plot = pmin(pmax(CI_high, xmin), xmax), 
    row_label = fct_reorder(row_label, OR, .desc = TRUE)     
  )

p <- ggplot(plot_df, aes(x = OR_plot, y = row_label)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = CI_low_plot, xmax = CI_high_plot), height = 0.2) +
  geom_point(size = 1.8) +
  scale_x_log10(limits = c(xmin, xmax)) +
  labs(
    x = "Odds Ratio (log scale) [95% CI; capped for readability]",
    y = NULL,
    title = "Forest plot (sorted by pre-specified predictor order): Self-reported EBDM integration (high vs low)"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())
p

# Export
ggsave(file.path(wdir, "results", "forestplot_ebdm_integration.png"), plot = p, width = 8, height = 10, dpi = 300)
ggsave(file.path(wdir, "results", "forestplot_ebdm_integration.pdf"), plot = p, width = 8, height = 10)



############################################################
################## PATIENT CENTEREDNESS ####################
############################################################

variable_groups$patient_centeredness
df_processed <- df_processed %>%
  mutate(across(all_of(variable_groups$patient_centeredness), ~ as.numeric(as.character(.)))) %>%
  mutate(
    patient_centeredness_score = rowMeans(
      select(., all_of(variable_groups$patient_centeredness)),
      na.rm = TRUE
    )
  )

# Dichotomizing: ≥4 = yes, <4 = no
df_OR_patient_centeredness <- df_processed %>%
  mutate(
    dv = factor(ifelse(patient_centeredness_score >= 4, "yes", "no"),
                levels = c("no", "yes"))
  ) %>%
  filter(!is.na(dv))

# Checks
summary(df_OR_patient_centeredness$patient_centeredness_score)
table(df_OR_patient_centeredness$dv)

# define predictors
df_OR_patient_centeredness$teaching_role <- relevel(factor(df_OR_patient_centeredness$teaching_role), ref = "Nein")
df_OR_patient_centeredness$managerial_position <- relevel(factor(df_OR_patient_centeredness$managerial_position), ref = "Nein")
df_OR_patient_centeredness$scientific_authorship <- relevel(factor(df_OR_patient_centeredness$scientific_authorship), ref = "Nein")
df_OR_patient_centeredness$parkinson_focus <- relevel(factor(df_OR_patient_centeredness$parkinson_focus), ref = "Nein")
df_OR_patient_centeredness$postgraduate_training <- relevel(factor(df_OR_patient_centeredness$postgraduate_training), ref = "Nein")
df_OR_patient_centeredness$education_trend <- as.numeric(df_OR_patient_centeredness$education_trend)
df_OR_patient_centeredness$workexp_trend <- as.numeric(df_OR_patient_centeredness$workexp_trend)
df_OR_patient_centeredness$workload_trend <- as.numeric(df_OR_patient_centeredness$workload_trend)


df_OR_patient_centeredness$gender <- relevel(factor(df_OR_patient_centeredness$gender), ref = "männlich")

df_OR_patient_centeredness$age10 <- as.numeric(df_OR_patient_centeredness$age10)

df_OR_patient_centeredness$work_environment <- relevel(factor(df_OR_patient_centeredness$work_environment), ref = "freie Praxis")
df_OR_patient_centeredness$employment_status <- relevel(factor(df_OR_patient_centeredness$employment_status), ref = "Arbeitnehmerin/Arbeitnehmer")

df_OR_patient_centeredness$colleagues5 <- as.numeric(df_OR_patient_centeredness$colleagues5)

# K107_01 Likert-scale -> convert binary
df_OR_patient_centeredness <- df_OR_patient_centeredness %>%
  mutate(
    physician_collaboration_bin = factor(
      ifelse(as.numeric(physician_collaboration) >= 4, "Ja", "Nein"),
      levels = c("Nein", "Ja")
    )
  )
df_OR_patient_centeredness$physician_collaboration_bin <- relevel(
  df_OR_patient_centeredness$physician_collaboration_bin,
  ref = "Nein"
)

df_OR_patient_centeredness$K108_01 <- as.numeric(df_OR_patient_centeredness$K108_01)
df_OR_patient_centeredness$patient_volume5 <- as.numeric(df_OR_patient_centeredness$patient_volume5)

# OR, 95%-CI, p-Wert
univ_results_patient_centeredness <- map_df(predictors, function(var) {
  
  f <- as.formula(paste("dv ~", var))
  m <- glm(f, data = df_OR_patient_centeredness, family = binomial)
  
  broom::tidy(m) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96 * std.error),
      CI_high = exp(estimate + 1.96 * std.error),
      predictor = var
    ) %>%
    select(predictor, term, OR, CI_low, CI_high, p.value)
})

print(univ_results_patient_centeredness, n = Inf)
summary(univ_results_patient_centeredness$OR)

# Forest plot
plot_df <- univ_results_patient_centeredness %>%
  mutate(
    predictor = factor(predictor, levels = predictors),
    predictor_label = unname(predictor_labels[predictor]),
    level = str_remove(term, paste0("^", predictor)),
    level = ifelse(level == "" | is.na(level), "", level),
    row_label = ifelse(level == "",
                       predictor_label,
                       paste0(predictor_label, ": ", level))
  ) %>%
  arrange(predictor, term) %>%
  mutate(row_label = factor(row_label, levels = rev(unique(row_label))))

xmin <- 0.2; xmax <- 10
plot_df <- plot_df %>%
  mutate(
    OR_plot      = pmin(pmax(OR, xmin), xmax),
    CI_low_plot  = pmin(pmax(CI_low, xmin), xmax),
    CI_high_plot = pmin(pmax(CI_high, xmin), xmax), 
    row_label = fct_reorder(row_label, OR, .desc = TRUE)     
  )

p <- ggplot(plot_df, aes(x = OR_plot, y = row_label)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = CI_low_plot, xmax = CI_high_plot), height = 0.2) +
  geom_point(size = 1.8) +
  scale_x_log10(limits = c(xmin, xmax)) +
  labs(
    x = "Odds Ratio (log scale) [95% CI; capped for readability]",
    y = NULL,
    title = "Forest plot (sorted by pre-specified predictor order): Patient centeredness (high vs low)"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

p

# Export
ggsave(file.path(wdir, "results", "forestplot_patient_centeredness.png"),
       plot = p, width = 8, height = 10, dpi = 300)
ggsave(file.path(wdir, "results", "forestplot_patient_centeredness.pdf"),
       plot = p, width = 8, height = 10)

############################################################
##################### EBP_adherence #########################
############################################################

# 1) Items -> numeric, Dimensionen + Scores
df_processed <- df_processed %>%
  mutate(across(all_of(variable_groups$resources), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(variable_groups$attitudes), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(variable_groups$intention_behavioral_control), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(variable_groups$subjective_norm), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(variable_groups$patient_centeredness), ~ as.numeric(as.character(.)))) %>%
  mutate(
    resources_score = rowMeans(select(., all_of(variable_groups$resources)), na.rm = TRUE),
    attitudes_score = rowMeans(select(., all_of(variable_groups$attitudes)), na.rm = TRUE),
    intention_bc_score = rowMeans(select(., all_of(variable_groups$intention_behavioral_control)), na.rm = TRUE),
    subjective_norm_score = rowMeans(select(., all_of(variable_groups$subjective_norm)), na.rm = TRUE),
    
    # (5) Integration: MW aus 4 Dimensionen
    ebdm_integration_score = rowMeans(
      cbind(resources_score, attitudes_score, intention_bc_score, subjective_norm_score),
      na.rm = TRUE
    ),
    
    # (6) Patientenzentriertheit
    patient_centeredness_score = rowMeans(select(., all_of(variable_groups$patient_centeredness)), na.rm = TRUE),
    
    # (7) EBP_adherence = MW aus (5) + (6)  -> bleibt 1–5 Skala
    EBP_adherence_score = rowMeans(
      cbind(ebdm_integration_score, patient_centeredness_score),
      na.rm = TRUE
    )
  )

# 2) Dichotomizing: ≥4 = yes, <4 = no
df_OR_EBP_adherence <- df_processed %>%
  mutate(
    dv = factor(ifelse(EBP_adherence_score >= 4, "yes", "no"), levels = c("no", "yes"))
  ) %>%
  filter(!is.na(dv))

# Checks
summary(df_OR_EBP_adherence$EBP_adherence_score)
table(df_OR_EBP_adherence$dv)

# 3) define predictors (IDENTISCH)
df_OR_EBP_adherence$teaching_role <- relevel(factor(df_OR_EBP_adherence$teaching_role), ref = "Nein")
df_OR_EBP_adherence$managerial_position <- relevel(factor(df_OR_EBP_adherence$managerial_position), ref = "Nein")
df_OR_EBP_adherence$scientific_authorship <- relevel(factor(df_OR_EBP_adherence$scientific_authorship), ref = "Nein")
df_OR_EBP_adherence$parkinson_focus <- relevel(factor(df_OR_EBP_adherence$parkinson_focus), ref = "Nein")
df_OR_EBP_adherence$postgraduate_training <- relevel(factor(df_OR_EBP_adherence$postgraduate_training), ref = "Nein")
df_OR_EBP_adherence$education_trend <- as.numeric(df_OR_EBP_adherence$education_trend)
df_OR_EBP_adherence$workexp_trend <- as.numeric(df_OR_EBP_adherence$workexp_trend)
df_OR_EBP_adherence$workload_trend <- as.numeric(df_OR_EBP_adherence$workload_trend)


df_OR_EBP_adherence$gender <- relevel(factor(df_OR_EBP_adherence$gender), ref = "männlich")

df_OR_EBP_adherence$age10 <- as.numeric(df_OR_EBP_adherence$age10)

df_OR_EBP_adherence$work_environment <- relevel(factor(df_OR_EBP_adherence$work_environment), ref = "freie Praxis")
df_OR_EBP_adherence$employment_status <- relevel(factor(df_OR_EBP_adherence$employment_status), ref = "Arbeitnehmerin/Arbeitnehmer")

df_OR_EBP_adherence$colleagues5 <- as.numeric(df_OR_EBP_adherence$colleagues5)

df_OR_EBP_adherence <- df_OR_EBP_adherence %>%
  mutate(
    physician_collaboration_bin = factor(ifelse(as.numeric(physician_collaboration) >= 4, "Ja", "Nein"),
                         levels = c("Nein", "Ja"))
  )
df_OR_EBP_adherence$physician_collaboration_bin <- relevel(df_OR_EBP_adherence$physician_collaboration_bin, ref = "Nein")

df_OR_EBP_adherence$K108_01 <- as.numeric(df_OR_EBP_adherence$K108_01)
df_OR_EBP_adherence$patient_volume5 <- as.numeric(df_OR_EBP_adherence$patient_volume5)


# 4) Univariable ORs
univ_results_EBP_adherence <- map_df(predictors, function(var) {
  
  f <- as.formula(paste("dv ~", var))
  m <- glm(f, data = df_OR_EBP_adherence, family = binomial)
  
  broom::tidy(m) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96 * std.error),
      CI_high = exp(estimate + 1.96 * std.error),
      predictor = var
    ) %>%
    select(predictor, term, OR, CI_low, CI_high, p.value)
})

print(univ_results_EBP_adherence, n = Inf)
summary(univ_results_EBP_adherence$OR)

# 5) Forest plot (sorted by predictors)
plot_df <- univ_results_EBP_adherence %>%
  mutate(
    predictor = factor(predictor, levels = predictors),
    predictor_label = unname(predictor_labels[predictor]),
    level = str_remove(term, paste0("^", predictor)),
    level = ifelse(level == "" | is.na(level), "", level),
    row_label = ifelse(level == "", predictor_label, paste0(predictor_label, ": ", level))
  ) %>%
  arrange(predictor, term) %>%
  mutate(row_label = factor(row_label, levels = rev(unique(row_label))))

xmin <- 0.2; xmax <- 10
plot_df <- plot_df %>%
  mutate(
    OR_plot      = pmin(pmax(OR, xmin), xmax),
    CI_low_plot  = pmin(pmax(CI_low, xmin), xmax),
    CI_high_plot = pmin(pmax(CI_high, xmin), xmax), 
    row_label = fct_reorder(row_label, OR, .desc = TRUE)     
  )

p <- ggplot(plot_df, aes(x = OR_plot, y = row_label)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = CI_low_plot, xmax = CI_high_plot), height = 0.2) +
  geom_point(size = 1.8) +
  scale_x_log10(limits = c(xmin, xmax)) +
  labs(
    x = "Odds Ratio (log scale) [95% CI; capped for readability]",
    y = NULL,
    title = "Forest plot (sorted by pre-specified predictor order): EBP_adherence (high vs low)"
  ) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

p

# Export
ggsave(file.path(wdir, "results", "forestplot_EBP_adherence.png"), plot = p, width = 8, height = 10, dpi = 300)
ggsave(file.path(wdir, "results", "forestplot_EBP_adherence.pdf"), plot = p, width = 8, height = 10)
