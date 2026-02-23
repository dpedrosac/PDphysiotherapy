# -------------------------------------------------------------------------
# Title: Exploratory Factor Analysis (EFA) of PhysioPD EBP-implementation dimensions and predictors
#
# Description:
#   Performs exploratory factor analysis (EFA) on PhysioPD EBP-implementation
#   dimensions and predictors. Includes correlation screening, factor extraction
#   (oblique rotation), iterative variable filtering, reliability analysis, and
#   export of results and figures to the results/ directory.
#
# Authors:
#   Christian Isselstein
#   David Pedrosa
#
# Sources:
#   https://rpubs.com/pjmurphy/758265
#   Script "06_exploratory_factor_analysis.R" (Moormann & Pedrosa)
#   https://stackoverflow.com/questions/63856304/create-data-frame-from-efa-output-in-r
#   https://rpubs.com/pjmurphy/758265
#
# Last updated:
#   
#
# R version:
#   >= 4.3.1
#
# ---------------------------------------------------------------
# Plan - two EFAs: EBDM & predictors
# EBDM
#  1) Define variables (efa_vars_ebdm, efa_config_ebdm, label_map_ebdm)
#  2) Missingness diagnostics (VIM::aggr + counts)
#  3) Correlation screening / visualization (heatmap or pairs; threshold |r| > 0.29)
#  4) r-screening: exclude variables without any correlation |r| > 0.29 (complete-case corr)
#  5) KMO (MSA > 0.50) + Bartlett’s test (factorability)
#  6) Scree plot / eigenvalues (Kaiser criterion)
#  7) EFA extraction: 1-factor model using factanal() (ML; rotation irrelevant for 1 factor) + regression scores
#  8) Loading filter check (|loading| >= 0.40; no exclusions expected)
#  9) Finalize EBDM model (retained variables + final loadings)
# 10) Reliability: Cronbach’s alpha across the 4 EBDM component scores
# 11) Export: loadings/uniqueness table to results/
# 12) Use factor scores (scores_ebdm) for subsequent analyses
#
# Predictors
#  1) Define variables (efa_vars_pred, efa_config_pred, label_map_pred)
#  2) Missingness diagnostics (VIM::aggr + counts)
#  3) Correlation screening / visualization (heatmap or pairs; threshold |r| > 0.29)
#  4) r-screening: exclude predictors without any correlation |r| > 0.29 (complete-case corr)
#  5) KMO (overall MSA > 0.50) + drop items with individual MSA < 0.50 + Bartlett’s test
#  6) Scree plot / eigenvalues (Kaiser criterion)
#  7) Parallel analysis (psych::fa.parallel, fm="minres", fa="fa") to suggest number of factors
#  8) Exploratory solutions: 2-factor vs 3-factor comparison (psych::fa)
#  9) Oblique rotation applied (oblimin); inspect factor correlation (Phi)
# 10) Apply loading filter (|loading| >= 0.40) and remove weak variables
# 11) Refit final predictor model (psych::fa with oblimin rotation)
# 12) Assign factor labels:
#       - Factor 1: Experience (Age + Work experience)
#       - Factor 2: Professional collaboration/context (Colleagues + Physician collaboration)
# 13) Reliability: Cronbach’s alpha only if >= 2 variables per factor; interpret 2-item factors cautiously
# 14) Export final predictor results (loadings + Phi matrix) to results/
## ==============================================================
# Exploratory factor analysis (EFA)
# sources: https://rpubs.com/pjmurphy/758265 and R-script "06_exploratory_factor_analysis.R" by Leonie Moormann and David Pedrosa
#
# Visualization controls
plot_corr <- TRUE             # master switch for any correlation visualization
corr_plot_type <- "heatmap"   # "pairs" or "heatmap"
corr_threshold <- 0.29        # threshold used for filtering correlations
label_threshold <- 0.30       # label threshold (only used in pair plot)

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

# 1 - Evidence-based decision-making (EBDM) (Resources+Attitudes+IBC+SN)
efa_config_ebdm <- list(
  n_factors = 1,
  corr_threshold = 0.29,
  loading_threshold = 0.40,
  factor_assignment_threshold = 0.30
)

efa_vars_ebdm <- c(
  "resources_score",
  "attitudes_score",
  "intention_bc_score",        
  "subjective_norm_score"
)

label_map_ebdm <- c(
  resources_score = "Resources",
  attitudes_score = "Attitudes",
  intention_bc_score = "Intention & Behavioral Control",
  subjective_norm_score = "Subjective Norm"
)


# 2 - Predictors
efa_config_pred <- list(
  n_factors = 4,               # see Screeplot
  corr_threshold = 0.29,
  loading_threshold = 0.40,
  factor_assignment_threshold = 0.30
)

# nominal variables excluded out of EFA (employment_status, work_environment).
efa_vars_pred <- c(
  "age10",  
  "workexp_trend",
  "workload_trend",
  "education_trend",
  "teaching_role",
  "managerial_position",
  "scientific_authorship",
  "postgraduate_training",
  "colleagues5",
  "patient_volume5",
  "parkinson_focus",
  "physician_collaboration_bin"
  )


label_map_pred <- c(
  age10 = "Age (per 10 years)",
  workexp_trend = "Work experience (trend)",
  workload_trend = "Workload (trend)",
  education_trend = "Education (trend)",
  colleagues5 = "Colleagues (per 5)",
  patient_volume5 = "Patient volume (per 5)",
  teaching_role = "Teaching role",
  managerial_position = "Managerial position",
  scientific_authorship = "Scientific authorship",
  postgraduate_training = "Postgraduate training",
  parkinson_focus = "Parkinson focus",
  physician_collaboration_bin = "Physician collaboration"
)

df_EFA_pred <- df_processed %>%
  dplyr::select(dplyr::all_of(efa_vars_pred))

df_EFA_pred_num <- df_EFA_pred %>%
  dplyr::mutate(
    teaching_role          = as.numeric(as.character(teaching_role) == "Ja"),
    managerial_position    = as.numeric(as.character(managerial_position) == "Ja"),
    scientific_authorship  = as.numeric(as.character(scientific_authorship) == "Ja"),
    postgraduate_training  = as.numeric(as.character(postgraduate_training) == "Ja"),
    parkinson_focus        = as.numeric(as.character(parkinson_focus) == "Ja")
  )

# ---- Helpers -------------------------------------------------------------

make_cor_matrix <- function(df, threshold, use = "complete.obs") {
  m <- round(stats::cor(df, use = use), 2)
  m[abs(m) <= threshold] <- NA
  diag(m) <- NA
  m
}

cor_to_long_unique <- function(cor_matrix) {
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
  as.data.frame(as.table(cor_matrix)) |>
    dplyr::filter(!is.na(Freq)) |>
    dplyr::transmute(
      var1 = as.character(Var1),
      var2 = as.character(Var2),
      correlation = as.numeric(Freq)
    )
}

plot_cor_pairs <- function(cor_long, outfile, label_threshold = 0.30) {
  cor_long <- cor_long |>
    dplyr::mutate(
      pair_sorted = paste(var1, var2, sep = " + "),
      label = dplyr::if_else(abs(correlation) >= label_threshold,
                             sprintf("%.2f", correlation),
                             NA_character_)
    ) |>
    dplyr::arrange(dplyr::desc(correlation))
  
  grDevices::pdf(outfile, width = 13, height = 8.5)
  p <- ggplot2::ggplot(cor_long, ggplot2::aes(x = correlation,
                                              y = stats::reorder(pair_sorted, correlation))) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_segment(ggplot2::aes(xend = 0, yend = pair_sorted), linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label), hjust = -0.4, size = 3, na.rm = TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Correlation",
      y = NULL,
      title = "Correlation plot (filtered)",
      subtitle = "Unique pairs only",
      caption = "Pairs with |r| below the threshold were omitted."
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 11),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, size = 10, face = "italic")
    )
  print(p)
  grDevices::dev.off()
}

plot_cor_heatmap <- function(cor_matrix, out_base,
                             title = "Correlation heatmap (filtered)",
                             formats = c("pdf", "png")) {
  # to keep only the upper triangle, to avoid duplicate information
  cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
  
  cor_df <- as.data.frame(as.table(cor_matrix)) |>
    dplyr::transmute(
      var_x = factor(Var2, levels = colnames(cor_matrix)),
      var_y = factor(Var1, levels = rev(rownames(cor_matrix))),
      correlation = as.numeric(Freq)
    ) |>
    dplyr::filter(!is.na(correlation))
  
  p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = var_x, y = var_y, fill = correlation)) +
    ggplot2::geom_tile(color = NA, linewidth = 0) +
    ggplot2::scale_fill_gradient2(
      low = "#4575B4", mid = "white", high = "#D73027",
      midpoint = 0, na.value = "transparent", name = "r"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", correlation)), size = 4) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 16, face = "bold")
    ) +
    ggplot2::labs(title = title)
  
  for (fmt in formats) {
    out_file <- paste0(out_base, ".", fmt)
    
    if (fmt == "pdf") {
      grDevices::pdf(out_file, width = 15, height = 11)
      print(p)
      grDevices::dev.off()
    } else if (fmt == "png") {
      grDevices::png(filename = out_file, width = 2600, height = 1800, res = 300)
      print(p)
      grDevices::dev.off()
    } else {
      warning(sprintf("Unsupported format '%s' (use 'pdf' or 'png').", fmt))
    }
  }
  
  invisible(TRUE)
}

# ---- Data prep: EBDM -----------------------------------------------------

df_EFA_ebdm <- df_processed %>%
  dplyr::select(dplyr::all_of(efa_vars_ebdm))

flag_check <- TRUE   # like a switch to "hide" intermediate tests in the large project; maybe switch to FALSE later
# Missingsness overview EBDM
if (flag_check) {                     
  print(colSums(is.na(df_EFA_ebdm)))

  grDevices::pdf(
    file.path(wdir, "results", "missingness_efa_ebdm.pdf"),
    width = 11,
    height = 8.5
  )
  
  VIM::aggr(
    df_EFA_ebdm,
    col = c("navyblue", "red"),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(df_EFA_ebdm),
    cex.axis = 0.8,
    gap = 3,
    ylab = c("Missing data", "Pattern")
  )
  
  grDevices::dev.off()
}

# ---- Data prep: Predictors ----------------------------------------------

# Missingsness overview predictors
if (flag_check) {
  print(colSums(is.na(df_EFA_pred)))
  
  grDevices::pdf(file.path(wdir, "results", "missingness_efa_predictors.pdf"),
                 width = 11, height = 8.5)
  
  VIM::aggr(
    df_EFA_pred,
    col = c("navyblue", "red"),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(df_EFA_pred),
    cex.axis = 0.7,
    gap = 3,
    ylab = c("Missing data", "Pattern")
  )
  
  grDevices::dev.off()
}
sum(stats::complete.cases(df_EFA_pred))

# -------------------------------------------------------------------------
# Correlation
# -------------------------------------------------------------------------
# Correlations (filtered) - EBDM

cor_matrix_ebdm <- make_cor_matrix(
  df_EFA_ebdm,
  threshold = efa_config_ebdm$corr_threshold,
  use = "complete.obs"
)
results.cor_matrix_ebdm <- cor_matrix_ebdm

if (plot_corr) {
  out_dir <- file.path(wdir, "results")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Apply human-readable labels to matrix dimnames
  dimnames(cor_matrix_ebdm) <- list(
    dplyr::recode(rownames(cor_matrix_ebdm), !!!label_map_ebdm),
    dplyr::recode(colnames(cor_matrix_ebdm), !!!label_map_ebdm)
  )
  
  if (corr_plot_type == "pairs") {
    cor_long_ebdm <- cor_to_long_unique(cor_matrix_ebdm)
    outfile <- file.path(out_dir, "fig_corrEFA_ebdm_pairs.pdf")
    plot_cor_pairs(cor_long_ebdm, outfile, label_threshold = label_threshold)
    
  } else if (corr_plot_type == "heatmap") {
    plot_cor_heatmap(
      cor_matrix_ebdm,
      out_base = file.path(out_dir, "fig_corrEFA_ebdm_heatmap"),
      formats = c("pdf", "png"),
      title = "EBDM: Correlation heatmap (filtered)"
    )
    
  } else {
    warning(sprintf("Unknown corr_plot_type='%s'. Use 'pairs' or 'heatmap'.", corr_plot_type))
  }
}

# -------------------------------------------------------------------------
# Correlations (filtered) - Predictors

cor_matrix_pred <- make_cor_matrix(
  df_EFA_pred_num,
  threshold = efa_config_pred$corr_threshold,
  use = "pairwise.complete.obs"
)
results.cor_matrix_pred <- cor_matrix_pred

if (plot_corr) {
  out_dir <- file.path(wdir, "results")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Apply human-readable labels to matrix dimnames
  dimnames(cor_matrix_pred) <- list(
    dplyr::recode(rownames(cor_matrix_pred), !!!label_map_pred),
    dplyr::recode(colnames(cor_matrix_pred), !!!label_map_pred)
  )
  
  if (corr_plot_type == "pairs") {
    cor_long_pred <- cor_to_long_unique(cor_matrix_pred)
    outfile <- file.path(out_dir, "fig_corrEFA_pred_pairs.pdf")
    plot_cor_pairs(cor_long_pred, outfile, label_threshold = label_threshold)
    
  } else if (corr_plot_type == "heatmap") {
    plot_cor_heatmap(
      cor_matrix_pred,
      out_base = file.path(out_dir, "fig_corrEFA_pred_heatmap"),
      formats = c("pdf", "png"),
      title = "Predictors: Correlation heatmap (filtered)"
    )
    
  } else {
    warning(sprintf("Unknown corr_plot_type='%s'. Use 'pairs' or 'heatmap'.", corr_plot_type))
  }
}

# Excluding variables, that don't have r > 0,29
# if Item has not enough correlation to the others, it can't have much input for the common factor
# where should I put the threshold 

# EBDM
cor_matrix_ebdm <- cor(df_EFA_ebdm, use = "complete.obs")

mask_ebdm <- abs(cor_matrix_ebdm) > 0.29
diag(mask_ebdm) <- FALSE

# Identify and exclude variables with no correlations > 0.29
exclude_vars_ebdm <- colnames(df_EFA_ebdm)[colSums(mask_ebdm) == 0]
data_EFA_ebdm <- df_EFA_ebdm[, !(colnames(df_EFA_ebdm) %in% exclude_vars_ebdm)]
exclude_vars_ebdm

# Predictors
cor_matrix_pred <- cor(df_EFA_pred_num, use = "complete.obs")
mask_pred <- abs(cor_matrix_pred) > 0.29
diag(mask_pred) <- FALSE
# Identify and exclude variables with no correlations > 0.29
exclude_vars_pred <- colnames(df_EFA_pred_num)[colSums(mask_pred) == 0]
data_EFA_pred <- df_EFA_pred_num[, !(colnames(df_EFA_pred_num) %in% exclude_vars_pred)]
exclude_vars_pred
  # Extracted predictors
     # teaching_role
     # postgraduate_training
     # patient_volume5
     # parkinson_focus

# KMO/Bartlett - EBDM
results.kmo_ebdm <- psych::KMO(data_EFA_ebdm)
data_EFA_ebdm <- data_EFA_ebdm[, results.kmo_ebdm$MSAi > 0.50]
results.kmo_ebdm
results.bartlett_ebdm <- psych::cortest.bartlett(
  cor(data_EFA_ebdm, use = "complete.obs"),
  n = nrow(na.omit(data_EFA_ebdm))
)
results.bartlett_ebdm
# KMO/Bartlett - predictors
results.kmo_pred <- psych::KMO(data_EFA_pred)
data_EFA_pred <- data_EFA_pred[, results.kmo_pred$MSAi > 0.50]
results.kmo_pred
results.bartlett_pred <- psych::cortest.bartlett(
  cor(data_EFA_pred, use = "complete.obs"),
  n = nrow(na.omit(data_EFA_pred))
)
results.bartlett_pred
# r-Screening extracts with >0.29: teaching_role, postgraduate_training, patient_volume5, parkinson_focus
# MSA extracts with > 0.50: workload_trend & scientific_authorship -  MSA each 0.47)
# 6 variables for predictors remaining --> beginning = 4 factors in scree plot; after excluding = 2 factors
# ------------------------------------------------------------
# Scree Plot – EBDM
pdf(file.path(wdir, "results", "suppl_fig_scree_EFA_ebdm.pdf"),
    width = 11, height = 8.5)
results.eigenvalues_ebdm <- eigen(
  cor(data_EFA_ebdm, use = "complete.obs")
)$values
plot(results.eigenvalues_ebdm,
     pch = 19,
     type = "b",
     ylab = "Eigenvalue",
     xlab = "Factor number",
     main = "Scree Plot – EBDM")
abline(h = 1, col = "red")   # Kaiser criterion
dev.off()
results.eigenvalues_ebdm

# ------------------------------------------------------------
# Scree Plot – Predictors
pdf(file.path(wdir, "results", "suppl_fig_scree_EFA_predictors.pdf"),
    width = 11, height = 8.5)
results.eigenvalues_pred <- eigen(
  cor(data_EFA_pred, use = "complete.obs")
)$values
plot(results.eigenvalues_pred,
     pch = 19,
     type = "b",
     ylab = "Eigenvalue",
     xlab = "Factor number",
     main = "Scree Plot – Predictors")

abline(h = 1, col = "red")
dev.off()
results.eigenvalues_pred
# Parallel analysis for predictors
set.seed(123)  
pdf(file.path(wdir, "results", "suppl_fig_parallel_analysis_predictors.pdf"),
    width = 11, height = 8.5)
fa_parallel_pred <- psych::fa.parallel(
  data_EFA_pred,
  fm = "minres",               # Parallel analysis suggests 2 factors = 2
  fa = "fa"
)
dev.off()

fa(data_EFA_pred, 
   nfactors = 2,         # 2 Factor better than 3 ->
   fm = "minres",
   rotate = "oblimin")
fa(data_EFA_pred, 
   nfactors = 3,         # -> because one factor is only one predictor: education_trend
   fm = "minres",
   rotate = "oblimin")

# ------------------------------------------------------------
# Extract factors with oblique rotation
# EBDM: factanal (ML) works fine with 4 variables to 1 factor
# Predictors: psych::fa because factanal cannot estimate 2 factors with only 4 variables

# 1) EBDM (factanal + PROMAX)

data_EFA_ebdm_cc <- data_EFA_ebdm %>%      # factanal needs complete cases
  dplyr::filter(stats::complete.cases(.))
fit_oblique_ebdm <- factanal(
  data_EFA_ebdm_cc,
  factors  = efa_config_ebdm$n_factors,    # n = 1
  rotation = "promax",
  scores   = "regression"
)
print(fit_oblique_ebdm, digits = 2, sort = TRUE)
# Factor scores (per person)
scores_ebdm <- as.data.frame(fit_oblique_ebdm$scores)
colnames(scores_ebdm) <- paste0("EBDM_factor", seq_len(ncol(scores_ebdm)))


# 2) Predictors (psych::fa + OBLIMIN = oblique)

data_EFA_pred_cc <- data_EFA_pred %>%
  dplyr::filter(stats::complete.cases(.))
fit_pred_fa <- psych::fa(
  data_EFA_pred_cc,
  nfactors = 2,
  fm = "minres",
  rotate = "oblimin",
  scores = "regression"
)
print(fit_pred_fa$loadings, cutoff = 0.30, sort = TRUE)
# Factor correlations (cause of oblique rotation)
print(fit_pred_fa$Phi)
# Factor scores (per person)
scores_pred <- as.data.frame(fit_pred_fa$scores)
# If scores were not stored (depending on psych version), compute explicitly:
if (is.null(scores_pred) || nrow(scores_pred) == 0) {
  scores_pred <- as.data.frame(
    psych::factor.scores(data_EFA_pred_cc, fit_pred_fa, method = "regression")$scores
  )
}
colnames(scores_pred) <- paste0("PRED_factor", seq_len(ncol(scores_pred)))
# optional: keep naming consistent with your other objects
fit_oblique_pred <- fit_pred_fa

# ------------------------------------------------------------------
# Check for factor loading < 0.4 and exclude variables with factor loading < 0.4
# and Perform EFA again
#
# 1 - EBDM (factanal)
# Extract loadings
loadings_ebdm <- as.matrix(fit_oblique_ebdm$loadings)
threshold_ebdm <- efa_config_ebdm$loading_threshold
# Keep items with any loading >= threshold (see on top)
items_to_keep_ebdm <- apply(loadings_ebdm, 1, function(x) any(abs(x) >= threshold_ebdm))
excluded_items_ebdm <- rownames(loadings_ebdm)[!items_to_keep_ebdm]
cat("EBDM excluded items (|loading| < threshold):\n")
print(excluded_items_ebdm)
# Filter dataset
filtered_data_EFA_ebdm <- data_EFA_ebdm_cc[, items_to_keep_ebdm, drop = FALSE]
# refit (keeps workflow consistent, even if nothing was excluded)
fit_oblique_ebdm_filtered <- factanal(
  filtered_data_EFA_ebdm,
  factors  = efa_config_ebdm$n_factors,
  rotation = "promax",
  scores   = "regression"
)
print(fit_oblique_ebdm_filtered, digits = 2, sort = TRUE)
results.fit_oblique_ebdm <- fit_oblique_ebdm_filtered

# 2 - Predictors (psych::fa)
# Extract loadings
loadings_pred <- as.matrix(fit_oblique_pred$loadings)  
threshold_pred <- efa_config_pred$loading_threshold
items_to_keep_pred <- apply(loadings_pred, 1, function(x) any(abs(x) >= threshold_pred))
excluded_items_pred <- rownames(loadings_pred)[!items_to_keep_pred]
cat("\nPredictors excluded items (|loading| < threshold):\n")
print(excluded_items_pred)
# Filter dataset
filtered_data_EFA_pred <- data_EFA_pred_cc[, items_to_keep_pred, drop = FALSE]
# refit final predictor model
fit_oblique_pred_filtered <- psych::fa(
  filtered_data_EFA_pred,
  nfactors = 2,
  fm = "minres",
  rotate = "oblimin",
  scores = "regression"
)
print(fit_oblique_pred_filtered$loadings, cutoff = 0.30, sort = TRUE)
cat("\nPredictors factor correlations (Phi):\n")
print(fit_oblique_pred_filtered$Phi)
results.fit_oblique_pred <- fit_oblique_pred_filtered

# -----------------------------------------------------------
# Factor naming and variable assignment
#
# EBDM = 1 factor
ebdm_factor_name <- "EBDM"
# Variables assigned to factor (threshold = factor_assignment_threshold)
loadings_ebdm_final <- as.matrix(results.fit_oblique_ebdm$loadings)
ebdm_factor_variables <- list(
  EBDM = rownames(loadings_ebdm_final)[abs(loadings_ebdm_final[, 1]) >= efa_config_ebdm$factor_assignment_threshold]
)
cat("EBDM factor name:\n")
print(ebdm_factor_name)
cat("\nEBDM factor variables:\n")
print(ebdm_factor_variables)

# Predictors = 2 factors
# Extract loadings as numeric matrix
pred_loadings <- as.matrix(results.fit_oblique_pred$loadings)
# rename factors to meaningful labels
pred_factor_names <- c("Experience", "Professional_collaboration")
colnames(pred_loadings) <- pred_factor_names
# Assign variables to factors based on loading threshold
pred_factor_variables <- list()
for (j in seq_len(ncol(pred_loadings))) {
  pred_factor_variables[[colnames(pred_loadings)[j]]] <-
    rownames(pred_loadings)[abs(pred_loadings[, j]) >= efa_config_pred$factor_assignment_threshold]
}
cat("\nPredictor factor names:\n")
print(pred_factor_names)
cat("\nPredictor factor variables (|loading| >= assignment threshold):\n")
print(pred_factor_variables)

# -------------------------------------------------------
# Cronbach's alpha (reliability) 

# EBDM
cat("\nCronbach's Alpha – EBDM:\n")
ebdm_vars <- ebdm_factor_variables$EBDM
existing_ebdm_vars <- ebdm_vars[ebdm_vars %in% colnames(filtered_data_EFA_ebdm)]
if (length(existing_ebdm_vars) > 1) {
  results.alpha_ebdm <- psych::alpha(filtered_data_EFA_ebdm[, existing_ebdm_vars])
  print(results.alpha_ebdm$total$raw_alpha)
} else {
  cat("Not enough variables to compute Cronbach's Alpha for EBDM\n")
}

# predictors
cat("\nCronbach's Alpha – Predictors:\n")
results.alpha_pred <- list()
for (factor_name in names(pred_factor_variables)) {
  
  vars <- pred_factor_variables[[factor_name]]
  existing_vars <- vars[vars %in% colnames(filtered_data_EFA_pred)]
  
  if (length(existing_vars) > 1) {
    alpha_result <- psych::alpha(filtered_data_EFA_pred[, existing_vars])
    results.alpha_pred[[factor_name]] <- alpha_result
    cat(paste("Cronbach's Alpha for", factor_name, ":", alpha_result$total$raw_alpha, "\n"))
  } else {
    cat(paste("Not enough variables to compute Cronbach's Alpha for", factor_name, "\n"))
  }
}

# ---------------------------------------------------------------
# Exporting final EFA loadings table (Excel)
out_dir <- file.path(wdir, "results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
# EBDM (factanal) table
fa_table_factanal <- function(x, cut = 0.30) {
  L <- as.data.frame(unclass(x$loadings))
  L <- round(L, 3)
  L[abs(L) < cut] <- NA
  
  uniq <- round(x$uniquenesses, 3)
  comm <- round(1 - x$uniquenesses, 3)
  
  out <- cbind(
    item = rownames(L),
    L,
    commonality = comm,
    uniqueness  = uniq
  )
  out <- as.data.frame(out)
  out[is.na(out)] <- ""   
  out
}

# Predictors (psych::fa) table
fa_table_psych <- function(x, cut = 0.30) {
  L <- as.data.frame(unclass(x$loadings))
  L <- round(L, 3)
  L[abs(L) < cut] <- NA
  
  add_info <- data.frame(
    commonality = round(x$communality, 3),
    uniqueness  = round(x$uniquenesses, 3),
    complexity  = round(x$complexity, 3)
  )
  out <- cbind(item = rownames(L), L, add_info[rownames(L), , drop = FALSE])
  out <- as.data.frame(out)
  out[is.na(out)] <- ""
  out
}

# final tables
efa_table_ebdm_final <- fa_table_factanal(results.fit_oblique_ebdm, cut = 0.30)
efa_table_pred_final <- fa_table_psych(results.fit_oblique_pred,  cut = 0.30)

# EBDM
wb_ebdm <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_ebdm, "EBDM_final")
openxlsx::writeData(wb_ebdm, "EBDM_final", efa_table_ebdm_final)
openxlsx::setColWidths(wb_ebdm, "EBDM_final", cols = 1:ncol(efa_table_ebdm_final), widths = "auto")
openxlsx::saveWorkbook(
  wb_ebdm,
  file = file.path(out_dir, "efa_table_EBDM_final.xlsx"),
  overwrite = TRUE
)

# Predictors
wb_pred <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_pred, "Predictors_final")
openxlsx::writeData(wb_pred, "Predictors_final", efa_table_pred_final)
openxlsx::setColWidths(wb_pred, "Predictors_final", cols = 1:ncol(efa_table_pred_final), widths = "auto")
openxlsx::saveWorkbook(
  wb_pred,
  file = file.path(out_dir, "efa_table_Predictors_final.xlsx"),
  overwrite = TRUE
)

cat("\nSaved:\n",
    "- efa_table_EBDM_final.xlsx\n",
    "- efa_table_Predictors_final.xlsx\n",
    "in: ", out_dir, "\n", sep = "")

# ---------------------------------------------------------------
# Export factor correlations (Phi) – Predictors only (oblique rotation)
# Predictors: factor correlations exist (Phi)
if (!is.null(results.fit_oblique_pred$Phi)) {
  phi_pred <- as.data.frame(round(results.fit_oblique_pred$Phi, 3))
  phi_pred <- cbind(Factor = rownames(phi_pred), phi_pred)
  
  wb_phi <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_phi, "Predictors_Phi")
  openxlsx::writeData(wb_phi, "Predictors_Phi", phi_pred)
  openxlsx::setColWidths(wb_phi, "Predictors_Phi", cols = 1:ncol(phi_pred), widths = "auto")
  
  openxlsx::saveWorkbook(
    wb_phi,
    file = file.path(out_dir, "efa_factor_correlations_Predictors_final.xlsx"),
    overwrite = TRUE
  )
  
  cat("\nSaved:\n- efa_factor_correlations_Predictors_final.xlsx\nin: ", out_dir, "\n", sep = "")
} else {
  cat("\nNo Phi matrix found for predictors (rotation not oblique or object missing Phi).\n")
}

# EBDM: only 1 factor -> no factor correlation matrix to export
cat("\nEBDM has 1 factor -> no factor correlation matrix (Phi) to export.\n")
