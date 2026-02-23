#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Physiotherapy in Parkinson's Disease (PD)
#
# Description:
#   Wrapper script to run all preprocessing and necessary analyses fpr the 
#   entire project
#
# Authors:
#   Christian Isselstein
#   David Pedrosa
#
# Last updated:
#   2026-02-11
#
# R version:
#   >= 4.2.0
#
# Notes:
#   - This script is intended to provide the necessary packages and tidy up data.
#   - Major methodological changes are tracked via git commit history.
#
# -------------------------------------------------------------------------

#----- Configuration  -----------------------------------------------------

flag_check <- FALSE

username <- Sys.getenv("USERNAME")
if (username == "dpedr") {
  project_root <- "D:/PDphysiotherapy"
  data_dir <- file.path(project_root, "data")
  
} else if (username == "david") {
  project_root <- "/media/storage/PDphysiotherapy"
  data_dir <- file.path(project_root, "data")
  
} else if (username == "chris") {
  project_root <- "C:/Users/chris/Desktop/PDphysiotherapy"
  data_dir <- file.path(project_root, "data")
  
} else {
  cat("Username unknown\n")
}

setwd(project_root)
results_dir <- file.path(project_root, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# -------------------------------------------------------------------------
## Part 1: Load required packages and load raw data into workspace 
# -------------------------------------------------------------------------

source("01_packages.v1.0.R")

df_raw 		<- read_xlsx(file.path(data_dir, "Datensatz_PhysioPD.xlsx"))
col_explanations<- df_raw[1,]
df_raw 		<- df_raw[-1,]

# -------------------------------------------------------------------------
## Part 2: Preprocessing ()
# -------------------------------------------------------------------------

source("02_tidyup_dataframe.v1.1.R")

# ==================================================================================================
## Part 3: Create TableOne for participants
source("02_CreateTableOne.v1.1.R")


## =================================================================================================
# Preamble further analysis: Data imputation using the MICE package with a multivariate approach
source("imputation.R")


# Generate and save density plot to PDF in landscape orientation
pdf(file.path(getwd(), "results", "suppl_fig1b.densityplots_afterimputation.pdf"), width = 11, height = 8.5)

densityplot(
  generate_imputation,
  xlim = c(0, 7),    # Set x-axis range for density plot
  ylim = c(0, 1)   # Set y-axis range for density plot
)

dev.off()  # Close the PDF device to save the file


## =================================================================================================
# Part 2: Exploratory factor analysis (EFA), source: https://rpubs.com/pjmurphy/758265

source("exploratory_factor_analysis.R")


## =================================================================================================
# Part 3: Odds Ratios: 
# How much more likely (or less likely) is an 'extremely or very negative' rating of the impact of Parkinson's disease on quality of life (QoL) across groups defined by different demographic and health-related characteristics?

source("odds_ratio_analysis.R")

