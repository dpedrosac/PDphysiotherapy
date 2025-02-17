# ===========================================================
# Project: Physiotherapie and Parkinson's Disease
# Script: data_analysis.R
# Authors: Christian Isselstein and David Pedrosa
# Date: 2025-17-02
# Version: 1.0
# Description: Initial commit with some modifications
#
# Change log:
#   Version 1.0 (2025-17-02): Code was tiedied up and made easiert to read
# ===========================================================

# Set working directory based on the username
# ===========================================================
username <- Sys.getenv("USER")

if (username == "dpedr") {
  wdir <- "D:/PDphysiotherapy"
  data_dir <- file.path(wdir, "data")
} else if (username == "dpedrosac") {
  wdir <- "/media/storage/PDphysiotherapy"
  data_dir <- file.path(wdir, "data")
} else if (username == "chris") {
  wdir <- "C:/Users/chris/Desktop/Klinik/Doktorarbeit/M. Parkinson/"
  data_dir <- file.path(wdir, "Datenauswertung") # TODO: you may consider renaming this folder to 'data' ; )
} else {
  cat("Username unknown\n")
}

setwd(wdir)
source("packages.R") # Loads the packages required a little quicker

#  Load (raw) data
# ===========================================================
df_raw 		<- read_xlsx(file.path(data_dir, "Datensatz_PhysioPD.xlsx"))
col_explanations<- df_raw[1,]
df_raw 		<- df_raw[-1,]

#  Preprocess raw data
# ===========================================================
source("tidyup_dataframe.R")


# ==================================================================================================
# Create TableOne with all data

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

# TODO: I would recommend renaming variables such as "SD01" or "SD11" 
