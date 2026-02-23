#!/usr/bin/env Rscript
# -------------------------------------------------------------------------
# Title: Physiotherapy in Parkinson's Disease (PD)
#
# Description:
#   Defines and loads the R package dependencies required for the analyses
#   If a package is missing, it will be installed and then loaded.
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
#   - This script is typically sourced by preamble script.
#   - code was optimised using LLM
# -------------------------------------------------------------------------

packages <- c(

  "broom", 
  "car", 
  "dplyr",
  "ggplot2", 
  "modeest",
  "openxlsx",
  "psych", 
  "purrr",
  "readxl",
  "stringr",
  "tableone",
  "tidyverse", 
  "VIM"
)

load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
  invisible(TRUE)
}

invisible(lapply(packages, load_or_install))

