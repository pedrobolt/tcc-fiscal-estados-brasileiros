library(httr); library(jsonlite); library(dplyr); library(tidyr)
library(purrr); library(readr); library(stringr); library(janitor)
library(glue); library(progress)

source("R/00_setup.R", local=TRUE)
SOURCED_SICONFI <- FALSE
source("R/01_siconfi.R", local=TRUE)
siconfi_data <- collect_siconfi()
cat("\nDone.\n")
