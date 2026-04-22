library(dplyr); library(tidyr); library(readr); library(lubridate)
library(stringr); library(janitor); library(glue); library(purrr)
source("R/00_setup.R", local=TRUE)
SOURCED_PANEL <- FALSE
source("R/04_build_panel.R", local=TRUE)
panel <- build_panel()
cat("Panel OK\n")
