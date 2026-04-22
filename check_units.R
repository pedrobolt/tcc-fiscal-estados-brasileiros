library(readr); library(dplyr)
p <- read_csv("output/panel_final_v3.csv", show_col_types=FALSE)
cat("Panel rcl/dcl columns:", paste(names(p)[grep("rcl|dcl|prim", names(p))], collapse=", "), "\n\n")

cat("AC 2002-2005 (rcl_hist in R$ mil?):\n")
p %>% filter(uf=="AC", year %in% 2002:2005) %>%
  select(uf, year, rcl_hist, dcl_hist, primario_sobre_rcl) %>% print()

cat("\nSP 2002-2006:\n")
p %>% filter(uf=="SP", year %in% 2002:2006) %>%
  select(uf, year, rcl_hist, primario_sobre_rcl) %>% print()

cat("\nMG 2008:\n")
p %>% filter(uf=="MG", year==2008) %>%
  select(uf, year, rcl_hist, primario_sobre_rcl) %>% print()
