library(readr); library(dplyr)

df <- read_csv("data/processed/siconfi_processado.csv", show_col_types=FALSE)

cat("=== Cobertura por ano ===\n")
df %>%
  group_by(year) %>%
  summarise(
    n_dcl    = sum(!is.na(dcl)),
    n_rcl    = sum(!is.na(rcl_rgf)),
    n_prim   = sum(!is.na(resultado_primario)),
    n_enc    = sum(!is.na(encargos_divida)),
    .groups  = "drop"
  ) %>% print(n=23)

cat("\n=== Anos com DCL mas sem primário ===\n")
df %>%
  filter(!is.na(dcl), is.na(resultado_primario)) %>%
  count(year) %>% print(n=23)

cat("\n=== Amostra SP com primário NA ===\n")
df %>% filter(uf=="SP", is.na(resultado_primario)) %>%
  select(uf, year, dcl, rcl_rgf, resultado_primario) %>% print(n=23)
