# =============================================================================
# scripts/01_collect_data.R
# Coleta todos os dados brutos:
#   - DCL, RCL, primário, encargos (STN/SICONFI API)
#   - PIB estadual real (IBGE SIDRA tabela 5938)
#   - IPCA mensal (BCB SGS série 433)
# Saída: data/raw/ (arquivos RDS/CSV por fonte)
# =============================================================================

pkgs <- c("dplyr", "readr", "httr", "jsonlite", "glue",
          "sidrar", "rbcb", "lubridate", "purrr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")

dir.create("data/raw/siconfi", recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw/ibge",    recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw/bcb",     recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed",   recursive = TRUE, showWarnings = FALSE)

cat("=== 01_collect_data.R ===\n\n")

source("R/00_setup.R")

cat("--- Coletando SICONFI (DCL, RCL, primário, encargos) ---\n")
source("R/01_siconfi.R")

cat("\n--- Coletando PIB estadual (IBGE SIDRA) ---\n")
source("R/02_ibge_gdp.R")

cat("\n--- Coletando IPCA (BCB SGS) ---\n")
source("R/03_bcb_ipca.R")

cat("\nDados brutos coletados em data/raw/\n")
