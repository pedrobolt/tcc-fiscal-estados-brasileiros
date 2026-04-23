# =============================================================================
# scripts/02_build_panel.R
# Constrói o painel analítico final a partir dos dados brutos coletados.
# Etapas: merge SICONFI + IBGE + BCB, tetos Lei 9.496/97, lags, HP-filter yvar
# Entrada: data/raw/ | Saída: data/processed/panel_final_v5.csv
# =============================================================================

pkgs <- c("dplyr", "tidyr", "readr", "mFilter", "stringr", "purrr", "plm")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

cat("=== 02_build_panel.R ===\n\n")

# --- Tetos contratuais Lei 9.496/97 ------------------------------------------
tetos_df <- read_csv("data/raw/tetos_lei9496.csv", show_col_types = FALSE)

# --- Constrói painel bruto via scripts originais -----------------------------
# R/04_build_panel.R produz output/panel_estados_brasil.csv
source("R/04_build_panel.R")

# build_panel_final.R adiciona lags, interações e yvar inicial
source("build_panel_final.R")

# fix_yvar.R recalcula yvar como desvio % do trend HP em log(pib_real_mil)
source("fix_yvar.R")

# --- Copia resultado final para data/processed/ ------------------------------
if (file.exists("output/panel_final_v5.csv")) {
  file.copy("output/panel_final_v5.csv", "data/processed/panel_final_v5.csv",
            overwrite = TRUE)
  panel_check <- read_csv("data/processed/panel_final_v5.csv",
                          show_col_types = FALSE)
  cat(sprintf("\n✓ data/processed/panel_final_v5.csv  [%d obs × %d vars]\n",
              nrow(panel_check), ncol(panel_check)))
} else {
  stop("output/panel_final_v5.csv não encontrado — verifique os scripts anteriores.")
}
