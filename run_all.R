# =============================================================================
# run_all.R — Pipeline completo do TCC
# Sustentabilidade Fiscal e Regras de Endividamento: Painel de Estados (2002-2024)
#
# Execução: Rscript run_all.R
# Tempo estimado: ~20-30 min (coleta de dados + 2x bootstrap B=500)
# =============================================================================

pkgs <- c("dplyr", "tidyr", "readr", "purrr", "stringr",
          "fixest", "plm", "mFilter",
          "httr", "jsonlite", "glue",
          "sidrar", "rbcb", "lubridate",
          "kableExtra", "tibble")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  message("Instalando pacotes: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

dirs <- c("data/raw/siconfi", "data/raw/ibge", "data/raw/bcb",
          "data/processed", "output/tables", "output/figures", "docs")
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

cat("=================================================================\n")
cat(" TCC — Sustentabilidade Fiscal e Regras de Endividamento\n")
cat(" Painel de Estados Brasileiros (2002-2024)\n")
cat("=================================================================\n\n")

cat("[1/6] Coletando dados (SICONFI, IBGE, BCB)...\n")
source("scripts/01_collect_data.R")

cat("\n[2/6] Construindo painel analítico...\n")
source("scripts/02_build_panel.R")

cat("\n[3/6] Estimando Modelo I (OLS-FE + 2SLS)...\n")
source("scripts/03_model1_2sls.R")

cat("\n[4/6] Estimando Modelo II (LSDVC + FE+DK) — bootstrap B=500...\n")
source("scripts/04_model2_lsdvc.R")

cat("\n[5/6] Verificações de robustez + testes de raiz unitária...\n")
source("scripts/05_robustness.R")

cat("\n[6/6] Gerando tabela de resultados...\n")
source("scripts/06_tables.R")

cat("\n=================================================================\n")
cat(" CONCLUÍDO\n")
cat(" Tabela principal : output/tables/tabela_final.html\n")
cat(" Painel analítico : data/processed/panel_final_v5.csv\n")
cat("=================================================================\n")
