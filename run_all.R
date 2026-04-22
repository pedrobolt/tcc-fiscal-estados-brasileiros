# =============================================================================
# run_all.R
# Script mestre — executa toda a pipeline de coleta e construção do painel
#
# USO:
#   Rscript run_all.R                  # executa tudo
#   Rscript run_all.R --force-refresh  # ignora cache e re-baixa tudo
#
# SAÍDAS:
#   output/panel_estados_brasil.csv    — painel final (27 UFs × 23 anos)
#   output/qr_cobertura_*.csv          — relatórios de qualidade
#   output/quality_report.html         — relatório HTML (requer rmarkdown)
#
# TEMPO ESTIMADO: ~45–90 min (API SICONFI é o gargalo: 621 req × 3 endpoints)
# =============================================================================

# Muda working directory para a pasta do projeto (necessário se Rscript)
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) getwd()
)
setwd(script_dir)

args <- commandArgs(trailingOnly = TRUE)
FORCE_REFRESH <- "--force-refresh" %in% args

cat("=============================================================\n")
cat(" Painel Fiscal Estadual Brasileiro — Pipeline de Coleta\n")
cat(" Período: 2002–2024 | N = 27 estados\n")
cat(sprintf(" Modo: %s\n", if (FORCE_REFRESH) "FORÇAR REFRESH" else "usar cache"))
cat(sprintf(" Início: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=============================================================\n\n")

# ---- 0. Setup ----------------------------------------------------------------
source("R/00_setup.R")

# ---- 1. SICONFI — dados fiscais (etapa mais longa) --------------------------
cat("\n[1/5] SICONFI (DCL, RCL, Resultado Primário, Encargos)...\n")
SOURCED_SICONFI <- FALSE
source("R/01_siconfi.R")
siconfi_data <- collect_siconfi(force_refresh = FORCE_REFRESH)

# ---- 2. IBGE SIDRA — PIB estadual -------------------------------------------
cat("\n[2/5] IBGE SIDRA (PIB nominal por estado)...\n")
SOURCED_IBGE <- FALSE
source("R/02_ibge_gdp.R")
pib_data <- collect_ibge_gdp()

# ---- 3. BCB/SGS — IPCA -------------------------------------------------------
cat("\n[3/5] BCB/SGS (IPCA mensal → anual)...\n")
SOURCED_BCB <- FALSE
source("R/03_bcb_ipca.R")
ipca_data <- collect_bcb_ipca()

# ---- 4. Constrói painel ------------------------------------------------------
cat("\n[4/5] Construindo painel balanceado...\n")
SOURCED_PANEL <- FALSE
source("R/04_build_panel.R")
panel <- build_panel()

# ---- 5. Relatório de qualidade -----------------------------------------------
cat("\n[5/5] Gerando relatório de qualidade...\n")
SOURCED_QR <- FALSE
source("R/05_quality_report.R")
qr <- generate_quality_report()

# ---- Sumário final -----------------------------------------------------------
cat("\n=============================================================\n")
cat(" PIPELINE CONCLUÍDA\n")
cat(sprintf(" Término: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf(" Painel : %d obs × %d variáveis\n", nrow(panel), ncol(panel)))
cat(sprintf(" Output : %s\n", normalizePath("output/")))
cat("=============================================================\n")

# Abre painel no RStudio se disponível
if (interactive() && requireNamespace("utils", quietly = TRUE)) {
  message("\nMostrando primeiras linhas do painel:")
  print(head(panel))
  message("\nVariáveis do painel:")
  glimpse(panel)
}
