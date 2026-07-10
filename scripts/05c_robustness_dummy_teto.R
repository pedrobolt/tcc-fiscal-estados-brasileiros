# =============================================================================
# scripts/05c_robustness_dummy_teto.R
# ROB4 — Robustez: teto contínuo (12/13/15) vs dummy binária (teto >= 15)
#
# HIPÓTESE: se o efeito disciplinador do teto é robusto à forma funcional,
# o coeficiente de interação deve ter o mesmo sinal negativo nos dois modelos.
#
# MODELO A (baseline):  d_lag1 x teto contínuo   (teto = 12, 13 ou 15)
# MODELO B (robustez):  d_lag1 x dummy_teto_alto  (1 se teto >= 15, 0 caso contrário)
#
# Entrada: data/processed/panel_final_v5.csv
# Saída  : output/tables/tabela_rob4_dummy_teto.tex  (LaTeX)
#          output/tables/tabela_rob4_dummy_teto.html (HTML)
# =============================================================================

pkgs <- c("dplyr", "readr", "fixest", "modelsummary")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

cat("================================================================\n")
cat(" ROB4 — Funcao de Reacao Fiscal: teto continuo vs binario\n")
cat("================================================================\n\n")

# --- 1. Carregar dados (mesma base do 03_model1_2sls.R) ---------------------
df <- read_csv("data/processed/panel_slim.csv", show_col_types = FALSE) %>%
  arrange(uf, year)

df$uf <- as.factor(df$uf)

# --- 2. Construir variáveis binárias ----------------------------------------
# teto assume valores 12, 13 e 15 — dummy=1 identifica o teto mais restritivo
df$dummy_teto_alto <- as.integer(!is.na(df$teto) & df$teto >= 15)
df$d_lag1_dummy    <- df$d_lag1 * df$dummy_teto_alto
df$d_lag2_dummy    <- df$d_lag2 * df$dummy_teto_alto

cat("--- Distribuicao dummy_teto_alto por valor de teto ---\n")
print(table(teto = df$teto, dummy_alto = df$dummy_teto_alto, useNA = "ifany"))
cat(sprintf("\n  Obs com dummy=1 (teto=15%%): %d\n",  sum(df$dummy_teto_alto, na.rm = TRUE)))
cat(sprintf("  Obs com dummy=0 (teto<15%%): %d\n\n", sum(!df$dummy_teto_alto, na.rm = TRUE)))

# --- 3. Estimar os dois modelos 2SLS ----------------------------------------
panel_A <- df[!is.na(df$primario_sobre_rcl_ext) &
                !is.na(df$d_lag1) & !is.na(df$d_lag2) &
                !is.na(df$d_lag1_teto) & !is.na(df$d_lag2_teto) &
                !is.na(df$yvar), ]

panel_B <- df[!is.na(df$primario_sobre_rcl_ext) &
                !is.na(df$d_lag1) & !is.na(df$d_lag2) &
                !is.na(df$d_lag1_dummy) & !is.na(df$d_lag2_dummy) &
                !is.na(df$yvar), ]

cat("--- Estimando Modelo A: teto continuo (baseline) ---\n")
modeloA <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
  data    = panel_A,
  cluster = ~uf
)

cat("--- Estimando Modelo B: dummy teto alto (robustez) ---\n")
modeloB <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_dummy ~ d_lag2 + d_lag2_dummy,
  data    = panel_B,
  cluster = ~uf
)

# --- 4. Diagnósticos de primeiro estágio ------------------------------------
cat("\n--- Diagnosticos 1 estagio: Modelo A ---\n")
print(fitstat(modeloA, ~ ivf + sargan))

cat("\n--- Diagnosticos 1 estagio: Modelo B ---\n")
print(fitstat(modeloB, ~ ivf + sargan))

# --- 5. Tabela comparativa --------------------------------------------------
modelos <- list(
  "Modelo A (teto continuo)"  = modeloA,
  "Modelo B (dummy >= 15%)"   = modeloB
)

coef_map <- c(
  "fit_d_lag1"       = "DCL/RCL defasada (beta1)",
  "fit_d_lag1_teto"  = "DCL/RCL x Teto continuo (beta2)",
  "fit_d_lag1_dummy" = "DCL/RCL x Dummy teto alto (beta2)",
  "yvar"             = "Hiato do produto"
)

cat("\n================================================================\n")
cat(" TABELA COMPARATIVA\n")
cat("================================================================\n\n")

modelsummary(
  modelos,
  coef_map  = coef_map,
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map   = list(
    list(raw = "nobs",      clean = "Observacoes",  fmt = 0),
    list(raw = "r.squared", clean = "R2",           fmt = 3)
  ),
  title  = "ROB4: Funcao de Reacao Fiscal — Teto Continuo vs Binario",
  notes  = "Erros padrao clusterizados por estado. Efeitos fixos de estado e ano. Estimacao 2SLS (fixest::feols).",
  output = "markdown"
)

# --- 6. Exportar tabelas ----------------------------------------------------
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

modelsummary(
  modelos,
  coef_map  = coef_map,
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map   = list(
    list(raw = "nobs",      clean = "Observacoes",  fmt = 0),
    list(raw = "r.squared", clean = "R2",           fmt = 3)
  ),
  title  = "ROB4: Funcao de Reacao Fiscal --- Teto Continuo vs Binario",
  notes  = "Erros padrao clusterizados por estado. Efeitos fixos de estado e ano. 2SLS (fixest::feols).",
  output = "output/tables/tabela_rob4_dummy_teto.tex"
)

modelsummary(
  modelos,
  coef_map  = coef_map,
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map   = list(
    list(raw = "nobs",      clean = "Observacoes",  fmt = 0),
    list(raw = "r.squared", clean = "R2",           fmt = 3)
  ),
  title  = "ROB4: Funcao de Reacao Fiscal - Teto Continuo vs Binario",
  notes  = "Erros padrao clusterizados por estado. Efeitos fixos de estado e ano. 2SLS (fixest::feols).",
  output = "output/tables/tabela_rob4_dummy_teto.html"
)

cat("\nTabelas salvas em output/tables/tabela_rob4_dummy_teto.{tex,html}\n")

# --- 7. Interpretação automática --------------------------------------------
b2_A <- coef(modeloA)["fit_d_lag1_teto"]
b2_B <- coef(modeloB)["fit_d_lag1_dummy"]
p2_A <- pvalue(modeloA)["fit_d_lag1_teto"]
p2_B <- pvalue(modeloB)["fit_d_lag1_dummy"]

mk <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

cat("\n================================================================\n")
cat(" RESULTADO ROB4\n")
cat("================================================================\n\n")
cat(sprintf("  Modelo A — beta2 (teto continuo) : %+.4f%s  p=%.4f\n",
            b2_A, mk(p2_A), p2_A))
cat(sprintf("  Modelo B — beta2 (dummy binaria) : %+.4f%s  p=%.4f\n",
            b2_B, mk(p2_B), p2_B))

cat("\n  Interpretacao:\n")
if (!is.na(b2_A) && !is.na(b2_B) && sign(b2_A) == sign(b2_B)) {
  cat("  [ROBUSTO] Mesmo sinal nos dois modelos.\n")
  if (b2_A < 0 && b2_B < 0) {
    cat("  Estados com teto mais alto reduzem o ajuste fiscal em resposta\n")
    cat("  ao endividamento — resultado consistente independentemente da\n")
    cat("  forma funcional do teto (continua ou binaria).\n")
  }
} else {
  cat("  [ATENCAO] Sinais divergem entre as especificacoes. Investigar\n")
  cat("  distribuicao da dummy e cobertura de cada grupo de teto.\n")
}
cat("================================================================\n")
