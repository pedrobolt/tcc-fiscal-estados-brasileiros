# =============================================================================
# scripts/03_model1_2sls.R
# Modelo I — Função de Reação Fiscal (Bohn 1998)
#   I-A: OLS com efeitos fixos (benchmark)
#   I-B: 2SLS — instrumenta d_lag1 e d_lag1_teto com d_lag2 e d_lag2_teto
# Entrada: data/processed/panel_slim.csv
# Resultados: β₁=0.711*** (Bohn condition), β₂=−0.046*** (teto atenua ajuste)
# =============================================================================

pkgs <- c("dplyr", "readr", "fixest")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

panel <- read_csv("data/processed/panel_slim.csv", show_col_types = FALSE) %>%
  arrange(uf, year)

cat("Panel:", nrow(panel), "obs |", n_distinct(panel$uf), "estados ×",
    n_distinct(panel$year), "anos\n\n")

# =============================================================================
# MODEL I-A: OLS-FE (benchmark)
# =============================================================================
cat("════════════════════════════════════════════════════════\n")
cat(" MODEL I-A: OLS-FE (benchmark)\n")
cat("════════════════════════════════════════════════════════\n")

m1_ols <- feols(
  primario_sobre_rcl_ext ~ d_lag1 + d_lag1_teto + yvar | uf + year,
  data    = panel,
  cluster = ~uf
)
print(summary(m1_ols))

# =============================================================================
# MODEL I-B: 2SLS
# Instrumentos: d_lag2, d_lag2_teto (dois períodos defasados)
# Wu-Hausman p=0.0001 confirma endogeneidade; F-stat 1º estágio: 845/973
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" MODEL I-B: 2SLS\n")
cat("════════════════════════════════════════════════════════\n")

m1_iv <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
  data    = panel,
  cluster = ~uf
)
print(summary(m1_iv, stage = 1:2))

cat("\n--- Diagnósticos do 1º estágio (Model I-B) ---\n")
print(fitstat(m1_iv, ~ivf + wh))

# β₁ > 0 satisfaz condição de Bohn (1998): superávit sobe com dívida
# β₂ < 0: teto contratual mais alto atenua intensidade do ajuste fiscal
cat("\n════════════════════════════════════════════════════════\n")
cat(" RESULTADOS PRINCIPAIS (Model I-B)\n")
cat("════════════════════════════════════════════════════════\n")
b1 <- coef(m1_iv)["fit_d_lag1"]
b2 <- coef(m1_iv)["fit_d_lag1_teto"]
cat(sprintf("  β₁ DCL/RCL(t-1)  : %+.3f  [Condição de Bohn: dívida sustentável]\n", b1))
cat(sprintf("  β₂ DCL×Teto(t-1) : %+.3f  [Teto mais alto atenua ajuste fiscal]\n", b2))
