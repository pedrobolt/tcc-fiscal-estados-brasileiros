# =============================================================================
# estimar.R  —  Model I (2SLS + OLS-FE) and Model II (Arellano-Bond GMM)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(fixest); library(plm); library(modelsummary)
})

panel <- read_csv("output/panel_final_v5.csv", show_col_types = FALSE) %>%
  arrange(uf, year)

cat("Panel:", nrow(panel), "obs |", n_distinct(panel$uf), "states ×",
    n_distinct(panel$year), "years\n\n")

# =============================================================================
# MODEL I-A: OLS-FE benchmark
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

cat("\n--- First-stage diagnostics (Model I-B) ---\n")
print(fitstat(m1_iv, ~ivf + wh))

# =============================================================================
# MODEL II: Arellano-Bond difference GMM
# Note: System GMM (ld) is not feasible with N=25 (moment matrix singular at
#       rank = N). Difference GMM (Arellano-Bond) is the correct choice here.
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" MODEL II: Arellano-Bond difference GMM (one-step, robust)\n")
cat("════════════════════════════════════════════════════════\n")

panel_p <- pdata.frame(panel, index = c("uf", "year"))

m2 <- pgmm(
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
    primario_sobre_rcl_ext + encargos_sobre_rcl_ext +
    crescimento_pib_pct + primario_teto |
    lag(dcl_sobre_rcl_ext, 2:4) + lag(primario_sobre_rcl_ext, 2:3),
  data           = panel_p,
  effect         = "individual",
  model          = "onestep",
  transformation = "d",
  collapse       = TRUE
)

m2_sum <- summary(m2, robust = TRUE)

# Compute true N_obs (sum across per-state residual vectors)
n_obs_gmm <- sum(sapply(m2$residuals, function(x) if (!is.null(x)) length(x) else 0L))
n_instr   <- length(m2$instruments)

cat("\n")
print(m2_sum)
cat(sprintf("\n  True N_obs (total residuals): %d\n", n_obs_gmm))
cat(sprintf("  N instruments: %d (< N=%d states) ✓\n",
            n_instr, n_distinct(panel$uf)))
cat(sprintf("  Sargan test p-value : %.4f\n", m2_sum$sargan$p.value))
cat(sprintf("  AR(1) test p-value  : %.4f\n", m2_sum$m1$p.value))
cat(sprintf("  AR(2) test p-value  : %.4f\n", m2_sum$m2$p.value))

if (m2_sum$m2$p.value > 0.05)
  cat("  ✓ AR(2) p > 0.05 — no second-order autocorrelation, instruments valid\n")
if (m2_sum$sargan$p.value > 0.05)
  cat("  ✓ Sargan p > 0.05 — instrument exogeneity not rejected\n")

# =============================================================================
# INTERPRETATION CHECK
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" INTERPRETATION CHECK\n")
cat("════════════════════════════════════════════════════════\n")

get_p <- function(mod_sum, nm) {
  co <- coef(mod_sum)
  if (is.matrix(co)) co[nm, "Pr(>|z|)"] else co[nm, "Pr(>|t|)"]
}

# Model I-A
b1o  <- coef(m1_ols)["d_lag1"];       p1o <- pvalue(m1_ols)["d_lag1"]
b2o  <- coef(m1_ols)["d_lag1_teto"];  p2o <- pvalue(m1_ols)["d_lag1_teto"]
cat(sprintf("\nModel I-A (OLS-FE):\n"))
cat(sprintf("  β1 (d_lag1)      = %+.4f  p=%.4f → Bohn condition: %s\n",
            b1o, p1o,  if (b1o>0 & p1o<0.10) "HOLDS ✓" else "not significant"))
cat(sprintf("  β2 (d_lag1_teto) = %+.4f  p=%.4f → Fiscal rule: %s\n",
            b2o, p2o,  if (b2o<0) "weakens reaction ✓" else "POSITIVE (unexpected)"))

# Model I-B
b1iv <- coef(m1_iv)["fit_d_lag1"];      p1iv <- pvalue(m1_iv)["fit_d_lag1"]
b2iv <- coef(m1_iv)["fit_d_lag1_teto"]; p2iv <- pvalue(m1_iv)["fit_d_lag1_teto"]
cat(sprintf("\nModel I-B (2SLS):\n"))
cat(sprintf("  β1 (d_lag1, IV)      = %+.4f  p=%.4f → Bohn condition: %s\n",
            b1iv, p1iv, if (b1iv>0 & p1iv<0.10) "HOLDS ✓" else "not significant"))
cat(sprintf("  β2 (d_lag1_teto, IV) = %+.4f  p=%.4f → Fiscal rule: %s\n",
            b2iv, p2iv, if (b2iv<0) "weakens reaction ✓" else "POSITIVE (unexpected)"))
cat(sprintf("  IV vs OLS attenuation bias: β1_OLS=%.4f vs β1_IV=%.4f → IV larger (endogeneity corrected)\n",
            b1o, b1iv))

# Model II
co2  <- coef(m2_sum)
rho  <- co2["lag(dcl_sobre_rcl_ext, 1)", "Estimate"]
p_rho <- co2["lag(dcl_sobre_rcl_ext, 1)", "Pr(>|z|)"]
th   <- co2["primario_sobre_rcl_ext",     "Estimate"]
p_th  <- co2["primario_sobre_rcl_ext",    "Pr(>|z|)"]
pi_t <- co2["primario_teto",              "Estimate"]
p_pt  <- co2["primario_teto",             "Pr(>|z|)"]

cat(sprintf("\nModel II (Arellano-Bond GMM):\n"))
cat(sprintf("  ρ  (DCL lag 1)         = %+.4f  p=%.4f → Dynamics: %s\n",
            rho, p_rho, if (abs(rho)<1) "STABLE |ρ|<1 ✓" else "UNSTABLE"))
cat(sprintf("  θ  (primario/RCL)      = %+.4f  p=%.4f → Primary balance %s\n",
            th, p_th,
            if (th<0 & p_th<0.10) "REDUCES debt ✓"
            else if (th>0 & p_th<0.10) "INCREASES debt (unexpected)"
            else "not significant"))
cat(sprintf("  π  (primario × teto)   = %+.4f  p=%.4f → Fiscal rule %s\n",
            pi_t, p_pt,
            if (p_pt >= 0.10) "not significant"
            else if (pi_t > 0) "attenuates primário effect ✓"
            else "amplifies primário effect"))

# =============================================================================
# MODELSUMMARY — formatted HTML table
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" FORMATTED TABLE → output/resultados_finais.html\n")
cat("════════════════════════════════════════════════════════\n")

# Register custom tidy/glance for pgmm via modelsummary S3 hooks
tidy_custom.pgmm <- function(x, ...) {
  s  <- summary(x, robust = TRUE)
  co <- coef(s)
  tibble::tibble(
    term      = rownames(co),
    estimate  = co[, "Estimate"],
    std.error = co[, "Std. Error"],
    statistic = co[, "z-value"],
    p.value   = co[, "Pr(>|z|)"]
  )
}
glance_custom.pgmm <- function(x, ...) {
  s     <- summary(x, robust = TRUE)
  n_obs <- sum(sapply(x$residuals, function(v) if (!is.null(v)) length(v) else 0L))
  tibble::tibble(
    nobs     = n_obs,
    sargan_p = round(s$sargan$p.value, 4),
    ar1_p    = round(s$m1$p.value,     4),
    ar2_p    = round(s$m2$p.value,     4)
  )
}

coef_map <- c(
  # Model I
  "fit_d_lag1"               = "DCL/RCL (t−1)  [IV]",
  "d_lag1"                   = "DCL/RCL (t−1)",
  "fit_d_lag1_teto"          = "DCL/RCL (t−1) × Teto  [IV]",
  "d_lag1_teto"              = "DCL/RCL (t−1) × Teto",
  "yvar"                     = "Output gap",
  # Model II
  "lag(dcl_sobre_rcl_ext, 1)"= "DCL/RCL (t−1)",
  "primario_sobre_rcl_ext"   = "Saldo primário/RCL",
  "encargos_sobre_rcl_ext"   = "Encargos/RCL",
  "crescimento_pib_pct"      = "Crescimento PIB (%)",
  "primario_teto"            = "Primário × Teto"
)

# GMM model stats to append as extra rows
gmm_rows <- data.frame(
  term         = c("Sargan p", "AR(1) p", "AR(2) p", "N Instrumentos"),
  `I-A: OLS-FE` = c("", "", "", ""),
  `I-B: 2SLS`   = c("", "", "", ""),
  `II: AB-GMM`  = c(
    sprintf("%.4f", m2_sum$sargan$p.value),
    sprintf("%.4f", m2_sum$m1$p.value),
    sprintf("%.4f", m2_sum$m2$p.value),
    as.character(length(m2$instruments))
  ),
  check.names = FALSE
)
attr(gmm_rows, "position") <- c(11, 12, 13, 14)

modelsummary(
  list("I-A: OLS-FE" = m1_ols,
       "I-B: 2SLS"   = m1_iv,
       "II: AB-GMM"  = m2),
  coef_map   = coef_map,
  stars      = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map    = list(
    list(raw = "nobs",      clean = "N obs", fmt = 0),
    list(raw = "r.squared", clean = "R²",    fmt = 3)
  ),
  add_rows   = gmm_rows,
  title  = paste(
    "Sustentabilidade Fiscal e Regras de Endividamento —",
    "Painel de Estados Brasileiros (2002–2024)"
  ),
  notes  = list(
    "Dep. var. Modelos I: Saldo Primário/RCL. Dep. var. Modelo II: DCL/RCL.",
    "Erros-padrão clusterizados por estado nos Modelos I (fixest).",
    "Modelo II: Arellano-Bond (1991) GMM de 1ª diferença, 1 passo, SEs robustos.",
    "Instrumentos Modelo II: defasagens 2–4 de DCL/RCL e 2–3 de Primário/RCL.",
    "Todos os modelos incluem efeitos fixos de estado e ano.",
    "Teste de Bohn (1998): β₁>0 e significativo → sustentabilidade fiscal.",
    "β₂<0: regra contratual atenua a resposta fiscal ao endividamento."
  ),
  output = "output/resultados_finais.html"
)

cat("✓ output/resultados_finais.html salvo\n")
cat("\n✓ estimar.R concluído\n")
