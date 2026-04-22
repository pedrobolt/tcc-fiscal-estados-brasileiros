suppressPackageStartupMessages({
  library(dplyr); library(readr); library(plm); library(fixest)
})
panel <- read_csv("output/panel_final_v5.csv", show_col_types=FALSE) %>%
  arrange(uf, year)

# ── Check why pgmm returns N_obs=25 ─────────────────────────────────────────
panel_p <- pdata.frame(panel, index=c("uf","year"))
cat("pdata.frame dim:", dim(panel_p), "\n")
cat("pdim:\n"); print(pdim(panel_p))

# pgmm residuals structure
m_test <- suppressWarnings(pgmm(
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
    primario_sobre_rcl_ext + encargos_sobre_rcl_ext +
    crescimento_pib_pct + primario_teto |
    lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
  data=panel_p, effect="individual", model="onestep",
  transformation="d", collapse=TRUE))
cat("\nresiduals structure:\n")
cat("  class:", class(m_test$residuals), "\n")
cat("  length:", length(m_test$residuals), "\n")
# true N_obs
n_true <- sum(!sapply(m_test$residuals, is.null))
cat("  n non-null groups:", n_true, "\n")
total_resid <- sum(sapply(m_test$residuals, function(x) if(!is.null(x)) length(x) else 0))
cat("  total residual obs:", total_resid, "\n")
s <- summary(m_test, robust=TRUE)
cat("Sargan p:", round(s$sargan$p.value,4),
    "| AR1 p:", round(s$m1$p.value,4),
    "| AR2 p:", round(s$m2$p.value,4), "\n")
cat("\nCoefficients (Arellano-Bond, onestep, robust):\n")
print(coef(s))

# ── Balanced panel subset for cleaner GMM ───────────────────────────────────
cat("\n════ Balanced subset (DF excluded, 2004-2022) ════\n")
panel_bal <- panel %>%
  filter(uf != "DF", year %in% 2004:2022) %>%
  filter(!is.na(dcl_sobre_rcl_ext), !is.na(primario_sobre_rcl_ext),
         !is.na(encargos_sobre_rcl_ext), !is.na(crescimento_pib_pct))

cat("Balanced subset:", nrow(panel_bal), "obs |",
    n_distinct(panel_bal$uf), "states ×",
    n_distinct(panel_bal$year), "years\n")

panel_bal_p <- pdata.frame(panel_bal, index=c("uf","year"))
cat("pdim balanced:\n"); print(pdim(panel_bal_p))

m_bal <- tryCatch(
  pgmm(
    dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
      primario_sobre_rcl_ext + encargos_sobre_rcl_ext +
      crescimento_pib_pct + primario_teto |
      lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
    data=panel_bal_p, effect="twoways", model="twosteps",
    transformation="ld", collapse=TRUE),
  error=function(e) { cat("ERR twoways:", e$message, "\n"); NULL })

if (is.null(m_bal)) {
  cat("Trying individual effects...\n")
  m_bal <- tryCatch(
    pgmm(
      dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
        primario_sobre_rcl_ext + encargos_sobre_rcl_ext +
        crescimento_pib_pct + primario_teto |
        lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
      data=panel_bal_p, effect="individual", model="twosteps",
      transformation="ld", collapse=TRUE),
    error=function(e) { cat("ERR individual:", e$message, "\n"); NULL })
}

if (!is.null(m_bal)) {
  s_bal <- summary(m_bal, robust=TRUE)
  total_obs <- sum(sapply(m_bal$residuals, function(x) if(!is.null(x)) length(x) else 0))
  cat("N_obs:", total_obs, "\n")
  print(coef(s_bal))
  cat("Sargan p:", round(s_bal$sargan$p.value,4),
      "| AR1 p:", round(s_bal$m1$p.value,4),
      "| AR2 p:", round(s_bal$m2$p.value,4), "\n")
}

# ── feols Panel-IV as Model II alternative ───────────────────────────────────
cat("\n════ Panel-IV via feols (robust alternative to GMM) ════\n")
# Instrument: d_lag2 for d_lag1 (debt persistence), primario lag for primario
panel_iv <- panel %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(
    dcl_lag1      = lag(dcl_sobre_rcl_ext, 1),
    dcl_lag2      = lag(dcl_sobre_rcl_ext, 2),
    prim_lag1     = lag(primario_sobre_rcl_ext, 1),
    prim_teto_lag1= lag(primario_teto, 1)
  ) %>%
  ungroup()

m_piv <- feols(
  dcl_sobre_rcl_ext ~ encargos_sobre_rcl_ext + crescimento_pib_pct | uf + year |
    dcl_lag1 + primario_sobre_rcl_ext + primario_teto ~
    dcl_lag2  + prim_lag1             + prim_teto_lag1,
  data    = panel_iv,
  cluster = ~uf
)
cat("\nPanel-IV (feols), dep=dcl_sobre_rcl_ext:\n")
print(summary(m_piv, stage=1:2))
cat("\nFirst-stage F-stats and Wu-Hausman:\n")
print(fitstat(m_piv, ~ivf + wh))
