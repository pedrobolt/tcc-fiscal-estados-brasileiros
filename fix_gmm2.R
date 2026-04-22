suppressPackageStartupMessages({
  library(dplyr); library(readr); library(plm); library(fixest)
})
panel <- read_csv("output/panel_final_v5.csv", show_col_types=FALSE) %>%
  arrange(uf, year)
panel_p <- pdata.frame(panel, index=c("uf","year"))

cat("════ A: pgmm one-step diff, individual, lags 2:4 ════\n")
m_a <- tryCatch(
  pgmm(
    dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
      primario_sobre_rcl_ext + encargos_sobre_rcl_ext +
      crescimento_pib_pct + primario_teto |
      lag(dcl_sobre_rcl_ext, 2:4) + lag(primario_sobre_rcl_ext, 2:3),
    data=panel_p, effect="individual", model="onestep",
    transformation="d", collapse=TRUE),
  error=function(e) { cat("ERR:", e$message, "\n"); NULL })
if (!is.null(m_a)) {
  s <- summary(m_a, robust=TRUE)
  cat("N_obs:", length(m_a$residuals), "| N_instr:", length(m_a$instruments),"\n")
  print(coef(s))
  cat("Sargan p:", round(s$sargan$p.value,4),
      "| AR1 p:", round(s$m1$p.value,4),
      "| AR2 p:", round(s$m2$p.value,4), "\n")
}

cat("\n════ B: pgmm two-step diff, individual, lags 2:3 ════\n")
m_b <- tryCatch(
  pgmm(
    dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
      primario_sobre_rcl_ext + encargos_sobre_rcl_ext +
      crescimento_pib_pct + primario_teto |
      lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
    data=panel_p, effect="individual", model="twosteps",
    transformation="d", collapse=TRUE),
  error=function(e) { cat("ERR:", e$message, "\n"); NULL })
if (!is.null(m_b)) {
  s <- summary(m_b, robust=TRUE)
  cat("N_obs:", length(m_b$residuals), "| N_instr:", length(m_b$instruments),"\n")
  print(coef(s))
  cat("Sargan p:", round(s$sargan$p.value,4),
      "| AR1 p:", round(s$m1$p.value,4),
      "| AR2 p:", round(s$m2$p.value,4), "\n")
}

cat("\n════ C: fixest 2SLS first-difference (manual) ════\n")
# First difference manually, then use lag(d.dcl, 2) as instrument
panel_fd <- panel %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(
    d_dcl         = dcl_sobre_rcl_ext - lag(dcl_sobre_rcl_ext, 1),
    d_dcl_lag1    = lag(d_dcl, 1),
    d_dcl_lag2    = lag(d_dcl, 2),
    d_prim        = primario_sobre_rcl_ext - lag(primario_sobre_rcl_ext, 1),
    d_prim_lag1   = lag(d_prim, 1),
    d_enc         = encargos_sobre_rcl_ext - lag(encargos_sobre_rcl_ext, 1),
    d_pib         = crescimento_pib_pct - lag(crescimento_pib_pct, 1),
    d_prim_teto   = primario_teto - lag(primario_teto, 1),
    d_prim_lag1_t = lag(primario_teto, 1)
  ) %>%
  ungroup()

# FD-2SLS: Δdcl ~ Δdcl(-1) + Δprim + Δenc + Δpib + Δprim_teto
#          instruments: Δdcl(-2), Δprim(-1), level lags
m_c <- feols(
  d_dcl ~ d_enc + d_pib | year |
    d_dcl_lag1 + d_prim + d_prim_teto ~
    d_dcl_lag2 + d_prim_lag1 + d_prim_lag1_t,
  data    = panel_fd,
  cluster = ~uf
)
cat("fixest FD-2SLS:\n")
print(summary(m_c, stage=1:2))
fs <- fitstat(m_c, ~ivf + wh)
print(fs)
