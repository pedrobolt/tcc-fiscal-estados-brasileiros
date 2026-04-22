# Diagnose and fix GMM singularity
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(plm)
})
panel <- read_csv("output/panel_final_v5.csv", show_col_types=FALSE) %>%
  arrange(uf, year)
panel_p <- pdata.frame(panel, index=c("uf","year"))

try_gmm <- function(lbl, ...) {
  cat(sprintf("\n--- %s ---\n", lbl))
  m <- tryCatch(pgmm(..., data=panel_p), error=function(e) { cat("ERROR:", e$message,"\n"); NULL })
  if (!is.null(m)) {
    s <- summary(m, robust=TRUE)
    cat("Converged. N_obs:", length(m$residuals),
        "| N_instr:", length(m$instruments),
        "| Sargan p:", round(s$sargan$p.value,4),
        "| AR1 p:", round(s$m1$p.value,4),
        "| AR2 p:", round(s$m2$p.value,4), "\n")
    print(coef(s))
  }
  m
}

# Attempt 1: Difference GMM only (Arellano-Bond)
m_ab <- try_gmm("Difference GMM (Arellano-Bond), lags 2:3",
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
    primario_sobre_rcl_ext + encargos_sobre_rcl_ext +
    crescimento_pib_pct + primario_teto |
    lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
  effect="twoways", model="twosteps", transformation="d", collapse=TRUE)

# Attempt 2: System GMM, individual effects only, lags 2:3
m_sys_ind <- try_gmm("System GMM, individual effects, lags 2:3",
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
    primario_sobre_rcl_ext + encargos_sobre_rcl_ext +
    crescimento_pib_pct + primario_teto |
    lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
  effect="individual", model="twosteps", transformation="ld", collapse=TRUE)

# Attempt 3: System GMM, twoways, lags 2:3, fewer instruments
m_sys_tw <- try_gmm("System GMM, twoways, lags 2:3",
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
    primario_sobre_rcl_ext + encargos_sobre_rcl_ext +
    crescimento_pib_pct + primario_teto |
    lag(dcl_sobre_rcl_ext, 2:3),
  effect="twoways", model="twosteps", transformation="ld", collapse=TRUE)
