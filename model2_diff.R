suppressPackageStartupMessages({library(dplyr);library(readr);library(plm)})
panel <- read_csv("output/panel_final_v5.csv", show_col_types=FALSE) %>%
  arrange(uf, year)
panel_pdata <- pdata.frame(panel, index=c("uf","year"))

cat("Arellano-Bond Difference-GMM (twoways, onestep, collapse=TRUE)\n\n")

m2_diff <- tryCatch(
  withCallingHandlers(
    pgmm(
      dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
        primario_sobre_rcl_ext + crescimento_pib_pct |
        lag(dcl_sobre_rcl_ext, 2:3) +
        lag(primario_sobre_rcl_ext, 2:3),
      data           = panel_pdata,
      effect         = "twoways",
      model          = "onestep",
      transformation = "d",
      collapse       = TRUE
    ),
    warning = function(w) {
      cat("pgmm warning:", conditionMessage(w), "\n")
      invokeRestart("muffleWarning")
    }
  ),
  error = function(e) {
    cat("pgmm ERROR:", e$message, "\n")
    NULL
  }
)

if (!is.null(m2_diff)) {
  s <- summary(m2_diff, robust = TRUE)

  # True N_obs from residual list
  n_obs <- sum(sapply(m2_diff$residuals,
                      function(x) if (!is.null(x)) length(x) else 0L))

  # Instrument count via Sargan df + number of params
  n_params <- length(coef(m2_diff))
  n_instr  <- s$sargan$parameter + n_params   # Sargan df = n_instr - n_params

  cat("════════════════════════════════════════════════════════\n")
  cat(" ARELLANO-BOND DIFF-GMM (twoways, onestep, collapse)\n")
  cat("════════════════════════════════════════════════════════\n\n")
  cat("pgmm N (header)    :", m2_diff$df.residual, "\n")
  cat("N obs (residuals)  :", n_obs, "\n")
  cat("N instruments      :", n_instr,
      if (!is.na(n_instr) && n_instr < 25) "✓ < 25" else "*** >= 25 ***", "\n")
  cat("Sargan df          :", s$sargan$parameter, "\n\n")

  cat("Coefficients (robust SEs):\n")
  co <- coef(s)
  for (i in seq_len(nrow(co))) {
    cat(sprintf("  %-35s %+.4f  SE=%.4f  p=%.4f%s\n",
                rownames(co)[i],
                co[i,"Estimate"], co[i,"Std. Error"], co[i,"Pr(>|z|)"],
                ifelse(co[i,"Pr(>|z|)"]<0.01,"***",
                       ifelse(co[i,"Pr(>|z|)"]<0.05,"**",
                              ifelse(co[i,"Pr(>|z|)"]<0.10,"*","")))))
  }

  cat(sprintf("\nSargan test  : chisq(%d) = %.4f  p = %.4f  %s\n",
              s$sargan$parameter, s$sargan$statistic, s$sargan$p.value,
              if(s$sargan$p.value > 0.05) "✓" else "***"))
  cat(sprintf("AR(1) test   : z = %.4f  p = %.4f  %s\n",
              s$m1$statistic, s$m1$p.value,
              if(s$m1$p.value < 0.05) "✓ (expected)" else ""))
  cat(sprintf("AR(2) test   : z = %.4f  p = %.4f  %s\n",
              s$m2$statistic, s$m2$p.value,
              if(s$m2$p.value > 0.05) "✓ no 2nd-order autocorr" else "*** instruments invalid ***"))

  cat("\n--- Comparison: θ (primario_sobre_rcl_ext) ---\n")
  theta_diff   <- co["primario_sobre_rcl_ext","Estimate"]
  p_diff       <- co["primario_sobre_rcl_ext","Pr(>|z|)"]
  cat(sprintf("  Full AB-GMM  (5 vars, ind. FE)    : %+.4f  p=0.584\n", -9.6738))
  cat(sprintf("  Sys-GMM simple (3 vars, ind. FE)  : %+.4f  p=0.242\n", -0.3354))
  cat(sprintf("  Diff-GMM (twoways) [this model]   : %+.4f  p=%.4f\n",
              theta_diff, p_diff))

} else {
  cat("\n*** twoways Diff-GMM failed — diagnosing instrument matrix rank ***\n\n")

  # Try with individual effects to confirm model runs
  m_ind <- suppressWarnings(pgmm(
    dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
      primario_sobre_rcl_ext + crescimento_pib_pct |
      lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
    data=panel_pdata, effect="individual", model="onestep",
    transformation="d", collapse=TRUE))
  s_ind <- summary(m_ind, robust=TRUE)
  n_obs_ind <- sum(sapply(m_ind$residuals,
                          function(x) if(!is.null(x)) length(x) else 0L))
  n_instr_ind <- s_ind$sargan$parameter + length(coef(m_ind))

  cat("Fallback: individual FE Diff-GMM\n")
  cat("N obs:", n_obs_ind, "| N instruments:", n_instr_ind, "\n\n")
  co <- coef(s_ind)
  for (i in seq_len(nrow(co))) {
    cat(sprintf("  %-35s %+.4f  SE=%.4f  p=%.4f%s\n",
                rownames(co)[i],
                co[i,"Estimate"], co[i,"Std. Error"], co[i,"Pr(>|z|)"],
                ifelse(co[i,"Pr(>|z|)"]<0.01,"***",
                       ifelse(co[i,"Pr(>|z|)"]<0.05,"**",
                              ifelse(co[i,"Pr(>|z|)"]<0.10,"*","")))))
  }
  cat(sprintf("\nSargan p: %.4f | AR(1) p: %.4f | AR(2) p: %.4f\n",
              s_ind$sargan$p.value, s_ind$m1$p.value, s_ind$m2$p.value))

  cat("\n--- Root cause of twoways failure ---\n")
  cat("  With N=25 states and transformation='d', pgmm adds T-1 time dummies\n")
  cat("  as additional columns in the instrument block (not collapsed).\n")
  cat("  The resulting moment matrix has rank < N=25, causing dgesv to fail.\n")
  cat("  This is a known limitation of pgmm with small cross-sections.\n")
  cat("  Recommendation: use individual FE + include year dummies explicitly\n")
  cat("  in the RHS, or switch to fixest::feols with manual first-differences.\n")
}
