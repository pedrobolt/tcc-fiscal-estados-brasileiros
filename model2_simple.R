suppressPackageStartupMessages({library(dplyr);library(readr);library(plm)})
panel <- read_csv("output/panel_final_v5.csv", show_col_types=FALSE) %>%
  arrange(uf, year)
panel_pdata <- pdata.frame(panel, index=c("uf","year"))

cat("Attempting System-GMM (ld, twoways, collapse=TRUE)...\n\n")

m2_simple <- tryCatch(
  pgmm(
    dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
      primario_sobre_rcl_ext + crescimento_pib_pct |
      lag(dcl_sobre_rcl_ext, 2:3) +
      lag(primario_sobre_rcl_ext, 2:3),
    data           = panel_pdata,
    effect         = "twoways",
    model          = "onestep",
    transformation = "ld",
    collapse       = TRUE
  ),
  warning = function(w) {
    cat("WARNING:", conditionMessage(w), "\n")
    tryCatch(
      pgmm(
        dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
          primario_sobre_rcl_ext + crescimento_pib_pct |
          lag(dcl_sobre_rcl_ext, 2:3) +
          lag(primario_sobre_rcl_ext, 2:3),
        data           = panel_pdata,
        effect         = "twoways",
        model          = "onestep",
        transformation = "ld",
        collapse       = TRUE
      ),
      error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
    )
  },
  error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
)

if (!is.null(m2_simple)) {
  s <- summary(m2_simple, robust=TRUE)
  n_obs  <- sum(sapply(m2_simple$residuals,
                       function(x) if(!is.null(x)) length(x) else 0L))

  # Instrument count: pgmm stores them in model$W
  # Use the formula: for ld with collapse, 2 instrument vars × 2 lags + level instruments
  # Best proxy: count columns in the first block of $W[[1]]
  n_instr_approx <- tryCatch(ncol(m2_simple$W[[1]]), error=function(e) NA_integer_)

  cat("════════════════════════════════════════════════════════\n")
  cat(" MODEL II (parsimonioso): System-GMM onestep, twoways\n")
  cat("════════════════════════════════════════════════════════\n")
  cat("N obs (total residuals):", n_obs, "\n")
  cat("N instruments (cols W[[1]]):", n_instr_approx,
      if(!is.na(n_instr_approx) && n_instr_approx < 25) "✓ < 25" else "*** >= 25 ***", "\n")
  cat("\n")
  print(s)

  cat("\n--- Diagnósticos ---\n")
  cat(sprintf("  Sargan p  : %.4f %s\n", s$sargan$p.value,
              if(s$sargan$p.value > 0.05) "✓" else "*** over-identification rejected ***"))
  cat(sprintf("  AR(1) p   : %.4f\n", s$m1$p.value))
  cat(sprintf("  AR(2) p   : %.4f %s\n", s$m2$p.value,
              if(s$m2$p.value > 0.05) "✓ no 2nd-order autocorrelation"
              else "*** 2nd-order autocorrelation present ***"))

  co <- coef(s)
  theta_simple <- co["primario_sobre_rcl_ext","Estimate"]
  theta_full   <- -9.6738   # from full AB-GMM

  cat(sprintf("\n--- Comparação θ (primario_sobre_rcl_ext) ---\n"))
  cat(sprintf("  Modelo completo (AB-GMM, 5 vars) : %+.4f  (p=0.584, não sig.)\n", theta_full))
  cat(sprintf("  Modelo simples  (Sys-GMM, 3 vars): %+.4f  (p=%.4f, %s)\n",
              theta_simple,
              co["primario_sobre_rcl_ext","Pr(>|z|)"],
              if(co["primario_sobre_rcl_ext","Pr(>|z|)"] < 0.10) "sig." else "não sig."))
} else {
  # Fallback: individual effects system GMM
  cat("\nSystem-GMM twoways falhou. Tentando individual effects...\n")
  m2_fb <- tryCatch(
    pgmm(
      dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
        primario_sobre_rcl_ext + crescimento_pib_pct |
        lag(dcl_sobre_rcl_ext, 2:3) +
        lag(primario_sobre_rcl_ext, 2:3),
      data           = panel_pdata,
      effect         = "individual",
      model          = "onestep",
      transformation = "ld",
      collapse       = TRUE
    ),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(m2_fb)) {
    s_fb <- summary(m2_fb, robust=TRUE)
    n_obs_fb <- sum(sapply(m2_fb$residuals,
                           function(x) if(!is.null(x)) length(x) else 0L))
    cat("N obs:", n_obs_fb, "\n")
    print(s_fb)
    cat(sprintf("Sargan p: %.4f | AR(1) p: %.4f | AR(2) p: %.4f\n",
                s_fb$sargan$p.value, s_fb$m1$p.value, s_fb$m2$p.value))
  }
}
