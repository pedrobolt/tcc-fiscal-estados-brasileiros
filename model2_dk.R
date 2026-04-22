suppressPackageStartupMessages({library(dplyr);library(readr);library(fixest)})

panel <- read_csv("output/panel_final_v5.csv", show_col_types=FALSE) %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(dcl_lag1 = dplyr::lag(dcl_sobre_rcl_ext, 1)) %>%
  ungroup()

m2_dk <- feols(
  dcl_sobre_rcl_ext ~ dcl_lag1 +
    primario_sobre_rcl_ext + crescimento_pib_pct | uf + year,
  data     = panel,
  panel.id = ~uf + year,
  vcov     = "DK")

print(summary(m2_dk))

co <- coef(m2_dk); se <- se(m2_dk); pv <- pvalue(m2_dk)

cat("\n────────────────────────────────────────────────────────\n")
cat(" Comparison: θ (primario_sobre_rcl_ext)\n")
cat("────────────────────────────────────────────────────────\n")
cat(sprintf("  Diff-GMM  (AB, ind FE)  : -0.5051  SE=0.3066  p=0.100\n"))
cat(sprintf("  LSDVC     (Nickell-cor) : -0.9366  SE=0.1052  p<0.001\n"))
cat(sprintf("  FE + DK   (this model)  : %+.4f  SE=%.4f  p=%.4f\n",
            co["primario_sobre_rcl_ext"],
            se["primario_sobre_rcl_ext"],
            pv["primario_sobre_rcl_ext"]))

cat(sprintf("\n  ρ  DCL/RCL (t-1)  : %+.4f  SE=%.4f  p=%.4f\n",
            co["dcl_lag1"], se["dcl_lag1"], pv["dcl_lag1"]))
cat(sprintf("  γ  Cresc. PIB     : %+.4f  SE=%.4f  p=%.4f\n",
            co["crescimento_pib_pct"],
            se["crescimento_pib_pct"],
            pv["crescimento_pib_pct"]))
cat(sprintf("\n  Within R²  : %.4f\n", r2(m2_dk, "wr2")))
cat(sprintf("  N obs      : %d\n", m2_dk$nobs))
cat("  Note: DK SEs (L=2) robust to heteroskedasticity,\n")
cat("        serial correlation up to 2 lags, and cross-sectional dependence.\n")
cat("        Nickell downward bias in ρ not corrected (FE estimator).\n")
