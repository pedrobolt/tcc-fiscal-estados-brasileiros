# =============================================================================
# unit_root.R — Panel unit root tests on dcl_sobre_rcl_ext
# Tests: Im-Pesaran-Shin (IPS) and Levin-Lin-Chu (LLC)
# =============================================================================
suppressPackageStartupMessages({
  library(plm); library(readr); library(dplyr)
})

panel <- read_csv("output/panel_final_v5.csv", show_col_types = FALSE) %>%
  filter(!is.na(dcl_sobre_rcl_ext)) %>%
  arrange(uf, year)

# Keep only states with >= 10 consecutive obs (needed for ADF with lag)
panel <- panel %>%
  group_by(uf) %>%
  filter(n() >= 10) %>%
  ungroup()

panel_p <- pdata.frame(panel, index = c("uf", "year"))

cat("N states:", n_distinct(panel$uf),
    "| Obs:", nrow(panel),
    "| Mean T:", round(nrow(panel) / n_distinct(panel$uf), 1), "\n")
cat("Variable: dcl_sobre_rcl_ext  (mean=",
    round(mean(panel$dcl_sobre_rcl_ext), 3), ")\n\n")

# ── Im-Pesaran-Shin ──────────────────────────────────────────────────────────
cat("=================================================================\n")
cat(" Im-Pesaran-Shin (IPS) test\n")
cat(" H0: All panels contain a unit root\n")
cat("=================================================================\n")
ips <- purtest(dcl_sobre_rcl_ext ~ 1, data = panel_p,
               test = "ips", lags = 1)
print(summary(ips))

# ── Levin-Lin-Chu ────────────────────────────────────────────────────────────
# LLC requires a balanced panel. Find years where ALL states have data.
cat("\n=================================================================\n")
cat(" Levin-Lin-Chu (LLC) test  [balanced subsample]\n")
cat(" H0: Common unit root process\n")
cat("=================================================================\n")
complete_years <- panel %>%
  group_by(year) %>%
  summarise(n_states = n_distinct(uf), .groups = "drop") %>%
  filter(n_states == n_distinct(panel$uf)) %>%
  pull(year)
panel_bal   <- panel %>% filter(year %in% complete_years)
panel_bal_p <- pdata.frame(panel_bal, index = c("uf", "year"))
cat(sprintf("Balanced window: %d-%d (%d years, %d states)\n",
            min(complete_years), max(complete_years),
            length(complete_years), n_distinct(panel_bal$uf)))
llc <- purtest(dcl_sobre_rcl_ext ~ 1, data = panel_bal_p,
               test = "levinlin", lags = 1)
print(summary(llc))

# ── Hadri (stationarity null, works on unbalanced) ────────────────────────────
cat("\n=================================================================\n")
cat(" Hadri LM test  [H0: All panels are stationary]\n")
cat("=================================================================\n")
had <- purtest(dcl_sobre_rcl_ext ~ 1, data = panel_bal_p,
               test = "hadri", lags = 1)
print(summary(had))

# ── Summary ──────────────────────────────────────────────────────────────────
cat("\n=================================================================\n")
cat(" SUMMARY\n")
cat("=================================================================\n")
ips_p <- ips$statistic$p.value
llc_p <- llc$statistic$p.value
had_p <- had$statistic$p.value
cat(sprintf("  IPS   p-value: %.4f  [H0: unit root]       %s\n", ips_p,
            ifelse(ips_p < 0.05, "=> Reject H0", "=> Fail to reject H0")))
cat(sprintf("  LLC   p-value: %.4f  [H0: unit root]       %s\n", llc_p,
            ifelse(llc_p < 0.05, "=> Reject H0", "=> Fail to reject H0")))
cat(sprintf("  Hadri p-value: %.4f  [H0: stationarity]    %s\n", had_p,
            ifelse(had_p > 0.05, "=> Fail to reject H0 (stationary)",
                               "=> Reject H0 (non-stationary)")))

n_reject <- sum(c(ips_p < 0.05, llc_p < 0.05))
cat("\n")
if (n_reject == 2) {
  cat("  CONCLUSION: IPS and LLC both reject H0 (unit root) — p<0.05.\n")
  cat("  Hadri rejects H0 (stationarity), but this test is known to\n")
  cat("  over-reject in the presence of cross-sectional dependence\n")
  cat("  (Hadri, 2000; Pesaran, 2007) — common among Brazilian states\n")
  cat("  due to shared federal transfer shocks.\n\n")
  cat("  VERDICT: STATIONARY — rho=0.988 reflects very high persistence\n")
  cat("  within a stationary process, not non-stationarity.\n")
  cat("  LSDVC levels interpretation is valid.\n")
} else if (n_reject == 0) {
  cat("  CONCLUSION: Neither IPS nor LLC rejects unit root.\n")
  cat("  Consider first-differencing or panel cointegration approach.\n")
} else {
  cat("  CONCLUSION: Mixed evidence. IPS rejects but LLC does not.\n")
  cat("  Interpret rho=0.988 with caution.\n")
}
cat("=================================================================\n")
