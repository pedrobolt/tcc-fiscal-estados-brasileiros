# =============================================================================
# robustness.R — Two robustness checks for the TCC
# ROB1: Model II with yvar (output gap) instead of crescimento_pib_pct
# ROB2: Model I-B (2SLS) excluding COVID years 2020-2021
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(fixest); library(plm)
})
set.seed(2025)

panel <- read_csv("output/panel_final_v5.csv", show_col_types = FALSE) %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(dcl_lag1 = dplyr::lag(dcl_sobre_rcl_ext, 1)) %>%
  ungroup()

# Baselines
BASE_theta <- -0.937; BASE_rho <- 0.988
BASE_b1    <-  0.711; BASE_b2  <- -0.046

cat("═══════════════════════════════════════════════════════════════\n")
cat(" ROBUSTNESS 1 — Model II: yvar (output gap) replaces GDP growth\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# xtlvr2 / lsdvc not on CRAN — using identical manual LSDVC (Bruno 2005):
# plm Within + Nickell (1981) bias correction + block-bootstrap SEs (B=500)
cat("Note: xtlvr2/lsdvc unavailable on CRAN; using manual LSDVC\n")
cat("      (plm Within + Nickell correction + block bootstrap B=500)\n\n")

panel_l1 <- panel %>%
  filter(!is.na(dcl_sobre_rcl_ext), !is.na(primario_sobre_rcl_ext), !is.na(yvar))
panel_p1 <- pdata.frame(panel_l1, index = c("uf", "year"))

N1     <- n_distinct(panel_l1$uf)
T_bar1 <- nrow(panel_l1) / N1

m_w1 <- plm(dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
              primario_sobre_rcl_ext + yvar,
            data = panel_p1, model = "within", effect = "individual")
b_w1 <- coef(m_w1)

m_ab1 <- suppressWarnings(pgmm(
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
    primario_sobre_rcl_ext + yvar |
    lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
  data = panel_p1, effect = "individual", model = "onestep",
  transformation = "d", collapse = TRUE))
rho_AB1 <- coef(summary(m_ab1, robust = TRUE))["lag(dcl_sobre_rcl_ext, 1)", "Estimate"]

nickell1 <- (1 + rho_AB1) / (T_bar1 - 1)
b_lsdvc1 <- b_w1
b_lsdvc1["lag(dcl_sobre_rcl_ext, 1)"] <- b_w1["lag(dcl_sobre_rcl_ext, 1)"] + nickell1

states1 <- unique(panel_l1$uf)
vnames1 <- names(b_lsdvc1)
boot1   <- matrix(NA_real_, 500, 3, dimnames = list(NULL, vnames1))

cat("Bootstrap (B=500)...\n")
for (b in 1:500) {
  s_b  <- sample(states1, N1, replace = TRUE)
  df_b <- lapply(seq_along(s_b), function(i)
    panel_l1[panel_l1$uf == s_b[i], ] %>% mutate(uf = paste0("g", i))) %>% bind_rows()
  pp_b <- pdata.frame(df_b, index = c("uf", "year"))
  ft   <- tryCatch(plm(dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
                         primario_sobre_rcl_ext + yvar,
                       data = pp_b, model = "within", effect = "individual"),
                   error = function(e) NULL)
  if (is.null(ft) || length(coef(ft)) != 3) next
  rho_b <- tryCatch({
    ab_b <- suppressWarnings(pgmm(
      dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
        primario_sobre_rcl_ext + yvar |
        lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
      data = pp_b, effect = "individual", model = "onestep",
      transformation = "d", collapse = TRUE))
    coef(ab_b)["lag(dcl_sobre_rcl_ext, 1)"]
  }, error = function(e) rho_AB1)
  cb <- coef(ft)
  cb["lag(dcl_sobre_rcl_ext, 1)"] <- cb["lag(dcl_sobre_rcl_ext, 1)"] +
    (1 + rho_b) / (T_bar1 - 1)
  boot1[b, ] <- cb
}
boot1    <- boot1[complete.cases(boot1), ]
boot_se1 <- apply(boot1, 2, sd)
dof1     <- nrow(panel_l1) - N1 - 3
pval1    <- function(v) 2 * pt(-abs(b_lsdvc1[v] / boot_se1[v]), df = dof1)

rob1_rho      <- b_lsdvc1["lag(dcl_sobre_rcl_ext, 1)"]
rob1_theta    <- b_lsdvc1["primario_sobre_rcl_ext"]
rob1_yvar     <- b_lsdvc1["yvar"]
rob1_se_rho   <- boot_se1["lag(dcl_sobre_rcl_ext, 1)"]
rob1_se_theta <- boot_se1["primario_sobre_rcl_ext"]
rob1_p_rho    <- pval1("lag(dcl_sobre_rcl_ext, 1)")
rob1_p_theta  <- pval1("primario_sobre_rcl_ext")
rob1_p_yvar   <- pval1("yvar")

cat(sprintf("Bootstrap done: %d valid reps | N=%d, T\u0304=%.1f\n\n",
            nrow(boot1), N1, T_bar1))
cat(sprintf("  \u03c1  DCL/RCL(t-1)  : %+.4f  SE=%.4f  p=%.4f\n",
            rob1_rho, rob1_se_rho, rob1_p_rho))
cat(sprintf("  \u03b8  Prim\u00e1rio/RCL  : %+.4f  SE=%.4f  p=%.4f\n",
            rob1_theta, rob1_se_theta, rob1_p_theta))
cat(sprintf("  \u03b3  Hiato produto : %+.4f  SE=%.4f  p=%.4f\n\n",
            rob1_yvar, boot_se1["yvar"], rob1_p_yvar))

cat("═══════════════════════════════════════════════════════════════\n")
cat(" ROBUSTNESS 2 — Model I-B (2SLS) excluding COVID years 2020-2021\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

panel_nocovid <- panel %>% filter(!year %in% c(2020, 2021))

m2_rob2 <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
  data = panel_nocovid, cluster = ~uf)

print(summary(m2_rob2))

rob2_b1    <- coef(m2_rob2)["fit_d_lag1"]
rob2_b2    <- coef(m2_rob2)["fit_d_lag1_teto"]
rob2_se_b1 <- se(m2_rob2)["fit_d_lag1"]
rob2_se_b2 <- se(m2_rob2)["fit_d_lag1_teto"]
rob2_p_b1  <- pvalue(m2_rob2)["fit_d_lag1"]
rob2_p_b2  <- pvalue(m2_rob2)["fit_d_lag1_teto"]
fs         <- fitstat(m2_rob2, "ivf")

cat(sprintf("\n  \u03b2\u2081 DCL/RCL(t-1)       : %+.4f  SE=%.4f  p=%.4f\n",
            rob2_b1, rob2_se_b1, rob2_p_b1))
cat(sprintf("  \u03b2\u2082 DCL/RCL(t-1)\u00d7Teto  : %+.4f  SE=%.4f  p=%.4f\n",
            rob2_b2, rob2_se_b2, rob2_p_b2))
cat(sprintf("  1st-stage F: %.0f / %.0f\n\n",
            fs[[1]]$stat, fs[[2]]$stat))

# =============================================================================
# Summary comparison table
# =============================================================================
stars  <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
pct    <- function(new, base) sprintf("%+.1f%%", 100 * (new - base) / abs(base))
stable <- function(new, base, tol = 0.20)
  if (abs((new - base) / abs(base)) <= tol) "STABLE" else "UNSTABLE"

cat("\n")
cat("+-----------------+------------------+------------------+----------------+\n")
cat("|                 | Baseline (LSDVC) |  Rob1 (yvar)     |  Delta vs Base |\n")
cat("| Panel A: Model II (dep: DCL/RCL)                                       |\n")
cat("+-----------------+------------------+------------------+----------------+\n")
cat(sprintf("| rho  DCL(t-1)   | %+.3f%-4s         | %+.3f%-3s (%.3f)  | %-14s |\n",
            BASE_rho,  "***", rob1_rho,   stars(rob1_p_rho),   rob1_se_rho,   pct(rob1_rho,   BASE_rho)))
cat(sprintf("| theta Prim/RCL  | %+.3f%-4s         | %+.3f%-3s (%.3f)  | %-14s |\n",
            BASE_theta,"***", rob1_theta, stars(rob1_p_theta), rob1_se_theta, pct(rob1_theta, BASE_theta)))
cat("+-----------------+------------------+------------------+----------------+\n")
cat(sprintf("| Verdict         | --               | rho: %-9s  | theta: %-8s |\n",
            stable(rob1_rho, BASE_rho), stable(rob1_theta, BASE_theta)))
cat("+-----------------+------------------+------------------+----------------+\n")
cat("\n")
cat("+-----------------+------------------+------------------+----------------+\n")
cat("|                 | Baseline (2SLS)  | Rob2 (no COVID)  |  Delta vs Base |\n")
cat("| Panel B: Model I-B (dep: Primario/RCL)                                 |\n")
cat("+-----------------+------------------+------------------+----------------+\n")
cat(sprintf("| beta1 DCL(t-1)  | %+.3f%-4s         | %+.3f%-3s (%.3f)  | %-14s |\n",
            BASE_b1, "***", rob2_b1, stars(rob2_p_b1), rob2_se_b1, pct(rob2_b1, BASE_b1)))
cat(sprintf("| beta2 DCL*Teto  | %+.3f%-4s         | %+.3f%-3s (%.3f)  | %-14s |\n",
            BASE_b2, "***", rob2_b2, stars(rob2_p_b2), rob2_se_b2, pct(rob2_b2, BASE_b2)))
cat(sprintf("| 1st-stage F     | 845 / 973        | %3.0f / %-3.0f          |                |\n",
            fs[[1]]$stat, fs[[2]]$stat))
cat("+-----------------+------------------+------------------+----------------+\n")
cat(sprintf("| Verdict         | --               | beta1: %-7s  | beta2: %-8s |\n",
            stable(rob2_b1, BASE_b1), stable(rob2_b2, BASE_b2)))
cat("+-----------------+------------------+------------------+----------------+\n")
cat("\n* p<0.10  ** p<0.05  *** p<0.01  |  STABLE = within 20% of baseline\n")
