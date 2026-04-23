# =============================================================================
# scripts/04_model2_lsdvc.R
# Modelo II — Dinâmica da Dívida Pública Estadual
#   II-A: LSDVC (Bruno 2005) — Within + Nickell bias correction + bootstrap SEs
#   II-B: FE + Driscoll-Kraay SEs (robustez a dependência cross-seccional)
# Entrada: data/processed/panel_final_v5.csv
# Resultados: ρ=0.988***, θ=−0.937*** (LSDVC); ρ=0.862***, θ=−0.746*** (FE+DK)
# =============================================================================

pkgs <- c("dplyr", "readr", "plm", "fixest")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

set.seed(2025)

panel <- read_csv("data/processed/panel_final_v5.csv", show_col_types = FALSE) %>%
  arrange(uf, year) %>%
  filter(!is.na(dcl_sobre_rcl_ext),
         !is.na(primario_sobre_rcl_ext),
         !is.na(crescimento_pib_pct))

panel_p <- pdata.frame(panel, index = c("uf", "year"))
N       <- n_distinct(panel$uf)
T_mean  <- mean(panel %>% count(uf) %>% pull(n))

cat(sprintf("Amostra: N=%d estados | N_obs=%d | T̄=%.1f\n\n", N, nrow(panel), T_mean))

# =============================================================================
# MODEL II-A: LSDVC
# Step 1 — Within (LSDV)
# =============================================================================
cat("════════════════════════════════════════════════════════\n")
cat(" MODEL II-A: LSDVC (Nickell-corrected Within)\n")
cat("════════════════════════════════════════════════════════\n")

m_within <- plm(
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
    primario_sobre_rcl_ext + crescimento_pib_pct,
  data = panel_p, model = "within", effect = "individual"
)
b_w  <- coef(m_within)
se_w <- sqrt(diag(vcovHC(m_within, method = "arellano", type = "HC1")))
p_w  <- 2 * pt(-abs(b_w / se_w), df = df.residual(m_within))

# Step 2 — AB-GMM ρ for Nickell bias correction
m_ab <- suppressWarnings(pgmm(
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
    primario_sobre_rcl_ext + crescimento_pib_pct |
    lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
  data = panel_p, effect = "individual", model = "onestep",
  transformation = "d", collapse = TRUE
))
rho_AB  <- coef(summary(m_ab, robust = TRUE))["lag(dcl_sobre_rcl_ext, 1)", "Estimate"]
nickell <- (1 + rho_AB) / (T_mean - 1)

b_lsdvc <- b_w
b_lsdvc["lag(dcl_sobre_rcl_ext, 1)"] <- b_w["lag(dcl_sobre_rcl_ext, 1)"] + nickell
cat(sprintf("AB-GMM ρ_initial=%.4f | Nickell correction=+%.4f\n\n", rho_AB, nickell))

# Step 3 — Block bootstrap SEs (resample states)
B      <- 500
states <- unique(panel$uf)
vnames <- names(b_lsdvc)
boot_m <- matrix(NA_real_, B, length(vnames), dimnames = list(NULL, vnames))

cat("Block bootstrap (B=500)...\n")
for (b in seq_len(B)) {
  s_b  <- sample(states, N, replace = TRUE)
  df_b <- lapply(seq_along(s_b), function(i)
    panel[panel$uf == s_b[i], ] %>% mutate(uf = paste0("g", i))
  ) %>% bind_rows()
  pp_b  <- pdata.frame(df_b, index = c("uf", "year"))
  fit_b <- tryCatch(
    plm(dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
          primario_sobre_rcl_ext + crescimento_pib_pct,
        data = pp_b, model = "within", effect = "individual"),
    error = function(e) NULL)
  if (is.null(fit_b) || length(coef(fit_b)) != length(vnames)) next
  rho_b <- tryCatch({
    ab_b <- suppressWarnings(pgmm(
      dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
        primario_sobre_rcl_ext + crescimento_pib_pct |
        lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
      data = pp_b, effect = "individual", model = "onestep",
      transformation = "d", collapse = TRUE))
    coef(ab_b)["lag(dcl_sobre_rcl_ext, 1)"]
  }, error = function(e) rho_AB)
  cb <- coef(fit_b)
  cb["lag(dcl_sobre_rcl_ext, 1)"] <- cb["lag(dcl_sobre_rcl_ext, 1)"] +
    (1 + rho_b) / (T_mean - 1)
  boot_m[b, ] <- cb
}

boot_m  <- boot_m[complete.cases(boot_m), ]
boot_se <- apply(boot_m, 2, sd)
boot_lo <- apply(boot_m, 2, quantile, 0.025)
boot_hi <- apply(boot_m, 2, quantile, 0.975)
dof     <- nrow(panel) - N - length(vnames)

cat(sprintf("Bootstrap: %d reps válidos\n\n", nrow(boot_m)))
cat(sprintf("════ LSDVC — N=%d, T̄=%.1f, N_obs=%d, B=%d ════\n\n",
            N, T_mean, nrow(panel), nrow(boot_m)))

lbl <- c("lag(dcl_sobre_rcl_ext, 1)" = "ρ  DCL/RCL (t-1)  ",
         "primario_sobre_rcl_ext"     = "θ  Primário/RCL   ",
         "crescimento_pib_pct"        = "γ  Cresc. PIB (%) ")

for (v in vnames) {
  est <- b_lsdvc[v]; se <- boot_se[v]
  pv  <- 2 * pt(-abs(est / se), df = dof)
  sig <- ifelse(pv < 0.01, "***", ifelse(pv < 0.05, "**", ifelse(pv < 0.10, "*", "")))
  cat(sprintf("  %s  %+.4f  SE=%.4f  p=%.4f %s\n", lbl[v], est, se, pv, sig))
  cat(sprintf("    [95%% CI: %.4f, %.4f]\n", boot_lo[v], boot_hi[v]))
}

# =============================================================================
# MODEL II-B: FE + Driscoll-Kraay SEs
# Lag calculado via dplyr::lag() agrupado por estado (evita viés de feols interno)
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" MODEL II-B: FE + Driscoll-Kraay SEs\n")
cat("════════════════════════════════════════════════════════\n")

panel_dk <- read_csv("data/processed/panel_final_v5.csv", show_col_types = FALSE) %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(dcl_lag1 = dplyr::lag(dcl_sobre_rcl_ext, 1)) %>%
  ungroup() %>%
  filter(!is.na(dcl_sobre_rcl_ext), !is.na(primario_sobre_rcl_ext),
         !is.na(crescimento_pib_pct), !is.na(dcl_lag1))

m2_dk <- feols(
  dcl_sobre_rcl_ext ~ dcl_lag1 + primario_sobre_rcl_ext + crescimento_pib_pct |
    uf + year,
  data = panel_dk, panel.id = ~uf + year, vcov = "DK"
)
print(summary(m2_dk))
co <- coef(m2_dk); se <- se(m2_dk); pv <- pvalue(m2_dk)
cat(sprintf("\n  ρ DCL/RCL(t-1)   : %+.4f  SE=%.4f  p=%.4f\n",
            co["dcl_lag1"], se["dcl_lag1"], pv["dcl_lag1"]))
cat(sprintf("  θ Primário/RCL   : %+.4f  SE=%.4f  p=%.4f\n",
            co["primario_sobre_rcl_ext"], se["primario_sobre_rcl_ext"],
            pv["primario_sobre_rcl_ext"]))
cat(sprintf("  γ Cresc. PIB (%%) : %+.4f  SE=%.4f  p=%.4f\n",
            co["crescimento_pib_pct"], se["crescimento_pib_pct"],
            pv["crescimento_pib_pct"]))
cat(sprintf("  Within R²        : %.4f | N_obs: %d\n", r2(m2_dk, "wr2"), m2_dk$nobs))
