# =============================================================================
# model2_lsdvc.R — LSDVC manual implementation
# Within (LSDV) via plm + Nickell bias correction + block-bootstrap SEs
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(plm)
})
set.seed(2025)

panel <- read_csv("output/panel_final_v5.csv", show_col_types=FALSE) %>%
  arrange(uf, year) %>%
  filter(!is.na(dcl_sobre_rcl_ext),
         !is.na(primario_sobre_rcl_ext),
         !is.na(crescimento_pib_pct))

panel_p <- pdata.frame(panel, index=c("uf","year"))

N    <- n_distinct(panel$uf)
T_df <- nrow(panel) / N   # balanced: 488/25 ≈ 19.5; use actual T per state below

cat(sprintf("Sample: N=%d states | N_obs=%d\n\n", N, nrow(panel)))

# =============================================================================
# Step 1: Within (LSDV) using plm — lag handled internally by pdata.frame
# =============================================================================
m_within <- plm(
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
    primario_sobre_rcl_ext + crescimento_pib_pct,
  data   = panel_p,
  model  = "within",
  effect = "individual"
)

b_w   <- coef(m_within)
se_w  <- sqrt(diag(vcovHC(m_within, method="arellano", type="HC1")))
t_w   <- b_w / se_w
p_w   <- 2 * pt(-abs(t_w), df = df.residual(m_within))

cat("Within (LSDV) — Nickell-biased:\n")
for (v in names(b_w))
  cat(sprintf("  %-38s %+.4f  SE=%.4f  p=%.4f\n", v, b_w[v], se_w[v], p_w[v]))

# =============================================================================
# Step 2: AB-GMM ρ for bias correction
# =============================================================================
m_ab <- suppressWarnings(pgmm(
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
    primario_sobre_rcl_ext + crescimento_pib_pct |
    lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
  data=panel_p, effect="individual", model="onestep",
  transformation="d", collapse=TRUE))

s_ab   <- summary(m_ab, robust=TRUE)
co_ab  <- coef(s_ab)
rho_AB <- co_ab["lag(dcl_sobre_rcl_ext, 1)", "Estimate"]
cat(sprintf("\nAB-GMM ρ_initial = %.4f\n", rho_AB))

# Per-state T (use minimum T with valid obs for conservative correction)
T_by_state <- panel %>% count(uf) %>% pull(n)
T_mean     <- mean(T_by_state)

# Nickell (1981) 1st-order bias: E[ρ̂_LSDV - ρ] ≈ -(1+ρ)/(T-1)
# Therefore: ρ_corrected = ρ̂_LSDV - bias = ρ̂_LSDV + (1+ρ_AB)/(T-1)
nickell    <- (1 + rho_AB) / (T_mean - 1)
cat(sprintf("Nickell bias = (1+%.4f)/(%.1f-1) = +%.4f  (LSDV is downward-biased → add)\n\n",
            rho_AB, T_mean, nickell))

b_lsdvc        <- b_w
b_lsdvc["lag(dcl_sobre_rcl_ext, 1)"] <- b_w["lag(dcl_sobre_rcl_ext, 1)"] + nickell

# =============================================================================
# Step 3: Block bootstrap SEs (resample states)
# =============================================================================
B      <- 500
states <- unique(panel$uf)
vnames <- names(b_lsdvc)
boot_m <- matrix(NA_real_, B, length(vnames), dimnames=list(NULL, vnames))

cat("Block bootstrap (B=500)...\n")
for (b in seq_len(B)) {
  s_b  <- sample(states, N, replace=TRUE)
  df_b <- lapply(seq_along(s_b), function(i)
    panel[panel$uf == s_b[i], ] %>% mutate(uf = paste0("g", i))
  ) %>% bind_rows()

  pp_b <- pdata.frame(df_b, index=c("uf","year"))

  fit_b <- tryCatch(
    plm(dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
          primario_sobre_rcl_ext + crescimento_pib_pct,
        data=pp_b, model="within", effect="individual"),
    error=function(e) NULL)
  if (is.null(fit_b) || length(coef(fit_b)) != length(vnames)) next

  rho_b <- tryCatch({
    ab_b <- suppressWarnings(pgmm(
      dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
        primario_sobre_rcl_ext + crescimento_pib_pct |
        lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
      data=pp_b, effect="individual", model="onestep",
      transformation="d", collapse=TRUE))
    coef(ab_b)["lag(dcl_sobre_rcl_ext, 1)"]
  }, error=function(e) rho_AB)

  nb_b <- (1 + rho_b) / (T_mean - 1)
  cb   <- coef(fit_b)
  cb["lag(dcl_sobre_rcl_ext, 1)"] <- cb["lag(dcl_sobre_rcl_ext, 1)"] + nb_b
  boot_m[b, ] <- cb
}

boot_m  <- boot_m[complete.cases(boot_m), ]
boot_se <- apply(boot_m, 2, sd)
boot_lo <- apply(boot_m, 2, quantile, 0.025)
boot_hi <- apply(boot_m, 2, quantile, 0.975)
cat(sprintf("Bootstrap done: %d valid reps\n\n", nrow(boot_m)))

# =============================================================================
# Results
# =============================================================================
cat("════════════════════════════════════════════════════════\n")
cat(" LSDVC (Kiviet/Bruno), initial=AB, 1st-order correction\n")
cat(sprintf(" N=%d, T̄=%.1f, N_obs=%d, Bootstrap B=%d\n",
            N, T_mean, nrow(panel), nrow(boot_m)))
cat("════════════════════════════════════════════════════════\n\n")

lbl <- c("lag(dcl_sobre_rcl_ext, 1)"="ρ  DCL/RCL (t−1)    ",
         primario_sobre_rcl_ext     ="θ  Primário/RCL     ",
         crescimento_pib_pct        ="γ  Crescimento PIB  ")

dof <- nrow(panel) - N - length(vnames)
for (v in vnames) {
  est  <- b_lsdvc[v]
  se   <- boot_se[v]
  tv   <- est / se
  pv   <- 2 * pt(-abs(tv), df=dof)
  sig  <- ifelse(pv<0.01,"***",ifelse(pv<0.05,"**",ifelse(pv<0.10,"*","")))
  cat(sprintf("  %s  %+.4f  SE=%.4f  t=%6.3f  p=%.4f %s\n",
              lbl[v], est, se, tv, pv, sig))
  cat(sprintf("    [95%% CI: %.4f, %.4f]\n", boot_lo[v], boot_hi[v]))
}

cat("\n────────────────────────────────────────────────────────\n")
cat(" Comparison: θ (primario_sobre_rcl_ext)\n")
cat("────────────────────────────────────────────────────────\n")
cat(sprintf("  LSDV/Within (biased)  : %+.4f  p=%.4f\n",
            b_w["primario_sobre_rcl_ext"], p_w["primario_sobre_rcl_ext"]))
cat(sprintf("  AB-GMM (diff, ind FE) : %+.4f  p=%.4f\n",
            co_ab["primario_sobre_rcl_ext","Estimate"],
            co_ab["primario_sobre_rcl_ext","Pr(>|z|)"]))
cat(sprintf("  LSDVC (this model)    : %+.4f  p=%.4f %s\n",
            b_lsdvc["primario_sobre_rcl_ext"],
            2*pt(-abs(b_lsdvc["primario_sobre_rcl_ext"]/
                        boot_se["primario_sobre_rcl_ext"]), df=dof),
            ifelse(2*pt(-abs(b_lsdvc["primario_sobre_rcl_ext"]/
                               boot_se["primario_sobre_rcl_ext"]),df=dof)<0.10,"*","")))

cat(sprintf("\n  ρ comparison (Nickell downward bias check):\n"))
cat(sprintf("  LSDV ρ = %.4f → LSDVC ρ = %.4f (+%.4f correction) → AB ρ = %.4f\n",
            b_w["lag(dcl_sobre_rcl_ext, 1)"],
            b_lsdvc["lag(dcl_sobre_rcl_ext, 1)"],
            nickell, rho_AB))
