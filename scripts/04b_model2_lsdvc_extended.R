# =============================================================================
# scripts/04b_model2_lsdvc_extended.R
# Modelo II — LSDVC com controles adicionais
#
#   II-C: + ipca_aa  (inflação nacional como controle macroeconômico)
#   II-D: + ipca_aa + encargos_sobre_rcl_ext  (verificação de estabilidade)
#
# xtlvr2::lsdvc() indisponível no CRAN para esta versão do R.
# Implementação manual equivalente ao script 04:
#   Within → AB-GMM ρ inicial → correção Nickell O(1/T) → block bootstrap.
#
# Baseline (script 04): ρ = 0.988***, θ = −0.937***
# Entrada : data/processed/panel_final_v5.csv  (tem ipca_aa, encargos)
# =============================================================================

pkgs <- c("dplyr", "readr", "plm")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

set.seed(2025)

BASE_rho   <-  0.988
BASE_theta <- -0.937

# =============================================================================
# Função central: LSDVC manual + block bootstrap
# =============================================================================
run_lsdvc <- function(panel, formula_str, vars_needed, B = 500) {

  panel_clean <- panel %>%
    filter(if_all(all_of(vars_needed), ~ !is.na(.x))) %>%
    arrange(uf, year)

  N      <- n_distinct(panel_clean$uf)
  T_bar  <- nrow(panel_clean) / N
  states <- unique(panel_clean$uf)
  panel_p <- pdata.frame(panel_clean, index = c("uf", "year"))

  fml <- as.formula(formula_str)

  # Step 1 — Within (LSDV)
  m_w <- plm(fml, data = panel_p, model = "within", effect = "individual")
  b_w <- coef(m_w)
  k   <- length(b_w)

  # Step 2 — AB-GMM ρ inicial para correção de Nickell
  dep_var  <- all.vars(fml)[1]
  lag_term <- paste0("lag(", dep_var, ", 1)")
  other_vars <- setdiff(all.vars(fml)[-1], dep_var)

  fml_gmm <- as.formula(paste(
    dep_var, "~", lag_term, "+",
    paste(other_vars, collapse = " + "),
    "| lag(", dep_var, ", 2:3) +",
    paste(paste0("lag(", other_vars[1], ", 2:3)"))
  ))

  m_ab <- tryCatch(
    suppressWarnings(pgmm(fml_gmm, data = panel_p,
                          effect = "individual", model = "onestep",
                          transformation = "d", collapse = TRUE)),
    error = function(e) NULL
  )

  rho_AB <- if (!is.null(m_ab)) {
    tryCatch(
      coef(summary(m_ab, robust = TRUE))[lag_term, "Estimate"],
      error = function(e) b_w[1]
    )
  } else {
    cat("  AVISO: AB-GMM falhou; usando Within ρ como inicial.\n")
    b_w[1]
  }

  nickell    <- (1 + rho_AB) / (T_bar - 1)
  b_lsdvc    <- b_w
  b_lsdvc[1] <- b_w[1] + nickell

  cat(sprintf("  AB-GMM ρ_inicial=%.4f | Nickell correction=+%.4f\n",
              rho_AB, nickell))

  # Step 3 — Block bootstrap (resample states)
  vnames <- names(b_lsdvc)
  boot_m <- matrix(NA_real_, B, k, dimnames = list(NULL, vnames))

  cat(sprintf("  Block bootstrap (B=%d, N=%d, T̄=%.1f, N_obs=%d)...\n",
              B, N, T_bar, nrow(panel_clean)))

  for (b in seq_len(B)) {
    s_b  <- sample(states, N, replace = TRUE)
    df_b <- lapply(seq_along(s_b), function(i)
      panel_clean[panel_clean$uf == s_b[i], ] %>%
        mutate(uf = paste0("g", i))
    ) %>% bind_rows()
    pp_b <- pdata.frame(df_b, index = c("uf", "year"))

    ft_b <- tryCatch(
      plm(fml, data = pp_b, model = "within", effect = "individual"),
      error = function(e) NULL
    )
    if (is.null(ft_b) || length(coef(ft_b)) != k) next

    rho_b <- tryCatch({
      ab_b <- suppressWarnings(pgmm(fml_gmm, data = pp_b,
                                    effect = "individual", model = "onestep",
                                    transformation = "d", collapse = TRUE))
      coef(ab_b)[lag_term]
    }, error = function(e) rho_AB)

    cb    <- coef(ft_b)
    cb[1] <- cb[1] + (1 + rho_b) / (T_bar - 1)
    boot_m[b, ] <- cb
  }

  boot_m  <- boot_m[complete.cases(boot_m), ]
  boot_se <- apply(boot_m, 2, sd)
  boot_lo <- apply(boot_m, 2, quantile, 0.025)
  boot_hi <- apply(boot_m, 2, quantile, 0.975)
  dof     <- nrow(panel_clean) - N - k

  cat(sprintf("  Bootstrap: %d reps válidos\n", nrow(boot_m)))

  list(
    coef    = b_lsdvc,
    se      = boot_se,
    lo      = boot_lo,
    hi      = boot_hi,
    dof     = dof,
    N       = N,
    T_bar   = T_bar,
    N_obs   = nrow(panel_clean),
    B_valid = nrow(boot_m)
  )
}

# =============================================================================
# Função de relatório
# =============================================================================
report_lsdvc <- function(res, label, base_rho, base_theta) {
  co  <- res$coef; se <- res$se; lo <- res$lo; hi <- res$hi
  pv  <- 2 * pt(-abs(co / se), df = res$dof)
  sig <- ifelse(pv < 0.01, "***", ifelse(pv < 0.05, "**",
           ifelse(pv < 0.10, "*", "   ")))

  lbl <- c(
    "lag(dcl_sobre_rcl_ext, 1)" = "ρ  DCL/RCL (t-1)          ",
    "primario_sobre_rcl_ext"    = "θ  Primário/RCL            ",
    "crescimento_pib_pct"       = "γ  Cresc. PIB (%)          ",
    "ipca_aa"                   = "π  IPCA (% a.a.)           ",
    "encargos_sobre_rcl_ext"    = "δ  Encargos/RCL            "
  )

  cat(sprintf("\n════════════════════════════════════════════════════════\n"))
  cat(sprintf(" %s\n", label))
  cat(sprintf(" N=%d | T̄=%.1f | N_obs=%d | B_valid=%d\n",
              res$N, res$T_bar, res$N_obs, res$B_valid))
  cat(sprintf("════════════════════════════════════════════════════════\n\n"))

  for (v in names(co)) {
    nm <- if (v %in% names(lbl)) lbl[[v]] else sprintf("%-27s", v)
    cat(sprintf("  %s  %+7.4f  SE=%6.4f  p=%.4f %s\n",
                nm, co[v], se[v], pv[v], sig[v]))
    cat(sprintf("    [95%% CI: %+.4f, %+.4f]\n", lo[v], hi[v]))
  }

  # Identificar ρ e θ
  rho_idx   <- grep("lag\\(dcl_sobre_rcl_ext", names(co))[1]
  theta_idx <- grep("primario_sobre_rcl_ext",   names(co))[1]
  rho   <- co[rho_idx];   rho_p   <- pv[rho_idx]
  theta <- co[theta_idx]; theta_p <- pv[theta_idx]

  cat(sprintf("\n  ── Comparação com baseline ──────────────────────────\n"))
  cat(sprintf("  ρ    %+.4f (p=%.4f) | baseline %+.3f | Δ=%+.4f\n",
              rho,   rho_p,   base_rho,   rho   - base_rho))
  cat(sprintf("  θ    %+.4f (p=%.4f) | baseline %+.3f | Δ=%+.4f\n",
              theta, theta_p, base_theta, theta - base_theta))

  # Flags de implausibilidade e instabilidade
  flags <- character(0)
  if (!is.na(rho) && (rho >= 1 || rho <= 0))
    flags <- c(flags, sprintf("INSTÁVEL: ρ=%.4f fora de (0,1) — divergência dinâmica", rho))
  if (!is.na(rho) && abs(rho - base_rho) > 0.10)
    flags <- c(flags, sprintf("DESVIO: ρ diverge %.3fpp do baseline (limiar: 10pp)", abs(rho - base_rho)))
  if (!is.na(theta) && theta > 0)
    flags <- c(flags, sprintf("IMPLAUSÍVEL: θ=%.4f > 0 — superávit aumentaria dívida", theta))
  if (!is.na(theta) && abs(theta - base_theta) > 0.30)
    flags <- c(flags, sprintf("DESVIO: θ diverge %.3fpp do baseline (limiar: 30pp)", abs(theta - base_theta)))
  if (res$B_valid < 400)
    flags <- c(flags, sprintf("BOOTSTRAP: apenas %d/500 reps válidos (< 80%%)", res$B_valid))

  cat("\n  ── Diagnósticos ─────────────────────────────────────\n")
  if (length(flags) == 0) {
    cat("  OK: nenhum sinal de instabilidade ou coeficiente implausível.\n")
  } else {
    for (f in flags) cat(sprintf("  ⚠  %s\n", f))
  }

  invisible(list(rho = rho, theta = theta, pv_rho = rho_p, pv_theta = theta_p,
                 co = co, se = se, pv = pv))
}

# =============================================================================
# Carregar dados
# =============================================================================
panel_raw <- read_csv("data/processed/panel_final_v5.csv",
                      show_col_types = FALSE) %>%
  arrange(uf, year)

cat(sprintf("Painel bruto: %d obs | %d estados | %d anos\n",
            nrow(panel_raw), n_distinct(panel_raw$uf), n_distinct(panel_raw$year)))

# Cobertura do IPCA
ipca_miss <- sum(is.na(panel_raw$ipca_aa))
cat(sprintf("ipca_aa: %d NAs de %d obs\n\n", ipca_miss, nrow(panel_raw)))

# Fórmulas — lag da dep. var. incluído explicitamente para plm::Within
FORMULA_C <- paste(
  "dcl_sobre_rcl_ext ~",
  "lag(dcl_sobre_rcl_ext, 1) + primario_sobre_rcl_ext +",
  "crescimento_pib_pct + ipca_aa"
)

FORMULA_D <- paste(
  "dcl_sobre_rcl_ext ~",
  "lag(dcl_sobre_rcl_ext, 1) + primario_sobre_rcl_ext +",
  "crescimento_pib_pct + ipca_aa + encargos_sobre_rcl_ext"
)

VARS_C <- c("dcl_sobre_rcl_ext", "primario_sobre_rcl_ext",
            "crescimento_pib_pct", "ipca_aa")

VARS_D <- c(VARS_C, "encargos_sobre_rcl_ext")

# =============================================================================
# MODEL II-C: LSDVC + IPCA
# =============================================================================
cat("════════════════════════════════════════════════════════\n")
cat(" Estimando MODEL II-C (+IPCA)...\n")
cat("════════════════════════════════════════════════════════\n")

res_c <- run_lsdvc(panel_raw, FORMULA_C, VARS_C, B = 500)
r_c   <- report_lsdvc(res_c, "MODEL II-C  LSDVC + IPCA", BASE_rho, BASE_theta)

# =============================================================================
# MODEL II-D: LSDVC + IPCA + encargos
# Nota: encargos_sobre_rcl_ext pode ser endógeno à dívida (juros ~ DCL).
# Instabilidade esperada se colinearidade alta.
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" Estimando MODEL II-D (+IPCA +encargos)...\n")
cat("════════════════════════════════════════════════════════\n")

res_d <- run_lsdvc(panel_raw, FORMULA_D, VARS_D, B = 500)
r_d   <- report_lsdvc(res_d, "MODEL II-D  LSDVC + IPCA + encargos", BASE_rho, BASE_theta)

# Diagnóstico específico: encargos
enc_co <- res_d$coef["encargos_sobre_rcl_ext"]
enc_se <- res_d$se["encargos_sobre_rcl_ext"]
enc_pv <- 2 * pt(-abs(enc_co / enc_se), df = res_d$dof)
cat(sprintf("\n  Coeficiente δ (encargos): %+.4f  SE=%.4f  p=%.4f\n",
            enc_co, enc_se, enc_pv))
if (enc_pv >= 0.10)
  cat("  ℹ  δ não significativo — possível colinearidade encargos↔DCL.\n")
if (!is.na(enc_co) && enc_co > 0 && enc_pv < 0.10)
  cat("  ℹ  δ > 0 e significativo — consistente com dinâmica de dívida.\n")

# =============================================================================
# RESUMO COMPARATIVO
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" RESUMO COMPARATIVO\n")
cat("════════════════════════════════════════════════════════\n\n")

cat(sprintf("  %-30s  %8s  %6s  %8s  %6s\n",
            "Modelo", "ρ", "p(ρ)", "θ", "p(θ)"))
cat(sprintf("  %s\n", strrep("-", 72)))
cat(sprintf("  %-30s  %+8.4f  %6s  %+8.4f  %6s\n",
            "Baseline (II-A, script 04)", BASE_rho, "—", BASE_theta, "—"))
cat(sprintf("  %-30s  %+8.4f  %6.4f  %+8.4f  %6.4f\n",
            "II-C  (+IPCA)", r_c$rho, r_c$pv_rho, r_c$theta, r_c$pv_theta))
cat(sprintf("  %-30s  %+8.4f  %6.4f  %+8.4f  %6.4f\n",
            "II-D  (+IPCA +encargos)", r_d$rho, r_d$pv_rho, r_d$theta, r_d$pv_theta))

cat("\n")
cat("Nota: xtlvr2::lsdvc() não disponível no CRAN para esta versão do R.\n")
cat("Equivalente manual: Within + AB-GMM ρ_inicial + Nickell O(1/T) +\n")
cat("block bootstrap por estado (B=500).\n")
