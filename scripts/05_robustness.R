# =============================================================================
# scripts/05_robustness.R
# Verificações de robustez + testes de raiz unitária em painel
#   ROB1: Modelo II com hiato do produto (yvar) no lugar do crescimento do PIB
#   ROB2: Modelo I-B (2SLS) excluindo anos da pandemia (2020-2021)
#   UNIT ROOT: IPS (Im-Pesaran-Shin), LLC (Levin-Lin-Chu), Hadri
# Entrada: data/processed/panel_slim.csv
# =============================================================================

pkgs <- c("dplyr", "readr", "fixest", "plm")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

set.seed(2025)

panel <- read_csv("data/processed/panel_slim.csv", show_col_types = FALSE) %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(dcl_lag1 = dplyr::lag(dcl_sobre_rcl_ext, 1)) %>%
  ungroup()

BASE_theta <- -0.937; BASE_rho <- 0.988
BASE_b1    <-  0.711; BASE_b2  <- -0.046

# =============================================================================
# ROB1: LSDVC com yvar (hiato do produto)
# Resultados esperados: rho=0.965***, theta=-0.942***
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat(" ROB1 — Modelo II: yvar (hiato) substitui crescimento do PIB\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

panel_l1 <- panel %>%
  filter(!is.na(dcl_sobre_rcl_ext), !is.na(primario_sobre_rcl_ext), !is.na(yvar))
panel_p1 <- pdata.frame(panel_l1, index = c("uf", "year"))
N1       <- n_distinct(panel_l1$uf)
T_bar1   <- nrow(panel_l1) / N1

m_w1 <- plm(dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) +
              primario_sobre_rcl_ext + yvar,
            data = panel_p1, model = "within", effect = "individual")
b_w1 <- coef(m_w1)

m_ab1    <- suppressWarnings(pgmm(
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) + primario_sobre_rcl_ext + yvar |
    lag(dcl_sobre_rcl_ext, 2:3) + lag(primario_sobre_rcl_ext, 2:3),
  data = panel_p1, effect = "individual", model = "onestep",
  transformation = "d", collapse = TRUE))
rho_AB1  <- coef(summary(m_ab1, robust = TRUE))["lag(dcl_sobre_rcl_ext, 1)", "Estimate"]
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
      dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext, 1) + primario_sobre_rcl_ext + yvar |
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

cat(sprintf("Bootstrap: %d reps válidos\n\n", nrow(boot1)))
cat(sprintf("  rho DCL/RCL(t-1) : %+.4f  SE=%.4f  p=%.4f  [baseline: %.3f]\n",
            b_lsdvc1["lag(dcl_sobre_rcl_ext, 1)"], boot_se1["lag(dcl_sobre_rcl_ext, 1)"],
            2*pt(-abs(b_lsdvc1["lag(dcl_sobre_rcl_ext, 1)"]/boot_se1["lag(dcl_sobre_rcl_ext, 1)"]), df=dof1),
            BASE_rho))
cat(sprintf("  theta Prim/RCL   : %+.4f  SE=%.4f  p=%.4f  [baseline: %.3f]\n",
            b_lsdvc1["primario_sobre_rcl_ext"], boot_se1["primario_sobre_rcl_ext"],
            2*pt(-abs(b_lsdvc1["primario_sobre_rcl_ext"]/boot_se1["primario_sobre_rcl_ext"]), df=dof1),
            BASE_theta))
cat(sprintf("  gamma Hiato prod. : %+.4f  SE=%.4f  p=%.4f\n\n",
            b_lsdvc1["yvar"], boot_se1["yvar"],
            2*pt(-abs(b_lsdvc1["yvar"]/boot_se1["yvar"]), df=dof1)))

# =============================================================================
# ROB2: 2SLS excluindo 2020-2021
# Resultados esperados: beta1=0.751***, beta2=-0.048***
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat(" ROB2 — Modelo I-B (2SLS) excluindo anos COVID (2020-2021)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

panel_nocovid <- panel %>% filter(!year %in% c(2020, 2021))

m_rob2 <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
  data = panel_nocovid, cluster = ~uf)

print(summary(m_rob2))
print(fitstat(m_rob2, ~ivf + wh))
cat(sprintf("\n  beta1 DCL/RCL(t-1) : %+.3f  [baseline: %.3f]\n",
            coef(m_rob2)["fit_d_lag1"], BASE_b1))
cat(sprintf("  beta2 DCL x Teto   : %+.3f  [baseline: %.3f]\n\n",
            coef(m_rob2)["fit_d_lag1_teto"], BASE_b2))

# =============================================================================
# RAIZ UNITÁRIA: IPS, LLC, Hadri
# IPS: painel desbalanceado OK | LLC e Hadri: requerem painel balanceado
# Conclusão: IPS e LLC rejeitam H0 -> serie estacionaria; rho=0.988 e persistencia
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat(" TESTES DE RAIZ UNITÁRIA em dcl_sobre_rcl_ext\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

panel_ur <- read_csv("data/processed/panel_slim.csv", show_col_types = FALSE) %>%
  filter(!is.na(dcl_sobre_rcl_ext)) %>%
  arrange(uf, year) %>%
  group_by(uf) %>% filter(n() >= 10) %>% ungroup()

panel_ur_p <- pdata.frame(panel_ur, index = c("uf", "year"))

cat("IPS (Im-Pesaran-Shin) — H0: todos os paineis tem raiz unitaria\n")
ips <- purtest(dcl_sobre_rcl_ext ~ 1, data = panel_ur_p, test = "ips", lags = 1)
print(summary(ips))

complete_years <- panel_ur %>%
  group_by(year) %>%
  summarise(n_s = n_distinct(uf), .groups = "drop") %>%
  filter(n_s == n_distinct(panel_ur$uf)) %>% pull(year)
panel_bal   <- panel_ur %>% filter(year %in% complete_years)
panel_bal_p <- pdata.frame(panel_bal, index = c("uf", "year"))

cat(sprintf("\nJanela balanceada: %d-%d (%d anos, %d estados)\n",
            min(complete_years), max(complete_years),
            length(complete_years), n_distinct(panel_bal$uf)))

cat("\nLLC (Levin-Lin-Chu) — H0: processo comum de raiz unitaria\n")
llc <- purtest(dcl_sobre_rcl_ext ~ 1, data = panel_bal_p, test = "levinlin", lags = 1)
print(summary(llc))

cat("\nHadri LM — H0: todos os paineis sao estacionarios\n")
had <- purtest(dcl_sobre_rcl_ext ~ 1, data = panel_bal_p, test = "hadri", lags = 1)
print(summary(had))

ips_p <- ips$statistic$p.value
llc_p <- llc$statistic$p.value
had_p <- had$statistic$p.value

cat("\n====== RESUMO ======\n")
cat(sprintf("  IPS   p=%.4f  %s\n", ips_p, ifelse(ips_p < 0.05, "Rejeita raiz unitaria", "Nao rejeita")))
cat(sprintf("  LLC   p=%.4f  %s\n", llc_p, ifelse(llc_p < 0.05, "Rejeita raiz unitaria", "Nao rejeita")))
cat(sprintf("  Hadri p=%.4f  %s\n", had_p, ifelse(had_p > 0.05, "Nao rejeita estacionariedade", "Rejeita")))
if (ips_p < 0.05 && llc_p < 0.05)
  cat("  CONCLUSAO: serie estacionaria. rho=0.988 e persistencia, nao raiz unitaria.\n")
