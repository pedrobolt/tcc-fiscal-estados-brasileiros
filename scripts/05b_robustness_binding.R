# =============================================================================
# scripts/05b_robustness_binding.R
# ROB3 — Heterogeneidade pelo status binding DEFASADO (t-1)
#
# HIPOTESE: o efeito disciplinador do teto sobre o ajuste fiscal e maior
# quando o estado ja estava proximo do limite no ano anterior (binding_{t-1}).
#
# DEFINICAO DE BINDING:
#   binding_it   = 1 se (encargos_sobre_rcl_ext_it) / (teto_i/100) > 0.70
#   binding_lag1 = lag(binding, 1) por estado — STATUS NO ANO ANTERIOR
#   binding_lag2 = lag(binding, 2) por estado — para instrumento
#
#   encargos_sobre_rcl_ext: fracao decimal (ex: 0.09 = 9% da RCL)
#   teto: pontos percentuais (12, 13 ou 15) → teto/100 converte para fracao
#   Limiar: encargos >= 70% DO TETO CONTRATUAL (nao 70% da RCL)
#
# Correcao vs versao anterior: binding agora e defasado para evitar
# endogeneidade do binding contemporaneo com o resultado fiscal.
#
# Entrada: data/processed/panel_slim.csv
# =============================================================================

pkgs <- c("dplyr", "readr", "fixest")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

panel <- read_csv("data/processed/panel_slim.csv", show_col_types = FALSE) %>%
  arrange(uf, year)

# =============================================================================
# 1. Construir binding contemporaneo e seus lags
# =============================================================================
panel <- panel %>%
  group_by(uf) %>%
  mutate(
    teto_frac    = teto / 100,

    # binding_it: 1 se encargos >= 70% do teto (contemporaneo)
    # Quando encargos e NA, trata como nao-binding (0)
    binding      = as.integer(
      !is.na(encargos_sobre_rcl_ext) &
      !is.na(teto_frac) &
      (encargos_sobre_rcl_ext / teto_frac) > 0.70
    ),

    # Lags do binding — defasados 1 e 2 periodos dentro de cada estado
    binding_lag1 = dplyr::lag(binding, 1),   # status NO ANO ANTERIOR
    binding_lag2 = dplyr::lag(binding, 2)    # status DOIS ANOS ANTES (instrumento)
  ) %>%
  ungroup() %>%
  mutate(
    # Interacoes com binding defasado (endogenas e instrumentos)
    d_lag1_teto_bind = d_lag1 * teto * binding_lag1,   # endogena
    d_lag2_teto_bind = d_lag2 * teto * binding_lag2    # instrumento
  )

# =============================================================================
# 2. Diagnostico: distribuicao do binding_lag1
# =============================================================================
cat("================================================================\n")
cat(" ROB3 — Diagnostico: binding_{t-1}\n")
cat("================================================================\n\n")

cat("ESCALA:\n")
cat("  encargos_sobre_rcl_ext: fracao decimal (ex: 0.09 = 9%% da RCL)\n")
cat("  teto: pontos percentuais (12, 13, 15)\n")
cat("  binding = 1 se encargos/(teto/100) > 0.70\n")
cat("  binding_lag1 = lag(binding, 1) por estado\n\n")

ratio_vals <- panel$encargos_sobre_rcl_ext / panel$teto_frac
ratio_vals <- ratio_vals[!is.na(ratio_vals)]
cat(sprintf(
  "  Ratio encargos/(teto/100) [limiar=0.70]:\n"
))
cat(sprintf(
  "  Min=%.3f | p25=%.3f | Median=%.3f | Mean=%.3f | p75=%.3f | p90=%.3f | Max=%.3f\n\n",
  min(ratio_vals), quantile(ratio_vals,.25), median(ratio_vals),
  mean(ratio_vals), quantile(ratio_vals,.75), quantile(ratio_vals,.90), max(ratio_vals)
))

cat("--- Binding CONTEMPORANEO (binding_it) ---\n")
tot_bind <- sum(panel$binding, na.rm = TRUE)
tot_obs  <- sum(!is.na(panel$encargos_sobre_rcl_ext))
cat(sprintf("  Total binding=1 : %d / %d obs (%.1f%%)\n\n",
            tot_bind, tot_obs, 100 * tot_bind / tot_obs))

cat("--- Binding DEFASADO (binding_lag1) ---\n")
bind_l1 <- panel$binding_lag1[!is.na(panel$binding_lag1)]
cat(sprintf("  binding_lag1 = 0 : %d obs\n", sum(bind_l1 == 0)))
cat(sprintf("  binding_lag1 = 1 : %d obs\n", sum(bind_l1 == 1)))
cat(sprintf("  binding_lag1 = NA: %d obs (primeiro ano de cada estado)\n\n",
            sum(is.na(panel$binding_lag1))))

cat("--- table(uf, binding_lag1) — anos com binding_lag1=1 por estado ---\n")
bind_tbl <- panel %>%
  filter(!is.na(binding_lag1)) %>%
  group_by(uf) %>%
  summarise(
    n_total  = n(),
    n_bind1  = sum(binding_lag1 == 1),
    pct_bind = round(100 * n_bind1 / n_total, 1),
    anos_bind = paste(year[binding_lag1 == 1], collapse = ","),
    .groups  = "drop"
  ) %>%
  arrange(desc(n_bind1))

cat(sprintf("  %-4s  %8s  %8s  %8s  %s\n",
            "UF", "n_total", "n_bind1", "pct", "anos_binding_lag1"))
cat(strrep("-", 72), "\n")
for (i in seq_len(nrow(bind_tbl))) {
  r <- bind_tbl[i, ]
  cat(sprintf("  %-4s  %8d  %8d  %7.1f%%  %s\n",
              r$uf, r$n_total, r$n_bind1, r$pct_bind,
              if (r$n_bind1 == 0) "—" else r$anos_bind))
}
total_bind1_obs <- sum(bind_tbl$n_bind1)
cat(sprintf("\n  TOTAL estados com binding_lag1=1 em algum ano : %d\n",
            sum(bind_tbl$n_bind1 > 0)))
cat(sprintf("  TOTAL obs com binding_lag1=1                  : %d\n\n",
            total_bind1_obs))

if (total_bind1_obs < 30) {
  cat("  AVISO: binding_lag1=1 em menos de 30 obs.\n")
  cat("  Variacao insuficiente — B3 pode ser impreciso ou nao identificado.\n\n")
}

# =============================================================================
# 3. Amostra efetiva do modelo
# =============================================================================
panel_rob3 <- panel %>%
  filter(
    !is.na(primario_sobre_rcl_ext),
    !is.na(d_lag1),           !is.na(d_lag2),
    !is.na(d_lag1_teto),      !is.na(d_lag2_teto),
    !is.na(binding_lag1),     !is.na(binding_lag2),
    !is.na(d_lag1_teto_bind), !is.na(d_lag2_teto_bind),
    !is.na(yvar)
  )

cat(sprintf("Amostra efetiva: %d obs | %d estados | anos %d-%d\n",
            nrow(panel_rob3), n_distinct(panel_rob3$uf),
            min(panel_rob3$year), max(panel_rob3$year)))
n1_eff <- sum(panel_rob3$binding_lag1 == 1)
n0_eff <- sum(panel_rob3$binding_lag1 == 0)
cat(sprintf("  binding_lag1 na amostra: 0=%d | 1=%d (%.1f%%)\n\n",
            n0_eff, n1_eff, 100 * n1_eff / (n0_eff + n1_eff)))

# =============================================================================
# 4. Baseline na mesma amostra (para comparacao justa)
# =============================================================================
cat("================================================================\n")
cat(" BASELINE — Modelo I-B sem binding (mesma amostra do ROB3)\n")
cat("================================================================\n\n")

m_base <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
  data    = panel_rob3,
  cluster = ~uf
)

co_b <- coef(m_base); se_b <- se(m_base); pv_b <- pvalue(m_base)
mk   <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))

cat(sprintf("  B1 DCL/RCL(t-1)  : %+.4f  SE=%.4f  p=%.4f %s\n",
            co_b["fit_d_lag1"],      se_b["fit_d_lag1"],
            pv_b["fit_d_lag1"],      mk(pv_b["fit_d_lag1"])))
cat(sprintf("  B2 DCL x Teto    : %+.4f  SE=%.4f  p=%.4f %s\n",
            co_b["fit_d_lag1_teto"], se_b["fit_d_lag1_teto"],
            pv_b["fit_d_lag1_teto"], mk(pv_b["fit_d_lag1_teto"])))
cat(sprintf("  N obs            : %d\n\n", m_base$nobs))

# =============================================================================
# 5. ROB3: 2SLS com tripla interacao e binding defasado
# =============================================================================
cat("================================================================\n")
cat(" ROB3 — 2SLS com d_lag1 x teto x binding_{t-1}\n")
cat(" Endogenas  : d_lag1, d_lag1_teto, d_lag1_teto_bind\n")
cat(" Instrumentos: d_lag2, d_lag2_teto, d_lag2_teto_bind\n")
cat("================================================================\n\n")

m_rob3 <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_teto + d_lag1_teto_bind ~
    d_lag2 + d_lag2_teto + d_lag2_teto_bind,
  data    = panel_rob3,
  cluster = ~uf
)

print(summary(m_rob3, stage = 1:2))

cat("\n--- Diagnosticos do 1o estagio ---\n")
print(fitstat(m_rob3, ~ivf + wh))

# =============================================================================
# 6. Resultados
# =============================================================================
cat("\n================================================================\n")
cat(" RESULTADOS ROB3\n")
cat("================================================================\n\n")

co3 <- coef(m_rob3); se3 <- se(m_rob3); pv3 <- pvalue(m_rob3)

b1 <- co3["fit_d_lag1"];           p1 <- pv3["fit_d_lag1"]
b2 <- co3["fit_d_lag1_teto"];      p2 <- pv3["fit_d_lag1_teto"]
b3 <- co3["fit_d_lag1_teto_bind"]; p3 <- pv3["fit_d_lag1_teto_bind"]

cat(sprintf("  B1  DCL/RCL (t-1)              : %+.4f  SE=%.4f  p=%.4f %s\n",
            b1, se3["fit_d_lag1"], p1, mk(p1)))
cat(sprintf("  B2  DCL/RCL x Teto             : %+.4f  SE=%.4f  p=%.4f %s\n",
            b2, se3["fit_d_lag1_teto"], p2, mk(p2)))
cat(sprintf("  B3  DCL/RCL x Teto x Bind(t-1) : %+.4f  SE=%.4f  p=%.4f %s\n",
            b3, se3["fit_d_lag1_teto_bind"], p3, mk(p3)))

fstats <- tryCatch(fitstat(m_rob3, "ivf"), error = function(e) NULL)
if (!is.null(fstats)) {
  fs_vals <- sapply(fstats, function(x) x$stat)
  cat(sprintf("\n  F-stat 1o estagio (limiar=10):\n"))
  nms <- c("d_lag1", "d_lag1_teto", "d_lag1_teto_bind")
  for (i in seq_along(fs_vals)) {
    flag <- if (!is.na(fs_vals[i]) && fs_vals[i] < 10)
              " *** INSTRUMENTO FRACO ***" else ""
    cat(sprintf("    %-24s : %.1f%s\n", nms[i], fs_vals[i], flag))
  }
}

cat(sprintf("\n  N obs                          : %d\n", m_rob3$nobs))

# =============================================================================
# 7. Comparacao baseline vs ROB3
# =============================================================================
cat("\n================================================================\n")
cat(" COMPARACAO BASELINE vs ROB3\n")
cat("================================================================\n\n")

cat(sprintf("  %-32s  %+8s  %+8s  %+8s\n", "Modelo", "B1", "B2", "B3"))
cat(strrep("-", 62), "\n")
cat(sprintf("  %-32s  %+8.4f  %+8.4f  %8s\n",
            "Baseline (sem binding)",
            co_b["fit_d_lag1"], co_b["fit_d_lag1_teto"], "—"))
cat(sprintf("  %-32s  %+8.4f  %+8.4f  %+8.4f\n",
            "ROB3 (binding_{t-1})", b1, b2, b3))
cat(sprintf("  %-32s  %+8.4f  %+8.4f  %8s\n",
            "Variacao B1 e B2",
            b1 - co_b["fit_d_lag1"],
            b2 - co_b["fit_d_lag1_teto"], "—"))

delta_b1 <- abs(b1 - co_b["fit_d_lag1"])
delta_b2 <- abs(b2 - co_b["fit_d_lag1_teto"])
if (delta_b1 > 0.05 || delta_b2 > 0.01) {
  cat("\n  AVISO: B1 ou B2 mudaram significativamente — verificar multicolinearidade.\n")
} else {
  cat("\n  B1 e B2 estao estaveis em relacao ao baseline.\n")
}

# =============================================================================
# 8. Interpretacao de B3
# =============================================================================
cat("\n--- Interpretacao de B3 ---\n")

if (is.na(b3) || is.na(p3)) {
  cat("  B3 nao estimado — sem variacao suficiente em binding_lag1.\n")
} else if (p3 >= 0.10) {
  cat(sprintf("  B3 = %+.4f (p=%.3f): nao significativo.\n", b3, p3))
  cat("  O efeito do teto independe do status binding defasado.\n")
  cat("  Resultado principal (B2) permanece valido.\n")
} else if (b3 < 0) {
  cat(sprintf("  B3 = %+.4f%s (p=%.3f): significativo e negativo.\n",
              b3, mk(p3), p3))
  cat("  Efeito disciplinador do teto e MAIS FORTE quando binding_{t-1}=1.\n")
  cat("  Resultado principal ROBUSTECIDO.\n")
} else {
  cat(sprintf("  B3 = %+.4f%s (p=%.3f): significativo e positivo.\n",
              b3, mk(p3), p3))
  cat("  Efeito do teto e MENOR quando binding_{t-1}=1.\n")
  cat("  Verificar multicolinearidade entre interacoes.\n")
}

cat("================================================================\n")
