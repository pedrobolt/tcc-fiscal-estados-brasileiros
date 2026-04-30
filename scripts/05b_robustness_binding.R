# =============================================================================
# scripts/05b_robustness_binding.R
# ROB3 — Teste de heterogeneidade: efeito do teto quando binding vs. nao-binding
#
# HIPOTESE: se o estado usa mais de 70% do teto permitido (binding=1), o
# efeito disciplinador do teto sobre o ajuste fiscal deveria ser MAIOR.
#
# DEFINICAO DE BINDING:
#   binding = 1 se (encargos_sobre_rcl_ext) / (teto/100) > 0.70
#   encargos_sobre_rcl_ext: fracao decimal (ex: 0.09 = 9% da RCL)
#   teto: pontos percentuais (12, 13 ou 15) convertido para fracao: teto/100
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
# 1. Construir variavel binding e interacoes
# =============================================================================
panel <- panel %>%
  mutate(
    teto_frac           = teto / 100,
    binding             = as.integer(!is.na(encargos_sobre_rcl_ext) &
                                       !is.na(teto_frac) &
                                       (encargos_sobre_rcl_ext / teto_frac) > 0.70),
    d_lag1_teto_binding = d_lag1 * teto * binding,
    d_lag2_teto_binding = d_lag2 * teto * binding
  )

# =============================================================================
# 2. Distribuicao binding por grupo de teto
# =============================================================================
cat("================================================================\n")
cat(" ROB3 - Distribuicao binding por grupo de teto\n")
cat("================================================================\n\n")

dist <- panel %>%
  filter(!is.na(encargos_sobre_rcl_ext)) %>%
  group_by(teto, binding) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(status = ifelse(binding == 1, "binding", "nao-binding"))

cat(sprintf("  %-6s  %-12s  %s\n", "Teto", "Status", "N obs"))
cat(strrep("-", 32), "\n")
for (i in seq_len(nrow(dist)))
  cat(sprintf("  %-6s  %-12s  %d\n",
              paste0(dist$teto[i], "%"), dist$status[i], dist$n[i]))

total_binding <- sum(panel$binding, na.rm = TRUE)
total_obs     <- sum(!is.na(panel$encargos_sobre_rcl_ext))
cat(sprintf("\n  Total binding: %d / %d obs (%.1f%%)\n",
            total_binding, total_obs, 100 * total_binding / total_obs))

ratio_vals <- with(panel, encargos_sobre_rcl_ext / teto_frac)
ratio_vals <- ratio_vals[!is.na(ratio_vals)]
cat(sprintf("\n  Ratio encargos/(teto/100) [threshold=0.70]:\n"))
cat(sprintf("  Min=%.3f | p25=%.3f | Median=%.3f | Mean=%.3f | p75=%.3f | Max=%.3f\n\n",
            min(ratio_vals), quantile(ratio_vals,.25), median(ratio_vals),
            mean(ratio_vals), quantile(ratio_vals,.75), max(ratio_vals)))

# =============================================================================
# 3. ROB3: 2SLS com tripla interacao d_lag1 x teto x binding
#    Endogenas  : d_lag1, d_lag1_teto, d_lag1_teto_binding
#    Instrumentos: d_lag2, d_lag2_teto, d_lag2_teto_binding
# =============================================================================
cat("================================================================\n")
cat(" ROB3 - Modelo I-B (2SLS) com interacao binding\n")
cat(" Dep.: primario_sobre_rcl_ext\n")
cat("================================================================\n\n")

panel_rob3 <- panel %>%
  filter(!is.na(primario_sobre_rcl_ext),
         !is.na(d_lag1), !is.na(d_lag2),
         !is.na(d_lag1_teto), !is.na(d_lag2_teto),
         !is.na(d_lag1_teto_binding), !is.na(d_lag2_teto_binding),
         !is.na(yvar))

m_rob3 <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_teto + d_lag1_teto_binding ~
    d_lag2 + d_lag2_teto + d_lag2_teto_binding,
  data    = panel_rob3,
  cluster = ~uf
)

print(summary(m_rob3, stage = 1:2))
cat("\n--- Diagnosticos 1 estagio ---\n")
print(fitstat(m_rob3, ~ivf + wh))

# =============================================================================
# 4. Resultados
# =============================================================================
cat("\n================================================================\n")
cat(" RESULTADOS ROB3\n")
cat("================================================================\n\n")

co   <- coef(m_rob3); se_v <- se(m_rob3); pv <- pvalue(m_rob3)
mk   <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))

b1 <- co["fit_d_lag1"];              p1 <- pv["fit_d_lag1"]
b2 <- co["fit_d_lag1_teto"];         p2 <- pv["fit_d_lag1_teto"]
b3 <- co["fit_d_lag1_teto_binding"]; p3 <- pv["fit_d_lag1_teto_binding"]

cat(sprintf("  B1  DCL/RCL (t-1)            : %+.4f  SE=%.4f  p=%.4f %s\n",
            b1, se_v["fit_d_lag1"], p1, mk(p1)))
cat(sprintf("  B2  DCL/RCL x Teto           : %+.4f  SE=%.4f  p=%.4f %s\n",
            b2, se_v["fit_d_lag1_teto"], p2, mk(p2)))
cat(sprintf("  B3  DCL/RCL x Teto x Binding : %+.4f  SE=%.4f  p=%.4f %s\n",
            b3, se_v["fit_d_lag1_teto_binding"], p3, mk(p3)))
cat(sprintf("  F-stat 1 estagio             : %.0f / %.0f / %.0f\n",
            fitstat(m_rob3,"ivf")[[1]]$stat,
            fitstat(m_rob3,"ivf")[[2]]$stat,
            fitstat(m_rob3,"ivf")[[3]]$stat))
cat(sprintf("  N obs                        : %d\n\n", m_rob3$nobs))

cat("--- Interpretacao ---\n")
if (!is.na(b3) && p3 < 0.10 && b3 < 0) {
  cat("  B3 < 0 e significativo: efeito do teto MAIS FORTE quando binding.\n")
  cat("  Resultado principal ROBUSTECIDO.\n")
} else if (!is.na(b3) && p3 >= 0.10) {
  cat("  B3 nao significativo: efeito do teto independe do status binding.\n")
  cat("  Resultado principal permanece valido mas critica tem algum peso.\n")
} else if (!is.na(b3) && b3 > 0) {
  cat("  B3 > 0: efeito do teto MENOR quando binding. Verificar\n")
  cat("  multicolinearidade entre d_lag1_teto e d_lag1_teto_binding.\n")
}
cat(sprintf("\n  Baseline : B1=+0.711***  B2=-0.046***\n"))
cat(sprintf("  ROB3     : B1=%+.3f%s  B2=%+.3f%s  B3=%+.3f%s\n",
            b1, mk(p1), b2, mk(p2), b3, mk(p3)))
cat("================================================================\n")
