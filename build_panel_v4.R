# =============================================================================
# build_panel_v4.R
# Mescla FINBRA resultado primário 2002-2014 com panel_final_v3.csv
# Cria primario_sobre_rcl_ext (FINBRA 2002-2014 + SICONFI 2015-2024)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)
})

# =============================================================================
# 1. Carrega arquivos
# =============================================================================
panel <- read_csv("output/panel_final_v3.csv", show_col_types = FALSE)
finbra <- read_csv("data/raw/finbra_receitas_despesas_resultado_2002_2014.csv",
                   show_col_types = FALSE)

cat("Panel:", nrow(panel), "obs ×", ncol(panel), "vars\n")
cat("FINBRA:", nrow(finbra), "obs | colunas:", paste(names(finbra), collapse=", "), "\n")
cat("FINBRA anos:", range(finbra$ano), "| estados:", n_distinct(finbra$estado), "\n\n")

# =============================================================================
# 2. Diagnóstico de magnitude — razão FINBRA vs rcl_hist para casos spot-check
# =============================================================================
cat("════════════════════════════════════════════════════════\n")
cat(" 2. DIAGNÓSTICO DE UNIDADES\n")
cat("════════════════════════════════════════════════════════\n")

# rcl_hist está em R$ mil; FINBRA em R$ bilhões
# Conversão: FINBRA (bilhões) × 1.000.000 = R$ mil
spots <- list(
  list(uf="AC", yr=2002),
  list(uf="SP", yr=2005),
  list(uf="MG", yr=2008)
)

for (s in spots) {
  p_row <- panel %>% filter(uf == s$uf, year == s$yr)
  f_row <- finbra %>% filter(estado == s$uf, ano == s$yr)
  if (nrow(p_row) == 0 || nrow(f_row) == 0) {
    cat(sprintf("  %s %d: dado ausente\n", s$uf, s$yr)); next
  }
  rcl_mil   <- p_row$rcl_hist
  res_bi    <- f_row$resultado_primario
  res_mil   <- res_bi * 1e6
  ratio     <- res_mil / rcl_mil
  cat(sprintf("  %s %d: rcl_hist=%s R$mil | resultado_FINBRA=%.4f bi → %.0f R$mil | ratio=%.4f\n",
              s$uf, s$yr,
              formatC(rcl_mil, format="f", digits=0, big.mark="."),
              res_bi, res_mil, ratio))
}

# =============================================================================
# 3. Calcula primario_sobre_rcl_hist = resultado_primario (bi→mil) / rcl_hist
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 3. CÁLCULO primario_sobre_rcl_hist\n")
cat("════════════════════════════════════════════════════════\n")

finbra_ratio <- finbra %>%
  rename(uf = estado, year = ano) %>%
  left_join(panel %>% select(uf, year, rcl_hist), by = c("uf", "year")) %>%
  mutate(
    resultado_primario_mil = resultado_primario * 1e6,
    primario_sobre_rcl_hist = if_else(
      !is.na(rcl_hist) & rcl_hist > 0,
      resultado_primario_mil / rcl_hist,
      NA_real_
    )
  ) %>%
  select(uf, year, resultado_primario_mil, primario_sobre_rcl_hist)

cat("Obs calculadas:", sum(!is.na(finbra_ratio$primario_sobre_rcl_hist)), "\n")
cat("Range primario_sobre_rcl_hist:",
    round(min(finbra_ratio$primario_sobre_rcl_hist, na.rm=TRUE), 4), "a",
    round(max(finbra_ratio$primario_sobre_rcl_hist, na.rm=TRUE), 4), "\n")

# =============================================================================
# 4. Sanity check: flag valores fora de [-0.30, +0.30]
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 4. SANITY CHECK — valores fora de [-0.30, +0.30]\n")
cat("════════════════════════════════════════════════════════\n")

suspeitos <- finbra_ratio %>%
  filter(!is.na(primario_sobre_rcl_hist),
         abs(primario_sobre_rcl_hist) > 0.30)

if (nrow(suspeitos) == 0) {
  cat("  ✓ Todos os valores dentro de [-0.30, +0.30]\n")
} else {
  cat("  *** SUSPEITOS (|ratio| > 0.30): ***\n")
  print(suspeitos %>% arrange(desc(abs(primario_sobre_rcl_hist))))
}

# Distribuição geral
p_dist <- quantile(finbra_ratio$primario_sobre_rcl_hist, c(.05,.25,.50,.75,.95), na.rm=TRUE)
cat("\n  Distribuição (p5/p25/p50/p75/p95):",
    paste(round(p_dist, 4), collapse=" / "), "\n")

# =============================================================================
# 5. Merge: cria primario_sobre_rcl_ext
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 5. MERGE — primario_sobre_rcl_ext\n")
cat("════════════════════════════════════════════════════════\n")

panel_v4 <- panel %>%
  left_join(finbra_ratio %>% select(uf, year, primario_sobre_rcl_hist),
            by = c("uf", "year")) %>%
  mutate(
    primario_sobre_rcl_ext = case_when(
      year <= 2014 & !is.na(primario_sobre_rcl_hist) ~ primario_sobre_rcl_hist,
      year >= 2015 & !is.na(primario_sobre_rcl)      ~ primario_sobre_rcl,
      TRUE ~ NA_real_
    ),
    fonte_primario = case_when(
      year <= 2014 & !is.na(primario_sobre_rcl_hist) ~ "FINBRA",
      year >= 2015 & !is.na(primario_sobre_rcl)      ~ "SICONFI",
      TRUE ~ NA_character_
    )
  )

cat("  Cobertura primario_sobre_rcl_ext:\n")
panel_v4 %>%
  group_by(fonte_primario) %>%
  summarise(n = n(), .groups="drop") %>%
  print()

# =============================================================================
# 6. Transição 2013-2016 — flag saltos > 0.15
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 6. TRANSIÇÃO 2013-2016 — primario_sobre_rcl_ext\n")
cat("════════════════════════════════════════════════════════\n")

trans_prim <- panel_v4 %>%
  filter(year %in% 2013:2016) %>%
  select(uf, teto, year, primario_sobre_rcl_ext) %>%
  pivot_wider(names_from = year, values_from = primario_sobre_rcl_ext,
              names_prefix = "p_") %>%
  mutate(
    jump_14_15   = round(p_2015 - p_2014, 4),
    flag_jump    = !is.na(jump_14_15) & abs(jump_14_15) > 0.15
  ) %>%
  arrange(desc(abs(jump_14_15)))

print(trans_prim %>%
        select(uf, teto, p_2013, p_2014, p_2015, p_2016, jump_14_15, flag_jump),
      n = 25)

n_flag <- sum(trans_prim$flag_jump, na.rm = TRUE)
cat("\n  Estados com |salto 2014→2015| > 0.15:", n_flag, "\n")
if (n_flag > 0) {
  cat("  Flagrados:\n")
  trans_prim %>% filter(flag_jump) %>%
    select(uf, p_2013, p_2014, p_2015, jump_14_15) %>% print()
}

# =============================================================================
# 7. Recalcula primario_teto
# =============================================================================
panel_v4 <- panel_v4 %>%
  mutate(primario_teto = primario_sobre_rcl_ext * teto)

# =============================================================================
# 8. Exporta panel_final_v4.csv
# =============================================================================
write_csv(panel_v4, "output/panel_final_v4.csv")
cat("\n✓ panel_final_v4.csv exportado:", nrow(panel_v4), "obs ×",
    ncol(panel_v4), "variáveis\n")

# =============================================================================
# 9. Cobertura final e obs para Model I
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 9. COBERTURA FINAL\n")
cat("════════════════════════════════════════════════════════\n")

n_tot  <- nrow(panel_v4)
n_prim <- sum(!is.na(panel_v4$primario_sobre_rcl_ext))
cat(sprintf("\n  primario_sobre_rcl_ext: %d/%d obs (%.1f%%)\n",
            n_prim, n_tot, 100*n_prim/n_tot))

cat("\n  Obs por ano:\n")
panel_v4 %>%
  group_by(year) %>%
  summarise(
    n_prim_ext = sum(!is.na(primario_sobre_rcl_ext)),
    n_prim_old = sum(!is.na(primario_sobre_rcl)),
    fonte      = paste(sort(unique(na.omit(fonte_primario))), collapse="+"),
    .groups = "drop"
  ) %>% print(n = 23)

cat("\n  Média de primario_sobre_rcl_ext por estado:\n")
panel_v4 %>%
  group_by(uf) %>%
  summarise(
    n    = sum(!is.na(primario_sobre_rcl_ext)),
    mean = round(mean(primario_sobre_rcl_ext, na.rm=TRUE), 4),
    sd   = round(sd(primario_sobre_rcl_ext,   na.rm=TRUE), 4),
    min  = round(min(primario_sobre_rcl_ext,  na.rm=TRUE), 4),
    max  = round(max(primario_sobre_rcl_ext,  na.rm=TRUE), 4),
    .groups = "drop"
  ) %>% arrange(uf) %>% print(n = 25)

cat("\n  Model I — obs com d_lag1, d_lag2, primario_sobre_rcl_ext, yvar não-NA:\n")
model1_n <- panel_v4 %>%
  filter(!is.na(d_lag1), !is.na(d_lag2),
         !is.na(primario_sobre_rcl_ext), !is.na(yvar)) %>%
  nrow()
cat("  N =", model1_n, "\n")

# Antes (apenas SICONFI, 2017+)
model1_before <- panel_v4 %>%
  filter(year >= 2017,
         !is.na(d_lag1), !is.na(d_lag2),
         !is.na(primario_sobre_rcl), !is.na(yvar)) %>%
  nrow()
cat("  Antes (SICONFI 2017+):", model1_before, "obs\n")
cat("  Ganho:", model1_n - model1_before, "obs adicionais\n")

cat("\n✓ build_panel_v4.R concluído\n")
