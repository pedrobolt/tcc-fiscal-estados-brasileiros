# =============================================================================
# check_scale.R
# Verifica consistência de escala na transição SISTN (2002-2014) → SICONFI (2015+)
# Detecta e corrige eventuais quebras de escala (ratio vs. percentual)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(purrr); library(mFilter)
})

panel <- read_csv("output/panel_final_v3.csv", show_col_types = FALSE)
if ("pib_real_mil_2010" %in% names(panel) && !"pib_real_mil" %in% names(panel))
  panel <- rename(panel, pib_real_mil = pib_real_mil_2010)

# ============================================================
# 1. Tabela de transição 2013-2016 por estado
# ============================================================
cat("════════════════════════════════════════════════════════\n")
cat(" 1. TRANSIÇÃO 2013-2016 — dcl_sobre_rcl_ext\n")
cat("════════════════════════════════════════════════════════\n")

trans <- panel %>%
  filter(year %in% 2013:2016) %>%
  select(uf, teto, year, dcl_sobre_rcl_ext) %>%
  pivot_wider(names_from = year, values_from = dcl_sobre_rcl_ext,
              names_prefix = "dcl_") %>%
  mutate(
    jump_2014_2015 = dcl_2015 - dcl_2014,
    jump_2015_2016 = dcl_2016 - dcl_2015,
    flag_scale     = abs(jump_2014_2015) > 0.3 | (!is.na(dcl_2014) &
                       (dcl_2015 / dcl_2014 > 2 | dcl_2015 / dcl_2014 < 0.5))
  ) %>%
  arrange(desc(abs(jump_2014_2015)))

cat("\n  (ordenado por |salto 2014→2015|)\n")
print(trans %>% select(uf, teto, dcl_2013, dcl_2014, dcl_2015, dcl_2016,
                        jump_2014_2015, flag_scale), n = 25)

n_flagged <- sum(trans$flag_scale, na.rm = TRUE)
cat("\n  Estados flagrados (|salto|>0.3 ou razão>2x):", n_flagged, "\n")

# ============================================================
# 2. Diagnóstico: raw DCL e RCL nominais — SICONFI 2015 vs SISTN 2014
# ============================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 2. RAW: DCL e RCL nominais 2013-2016 (R$ mil)\n")
cat("════════════════════════════════════════════════════════\n")

raw_check <- panel %>%
  filter(year %in% 2013:2016) %>%
  select(uf, year, dcl, rcl_rgf, dcl_hist, rcl_hist,
         dcl_sobre_rcl, dcl_sobre_rcl_hist, dcl_sobre_rcl_ext, fonte_dcl_rcl)

# Mostra top-5 estados por |salto|
top_states <- trans %>% slice_head(n = 8) %>% pull(uf)
cat("\nEstados com maiores saltos:\n")
raw_check %>%
  filter(uf %in% top_states) %>%
  arrange(uf, year) %>%
  print(n = 40)

# ============================================================
# 3. Teste de escala: as séries são comparáveis?
# ============================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 3. TESTE DE ESCALA\n")
cat("════════════════════════════════════════════════════════\n")

# Se SICONFI usasse percentual (0-100) em vez de ratio (0-1),
# veríamos valores de dcl_sobre_rcl em 74, 167, etc. em 2015.
# Se SISTN usasse ratio e SICONFI percentual: salto seria ~100x.

max_sistn  <- max(panel$dcl_sobre_rcl_hist, na.rm = TRUE)
max_siconfi <- max(panel$dcl_sobre_rcl,     na.rm = TRUE)
max_ext     <- max(panel$dcl_sobre_rcl_ext, na.rm = TRUE)

cat("\n  Máximo dcl_sobre_rcl_hist (SISTN) :", round(max_sistn, 3))
cat("\n  Máximo dcl_sobre_rcl (SICONFI)    :", round(max_siconfi, 3))
cat("\n  Máximo dcl_sobre_rcl_ext (unif.)  :", round(max_ext, 3))

# Percentis
p_sistn   <- quantile(panel$dcl_sobre_rcl_hist, c(.25,.5,.75,.95), na.rm=TRUE)
p_siconfi <- quantile(panel$dcl_sobre_rcl,      c(.25,.5,.75,.95), na.rm=TRUE)
cat("\n\n  Distribuição SISTN (p25/p50/p75/p95)  :",
    paste(round(p_sistn, 3), collapse=" / "))
cat("\n  Distribuição SICONFI (p25/p50/p75/p95):",
    paste(round(p_siconfi, 3), collapse=" / "))

scale_mismatch <- (max_sistn < 5 & max_siconfi > 50) |
                  (max_sistn > 50 & max_siconfi < 5)

if (scale_mismatch) {
  cat("\n\n  *** ESCALA INCOMPATÍVEL DETECTADA ***\n")
  # Determina qual precisa ser corrigida
  if (max_siconfi > 50 && max_sistn < 5) {
    cat("  → SICONFI está em percentual; SISTN em ratio. Corrigindo SICONFI /100.\n")
    SCALE_FIX <- "siconfi_div100"
  } else {
    cat("  → SISTN está em percentual; SICONFI em ratio. Corrigindo SISTN /100.\n")
    SCALE_FIX <- "sistn_div100"
  }
} else {
  cat("\n\n  ✓ Escalas compatíveis — ambas em ratio (0-1 a ~3).\n")
  SCALE_FIX <- "none"
  cat("  Os saltos 2014→2015 refletem mudança de abrangência SISTN→SICONFI,\n")
  cat("  não erro de escala. Documentar como quebra de série com dummy 2015.\n")
}

# ============================================================
# 4. Mesma verificação para primario_sobre_rcl e encargos_sobre_rcl
# ============================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 4. ESCALA — primario_sobre_rcl e encargos_sobre_rcl\n")
cat("════════════════════════════════════════════════════════\n")

for (vv in c("primario_sobre_rcl","encargos_sobre_rcl","encargos_sobre_rcl_ext")) {
  vals <- panel[[vv]]
  if (all(is.na(vals))) { cat("\n ", vv, ": toda NA\n"); next }
  mx <- max(vals, na.rm=TRUE)
  mn <- min(vals, na.rm=TRUE)
  p  <- quantile(vals, c(.25,.5,.75,.95), na.rm=TRUE)
  cat(sprintf("\n  %s:\n    range [%.4f, %.4f] | p25/50/75/95: %s\n",
              vv, mn, mx, paste(round(p,4), collapse=" / ")))
  if (mx > 50)
    cat("    *** POSSÍVEL ERRO DE ESCALA (valores >50) ***\n")
  else if (mx > 5)
    cat("    AVISO: alguns valores >5 — verificar se aceitável\n")
  else
    cat("    ✓ Escala compatível com ratio\n")
}

# Transição 2014→2015 para encargos
cat("\n  Transição encargos_sobre_rcl_ext 2014→2015:\n")
panel %>%
  filter(year %in% 2013:2016, !is.na(encargos_sobre_rcl_ext)) %>%
  select(uf, year, encargos_sobre_rcl_ext) %>%
  pivot_wider(names_from=year, values_from=encargos_sobre_rcl_ext,
              names_prefix="enc_") %>%
  mutate(jump = round(enc_2015 - enc_2014, 4)) %>%
  arrange(desc(abs(jump))) %>%
  print(n=25)

# ============================================================
# 5. Aplica correção de escala (se necessário) e re-exporta
# ============================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 5. CORREÇÃO E EXPORTAÇÃO\n")
cat("════════════════════════════════════════════════════════\n")

panel_fixed <- panel

if (SCALE_FIX == "siconfi_div100") {
  panel_fixed <- panel_fixed %>%
    mutate(
      dcl_sobre_rcl      = dcl_sobre_rcl      / 100,
      primario_sobre_rcl = primario_sobre_rcl / 100,
      encargos_sobre_rcl = encargos_sobre_rcl / 100,
      dcl_sobre_rcl_ext  = case_when(
        fonte_dcl_rcl == "SICONFI" ~ dcl_sobre_rcl,
        TRUE ~ dcl_sobre_rcl_ext
      )
    )
  cat("  Aplicada correção: SICONFI ÷ 100\n")
} else if (SCALE_FIX == "sistn_div100") {
  panel_fixed <- panel_fixed %>%
    mutate(
      dcl_sobre_rcl_hist   = dcl_sobre_rcl_hist / 100,
      dcl_sobre_rcl_ext    = case_when(
        fonte_dcl_rcl == "SISTN" ~ dcl_sobre_rcl_hist,
        TRUE ~ dcl_sobre_rcl_ext
      )
    )
  cat("  Aplicada correção: SISTN ÷ 100\n")
} else {
  cat("  Nenhuma correção de escala necessária.\n")
  cat("  Adicionando dummy d_quebra_serie (=1 em 2015+) para capturar\n")
  cat("  a mudança de abrangência SISTN→SICONFI na regressão.\n")
}

# Dummy de quebra de série (recomendada em qualquer caso)
panel_fixed <- panel_fixed %>%
  mutate(d_quebra_serie = as.integer(year >= 2015))

# Recalcula lags e interações após correção
panel_fixed <- panel_fixed %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(
    d_lag1 = lag(dcl_sobre_rcl_ext, 1),
    d_lag2 = lag(dcl_sobre_rcl_ext, 2)
  ) %>%
  ungroup() %>%
  mutate(
    d_lag1_teto   = d_lag1 * teto,
    d_lag2_teto   = d_lag2 * teto,
    primario_teto = primario_sobre_rcl * teto
  )

# Re-exporta
write_csv(panel_fixed, "output/panel_final_v3.csv")
cat("\n✓ panel_final_v3.csv re-exportado:",
    nrow(panel_fixed), "obs ×", ncol(panel_fixed), "variáveis\n")

# ============================================================
# 6. Tabela de confirmação após correção
# ============================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 6. CONFIRMAÇÃO: transição 2013-2016 após correção\n")
cat("════════════════════════════════════════════════════════\n")

trans_after <- panel_fixed %>%
  filter(year %in% 2013:2016) %>%
  select(uf, teto, year, dcl_sobre_rcl_ext) %>%
  pivot_wider(names_from=year, values_from=dcl_sobre_rcl_ext,
              names_prefix="dcl_") %>%
  mutate(
    jump_14_15   = round(dcl_2015 - dcl_2014, 4),
    flag_residual = abs(jump_14_15) > 0.3 & !is.na(dcl_2014)
  ) %>%
  arrange(desc(abs(jump_14_15)))

print(trans_after %>%
        select(uf, teto, dcl_2013, dcl_2014, dcl_2015, dcl_2016,
               jump_14_15, flag_residual),
      n = 25)

n_flag_after <- sum(trans_after$flag_residual, na.rm=TRUE)
if (n_flag_after == 0) {
  cat("\n  ✓ Nenhum salto residual >0.3 após correção.\n")
} else {
  cat("\n  AVISO:", n_flag_after,
      "estados ainda com salto >0.3 — verificar individualmente.\n")
  cat("  Estes podem refletir deterioração fiscal real em 2015,\n")
  cat("  não erro de mensuração. Recomendar dummy interativa por estado.\n")
  trans_after %>% filter(flag_residual) %>%
    select(uf, dcl_2013, dcl_2014, dcl_2015, jump_14_15) %>%
    print()
}

cat("\n  Estatísticas dcl_sobre_rcl_ext pós-correção:\n")
panel_fixed %>%
  group_by(fonte_dcl_rcl) %>%
  summarise(
    n    = n(),
    mean = round(mean(dcl_sobre_rcl_ext, na.rm=TRUE), 3),
    sd   = round(sd(dcl_sobre_rcl_ext,   na.rm=TRUE), 3),
    min  = round(min(dcl_sobre_rcl_ext,  na.rm=TRUE), 3),
    max  = round(max(dcl_sobre_rcl_ext,  na.rm=TRUE), 3),
    .groups="drop"
  ) %>% print()

cat("\n  RS  média:", round(mean(panel_fixed$dcl_sobre_rcl_ext[panel_fixed$uf=="RS"],  na.rm=TRUE),3), "\n")
cat("  RJ  média:", round(mean(panel_fixed$dcl_sobre_rcl_ext[panel_fixed$uf=="RJ"],  na.rm=TRUE),3), "\n")
cat("  SP  média:", round(mean(panel_fixed$dcl_sobre_rcl_ext[panel_fixed$uf=="SP"],  na.rm=TRUE),3), "\n")
cat("  MG  média:", round(mean(panel_fixed$dcl_sobre_rcl_ext[panel_fixed$uf=="MG"],  na.rm=TRUE),3), "\n")

cat("\n✓ check_scale.R concluído\n")
