# =============================================================================
# fix_yvar.R
# Recalcula yvar como desvio percentual do trend HP
# Método: HP filter em log(pib_real_mil), componente cíclico ≈ % desvio
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(purrr); library(mFilter)
})

panel <- read_csv("output/panel_final_v4.csv", show_col_types = FALSE)
cat("Panel carregado:", nrow(panel), "obs ×", ncol(panel), "vars\n")

cat("\n  yvar ATUAL (absoluto R$ mil):\n")
cat("  range:", round(min(panel$yvar, na.rm=TRUE)), "a",
    round(max(panel$yvar, na.rm=TRUE)), "\n")
cat("  sd   :", round(sd(panel$yvar, na.rm=TRUE)), "\n\n")

# =============================================================================
# 1. HP filter em log(pib_real_mil) por estado
#    yvar_pct = componente cíclico ≈ (pib - trend) / trend (aprox. %)
# =============================================================================
hp_log <- panel %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  group_modify(function(df, key) {
    pib    <- df$pib_real_mil
    valid  <- !is.na(pib) & pib > 0
    n_val  <- sum(valid)

    yvar_pct  <- rep(NA_real_, nrow(df))
    hp_trend  <- rep(NA_real_, nrow(df))

    if (n_val >= 8) {
      log_pib <- log(pib[valid])
      hp <- tryCatch(
        mFilter::hpfilter(log_pib, freq = 100, type = "lambda"),
        error = function(e) NULL
      )
      if (!is.null(hp)) {
        yvar_pct[valid] <- as.numeric(hp$cycle)   # ≈ (pib - trend) / trend
        hp_trend[valid] <- exp(as.numeric(hp$trend))  # trend em nível (R$ mil)
      }
    } else {
      message("  ", key$uf, ": apenas ", n_val, " obs — HP filter não aplicado")
    }

    df$yvar_pct <- yvar_pct
    df$hp_trend <- hp_trend
    df
  }) %>%
  ungroup() %>%
  select(uf, year, yvar_pct, hp_trend)

panel_v5 <- panel %>%
  select(-any_of(c("hp_trend"))) %>%
  left_join(hp_log, by = c("uf", "year")) %>%
  mutate(yvar = yvar_pct)   # substitui coluna yvar pela versão corrigida

# =============================================================================
# 2. Verificação de escala
# =============================================================================
cat("════════════════════════════════════════════════════════\n")
cat(" 2. VERIFICAÇÃO — yvar novo (log-HP, % desvio)\n")
cat("════════════════════════════════════════════════════════\n")

yv <- panel_v5$yvar
cat(sprintf("  N não-NA : %d\n",  sum(!is.na(yv))))
cat(sprintf("  range    : %.4f  a  %.4f\n", min(yv, na.rm=TRUE), max(yv, na.rm=TRUE)))
cat(sprintf("  mean/sd  : %.4f / %.4f\n",  mean(yv, na.rm=TRUE), sd(yv, na.rm=TRUE)))

pct <- quantile(yv, c(.01,.05,.25,.50,.75,.95,.99), na.rm=TRUE)
cat("  p1/p5/p25/p50/p75/p95/p99:",
    paste(round(pct, 4), collapse=" / "), "\n")

outside <- sum(abs(yv) > 0.10, na.rm=TRUE)
cat(sprintf("  Obs com |yvar| > 0.10: %d (%.1f%%) — verificar\n",
            outside, 100*outside/sum(!is.na(yv))))

if (outside > 0) {
  cat("  Estados/anos com |yvar| > 0.10:\n")
  panel_v5 %>%
    filter(!is.na(yvar), abs(yvar) > 0.10) %>%
    select(uf, year, pib_real_mil, hp_trend, yvar) %>%
    arrange(desc(abs(yvar))) %>%
    print(n = 20)
}

# =============================================================================
# 3. Amostra SP, RJ, MG
# =============================================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" 3. AMOSTRA — SP, RJ, MG (primeiras 5 obs cada)\n")
cat("════════════════════════════════════════════════════════\n")

for (estado in c("SP", "RJ", "MG")) {
  cat(sprintf("\n  %s:\n", estado))
  panel_v5 %>%
    filter(uf == estado) %>%
    arrange(year) %>%
    head(5) %>%
    select(uf, year, pib_real_mil, hp_trend, yvar) %>%
    mutate(across(c(pib_real_mil, hp_trend), ~round(.x/1e6, 3),
                  .names = "{.col}_bi")) %>%
    select(uf, year, pib_real_mil_bi, hp_trend_bi, yvar) %>%
    print()
}

# =============================================================================
# 4. Exporta panel_final_v5.csv
# =============================================================================
write_csv(panel_v5, "output/panel_final_v5.csv")
cat(sprintf("\n✓ panel_final_v5.csv: %d obs × %d variáveis\n",
            nrow(panel_v5), ncol(panel_v5)))
cat("  yvar agora = desvio % do trend HP em log(PIB) (lambda=100)\n")
