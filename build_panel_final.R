# =============================================================================
# build_panel_final.R
# Prepara painel final para replicação de Abubakar et al. (2025)
# Variáveis: lags de DCL/RCL, HP-filter output gap, interações com teto Lei 9.496
# =============================================================================

# ---- Pacotes ----------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "readr", "mFilter", "stringr", "purrr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  message("Instalando: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

# =============================================================================
# 1. Carrega painel
# =============================================================================
csv_path <- "output/panel_estados_brasil.csv"
if (!file.exists(csv_path)) stop("Arquivo não encontrado: ", csv_path)

panel <- read_csv(csv_path, show_col_types = FALSE)

# Normaliza nome da coluna PIB (aceita pib_real_mil ou pib_real_mil_2010)
if ("pib_real_mil_2010" %in% names(panel) && !"pib_real_mil" %in% names(panel)) {
  panel <- panel %>% rename(pib_real_mil = pib_real_mil_2010)
}

required <- c("uf", "year", "dcl_sobre_rcl", "primario_sobre_rcl",
              "encargos_sobre_rcl", "pib_real_mil")
missing_cols <- setdiff(required, names(panel))
if (length(missing_cols) > 0) stop("Colunas ausentes: ", paste(missing_cols, collapse = ", "))

cat("Panel carregado:", nrow(panel), "obs ×", ncol(panel), "variáveis\n")
cat("UFs originais:", paste(sort(unique(panel$uf)), collapse = " "), "\n\n")

# =============================================================================
# 2. Tetos contratuais — Lei 9.496/97 (% da RCL)
# AP e TO excluídos (sem contrato Lei 9.496)
# =============================================================================
tetos_df <- tribble(
  ~uf,  ~teto,
  "AC",  12,
  "AM",  12,
  "PA",  15,
  "RO",  15,
  "RR",  12,
  "MA",  13,
  "PI",  13,
  "CE",  12,
  "RN",  13,
  "PB",  13,
  "PE",  12,
  "AL",  15,
  "SE",  13,
  "BA",  13,
  "MG",  13,
  "ES",  13,
  "RJ",  13,
  "SP",  13,
  "PR",  13,
  "SC",  13,
  "RS",  13,
  "MT",  15,
  "MS",  15,
  "GO",  15,
  "DF",  13
)

cat("Tetos definidos para", nrow(tetos_df), "estados (AP e TO excluídos)\n")
cat("Distribuição de tetos:\n")
print(tetos_df %>% count(teto, name = "n_estados"))
cat("\n")

# =============================================================================
# 3. Remove AP e TO do painel
# =============================================================================
n_antes <- nrow(panel)
panel <- panel %>% filter(!uf %in% c("AP", "TO"))
cat("Excluídos AP e TO:", n_antes - nrow(panel), "obs removidas\n")
cat("Painel após exclusão:", nrow(panel), "obs |",
    n_distinct(panel$uf), "UFs ×", n_distinct(panel$year), "anos\n\n")

# =============================================================================
# 4. Merge tetos
# =============================================================================
panel <- panel %>%
  left_join(tetos_df, by = "uf")

# Verifica que nenhum estado ficou sem teto
sem_teto <- panel %>% filter(is.na(teto)) %>% distinct(uf)
if (nrow(sem_teto) > 0)
  warning("Estados sem teto após merge: ", paste(sem_teto$uf, collapse = ", "))

# =============================================================================
# 5. Lags de DCL/RCL e HP-filter output gap
# =============================================================================

# 5a. Lags (ordenação garantida dentro de cada estado)
panel <- panel %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(
    d_lag1 = lag(dcl_sobre_rcl, 1),   # d_{i,t-1}
    d_lag2 = lag(dcl_sobre_rcl, 2)    # d_{i,t-2} — instrumento
  ) %>%
  ungroup()

# 5b. HP-filter output gap por estado
# Aplica o filtro apenas nas observações não-NA de pib_real_mil
# Lambda = 100 (padrão para dados anuais — Hodrick & Prescott 1997)
hp_gap <- panel %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  # Trabalha apenas com anos que têm PIB não-NA
  mutate(row_id = row_number()) %>%
  group_modify(function(df, key) {
    pib_vals <- df$pib_real_mil
    valid    <- !is.na(pib_vals)
    n_valid  <- sum(valid)

    yvar <- rep(NA_real_, nrow(df))

    if (n_valid >= 8) {
      # Aplica HP filter nos valores disponíveis
      hp <- tryCatch(
        mFilter::hpfilter(pib_vals[valid], freq = 100, type = "lambda"),
        error = function(e) {
          message("  HP filter falhou para ", key$uf, ": ", e$message)
          NULL
        }
      )
      if (!is.null(hp)) {
        # cycle = desvio em relação à tendência (proxy do output gap)
        yvar[valid] <- as.numeric(hp$cycle)
      }
    } else {
      message("  ", key$uf, ": apenas ", n_valid,
              " obs de PIB — HP filter não aplicado (mínimo 8)")
    }

    df$yvar <- yvar
    df
  }) %>%
  ungroup() %>%
  select(uf, year, yvar)

panel <- panel %>% left_join(hp_gap, by = c("uf", "year"))

# =============================================================================
# 6. Variáveis de interação com teto
# =============================================================================
panel <- panel %>%
  mutate(
    # Modelo I: endógena e instrumento com teto
    d_lag1_teto  = d_lag1 * teto,       # d_{i,t-1} × teto
    d_lag2_teto  = d_lag2 * teto,       # d_{i,t-2} × teto (instrumento 2)
    # Modelo II: interação primário com teto
    primario_teto = primario_sobre_rcl * teto
  )

# =============================================================================
# 7. Exporta panel_final.csv
# =============================================================================
# Garante que pib_real_mil_2010 seja preservado com nome original se quiser
# (mantemos pib_real_mil, que é o mesmo dado)
write_csv(panel, "output/panel_final.csv")
cat("✓ panel_final.csv exportado:",
    nrow(panel), "obs ×", ncol(panel), "variáveis\n\n")

# =============================================================================
# 8. Sumário diagnóstico
# =============================================================================
new_vars <- c("teto", "d_lag1", "d_lag2", "yvar",
              "d_lag1_teto", "d_lag2_teto", "primario_teto")

cat("═══════════════════════════════════════════════════════\n")
cat(" SUMÁRIO DO PAINEL FINAL\n")
cat("═══════════════════════════════════════════════════════\n")
cat(sprintf("  Estados  : %d\n", n_distinct(panel$uf)))
cat(sprintf("  Anos     : %d (%d–%d)\n",
            n_distinct(panel$year), min(panel$year), max(panel$year)))
cat(sprintf("  Obs total: %d\n\n", nrow(panel)))

# Missings por variável nova
cat("--- Missing values por variável nova ---\n")
miss_tbl <- map_dfr(new_vars, function(v) {
  x <- panel[[v]]
  tibble(variavel = v,
         n_total   = length(x),
         n_missing = sum(is.na(x)),
         pct_miss  = round(100 * mean(is.na(x)), 1))
})
print(miss_tbl, n = 20)

# Média e DP por grupo de teto
cat("\n--- Média e DP por grupo de teto (12 / 13 / 15) ---\n")
stats_teto <- panel %>%
  filter(!is.na(teto)) %>%
  group_by(teto) %>%
  summarise(
    n              = n(),
    d_lag1_media   = round(mean(d_lag1,        na.rm = TRUE), 4),
    d_lag1_dp      = round(sd(d_lag1,          na.rm = TRUE), 4),
    d_lag2_media   = round(mean(d_lag2,        na.rm = TRUE), 4),
    d_lag2_dp      = round(sd(d_lag2,          na.rm = TRUE), 4),
    yvar_media     = round(mean(yvar,           na.rm = TRUE), 2),
    yvar_dp        = round(sd(yvar,             na.rm = TRUE), 2),
    primario_media = round(mean(primario_sobre_rcl, na.rm = TRUE), 4),
    primario_dp    = round(sd(primario_sobre_rcl,   na.rm = TRUE), 4),
    .groups = "drop"
  )
print(stats_teto)

# Correlação d_lag1 ~ d_lag2 (relevância do instrumento)
cat("\n--- Relevância do instrumento: cor(d_lag1, d_lag2) ---\n")
cor_instr <- panel %>%
  filter(!is.na(d_lag1), !is.na(d_lag2)) %>%
  summarise(
    n_obs       = n(),
    correlacao  = round(cor(d_lag1, d_lag2), 4),
    .groups     = "drop"
  )
cat(sprintf("  N = %d | cor(d_lag1, d_lag2) = %.4f\n\n",
            cor_instr$n_obs, cor_instr$correlacao))

# Cobertura por ano das variáveis-chave
cat("--- Obs com dcl_sobre_rcl não-NA por ano ---\n")
panel %>%
  group_by(year) %>%
  summarise(
    n_dcl  = sum(!is.na(dcl_sobre_rcl)),
    n_lag1 = sum(!is.na(d_lag1)),
    n_lag2 = sum(!is.na(d_lag2)),
    n_pib  = sum(!is.na(pib_real_mil)),
    n_yvar = sum(!is.na(yvar)),
    .groups = "drop"
  ) %>%
  print(n = 23)

cat("\n✓ Concluído. Arquivo: output/panel_final.csv\n")
