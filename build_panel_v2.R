# =============================================================================
# build_panel_v2.R
# Estende DCL/RCL de 2002-2014 usando série histórica do Tesouro Transparente
# (SISTN/COPEM — dados compilados de RGF, posição 31/12 de cada ano)
# e mescla com os dados SICONFI (2015-2024) já no panel_final.csv
#
# Resultado: panel_final_v2.csv — cobertura máxima da série DCL/RCL
# =============================================================================

pkgs <- c("dplyr","tidyr","readr","purrr","stringr","mFilter","janitor","httr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, repos="https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only=TRUE))

# ---- URLs Tesouro Transparente (CKAN) ----------------------------------------
URL_DCL     <- paste0("https://www.tesourotransparente.gov.br/ckan/dataset/",
                      "4d6b967d-1bc5-4f80-92ba-05e4498c111a/resource/",
                      "b2c8aa27-5551-41de-82cb-c325e87550cc/download/estados-dcl.csv")
URL_RCL     <- paste0("https://www.tesourotransparente.gov.br/ckan/dataset/",
                      "4d6b967d-1bc5-4f80-92ba-05e4498c111a/resource/",
                      "c0243b86-7e94-4c2c-ab70-df6f5e8cb8cf/download/estados-rcl.csv")
URL_RATIO   <- paste0("https://www.tesourotransparente.gov.br/ckan/dataset/",
                      "4d6b967d-1bc5-4f80-92ba-05e4498c111a/resource/",
                      "46fb732d-0182-45a9-8672-8c824be72fb7/download/estados-dcl-sobre-rcl.csv")

# ---- Converte número brasileiro (1.234.567 → 1234567) -----------------------
parse_br_num <- function(x) {
  x <- str_replace_all(as.character(x), "\\.", "")   # remove separadores de milhar
  x <- str_replace_all(x, ",", ".")                  # converte decimal pt-BR → en
  suppressWarnings(as.numeric(x))
}

# =============================================================================
# 1. Download e parse dos CSVs históricos
# =============================================================================

download_stn_csv <- function(url, label) {
  cache <- file.path("data/raw/siconfi", paste0("stn_hist_", label, ".rds"))
  if (file.exists(cache)) {
    cat("  Cache:", cache, "\n")
    return(readRDS(cache))
  }
  cat("  Baixando", label, "...\n")
  resp <- httr::GET(url, httr::timeout(60))
  if (httr::status_code(resp) != 200)
    stop("Erro HTTP ", httr::status_code(resp), " para ", url)
  raw_txt <- httr::content(resp, as="text", encoding="latin1")
  df <- read_delim(I(raw_txt), delim=";", show_col_types=FALSE,
                   locale=locale(encoding="latin1"))
  saveRDS(df, cache)
  df
}

raw_dcl   <- download_stn_csv(URL_DCL,   "dcl")
raw_rcl   <- download_stn_csv(URL_RCL,   "rcl")
raw_ratio <- download_stn_csv(URL_RATIO, "ratio")

cat("DCL  :", nrow(raw_dcl),   "estados ×", ncol(raw_dcl)-1,   "datas\n")
cat("RCL  :", nrow(raw_rcl),   "estados ×", ncol(raw_rcl)-1,   "datas\n")
cat("Ratio:", nrow(raw_ratio), "estados ×", ncol(raw_ratio)-1, "datas\n")

# =============================================================================
# 2. Reshape para formato longo — apenas colunas 31/12/YYYY
# =============================================================================

pivot_stn <- function(df, value_name) {
  # Seleciona apenas colunas do tipo "31/12/YYYY"
  date_cols <- names(df)[str_detect(names(df), "^31/12/\\d{4}$")]

  df %>%
    rename(uf = 1) %>%
    select(uf, all_of(date_cols)) %>%
    pivot_longer(-uf, names_to = "data_ref", values_to = value_name) %>%
    mutate(
      year         = as.integer(str_extract(data_ref, "\\d{4}")),
      !!value_name := parse_br_num(.data[[value_name]])
    ) %>%
    select(uf, year, all_of(value_name))
}

hist_dcl   <- pivot_stn(raw_dcl,   "dcl_hist")
hist_rcl   <- pivot_stn(raw_rcl,   "rcl_hist")
hist_ratio <- pivot_stn(raw_ratio, "dcl_rcl_hist")

# Filtra 2002-2014 e estados relevantes (os 25 com teto Lei 9.496)
STATES_25 <- c("AC","AL","AM","BA","CE","DF","ES","GO","MA","MG",
               "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO",
               "RR","RS","SC","SE","SP")

hist_dcl_rcl <- hist_dcl %>%
  inner_join(hist_rcl,   by = c("uf","year")) %>%
  inner_join(hist_ratio, by = c("uf","year")) %>%
  filter(year %in% 2002:2014, uf %in% STATES_25) %>%
  mutate(
    # Calcula ratio direto (verificação)
    dcl_rcl_calc = if_else(rcl_hist > 0, dcl_hist / rcl_hist, NA_real_),
    # Usa ratio publicado pelo STN (mais confiável — pode incluir ajustes LRF)
    dcl_sobre_rcl_hist = dcl_rcl_hist
  ) %>%
  arrange(uf, year)

cat("\n✓ Histórico SISTN processado:",
    n_distinct(hist_dcl_rcl$uf), "UFs ×",
    n_distinct(hist_dcl_rcl$year), "anos =",
    nrow(hist_dcl_rcl), "obs\n")

# Diagnóstico: comparação ratio calculado vs. publicado
ratio_check <- hist_dcl_rcl %>%
  filter(!is.na(dcl_rcl_calc), !is.na(dcl_rcl_hist)) %>%
  mutate(diff = abs(dcl_rcl_calc - dcl_rcl_hist)) %>%
  summarise(diff_max = max(diff), diff_mean = mean(diff))
cat("  Divergência calc vs. publicado: máx =",
    round(ratio_check$diff_max, 4), "| média =",
    round(ratio_check$diff_mean, 4), "\n\n")

# =============================================================================
# 3. Carrega panel_final.csv e mescla
# =============================================================================
panel <- read_csv("output/panel_final.csv", show_col_types=FALSE)
if ("pib_real_mil_2010" %in% names(panel) && !"pib_real_mil" %in% names(panel))
  panel <- rename(panel, pib_real_mil = pib_real_mil_2010)

cat("Panel atual:", nrow(panel), "obs\n")

# Extrai SICONFI DCL/RCL (2015-2024 — fonte mais recente, prevalece)
siconfi_dcl_rcl <- panel %>%
  select(uf, year, dcl_siconfi = dcl, rcl_siconfi = rcl_rgf,
         dcl_sobre_rcl_siconfi = dcl_sobre_rcl) %>%
  filter(year >= 2015)

# =============================================================================
# 4. Constrói série unificada de DCL_sobre_RCL
#    Regra: 2002-2014 → SISTN histórico | 2015+ → SICONFI
# =============================================================================
panel_ext <- panel %>%
  left_join(
    hist_dcl_rcl %>% select(uf, year,
                             dcl_hist, rcl_hist,
                             dcl_sobre_rcl_hist),
    by = c("uf","year")
  ) %>%
  mutate(
    # Série unificada: prioridade SICONFI para 2015+, SISTN para ≤2014
    dcl_sobre_rcl_ext = case_when(
      year >= 2015 & !is.na(dcl_sobre_rcl)      ~ dcl_sobre_rcl,
      year <= 2014 & !is.na(dcl_sobre_rcl_hist)  ~ dcl_sobre_rcl_hist,
      TRUE ~ NA_real_
    ),
    # Registra fonte para auditoria
    fonte_dcl_rcl = case_when(
      year >= 2015 & !is.na(dcl_sobre_rcl)      ~ "SICONFI",
      year <= 2014 & !is.na(dcl_sobre_rcl_hist)  ~ "SISTN",
      TRUE ~ NA_character_
    )
  )

# =============================================================================
# 5. Recalcula lags, HP-filter e interações usando a série estendida
# =============================================================================

# 5a. Lags
panel_ext <- panel_ext %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(
    d_lag1 = lag(dcl_sobre_rcl_ext, 1),
    d_lag2 = lag(dcl_sobre_rcl_ext, 2)
  ) %>%
  ungroup()

# 5b. HP-filter output gap (série de PIB não muda)
hp_gap <- panel_ext %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  group_modify(function(df, key) {
    pib_vals <- df$pib_real_mil
    valid    <- !is.na(pib_vals)
    n_valid  <- sum(valid)
    yvar     <- rep(NA_real_, nrow(df))
    if (n_valid >= 8) {
      hp <- tryCatch(
        mFilter::hpfilter(pib_vals[valid], freq=100, type="lambda"),
        error = function(e) NULL
      )
      if (!is.null(hp)) yvar[valid] <- as.numeric(hp$cycle)
    }
    df$yvar <- yvar
    df
  }) %>%
  ungroup() %>%
  select(uf, year, yvar)

# Substitui coluna yvar antiga
panel_ext <- panel_ext %>%
  select(-any_of("yvar")) %>%
  left_join(hp_gap, by=c("uf","year"))

# 5c. Interações
panel_ext <- panel_ext %>%
  mutate(
    d_lag1_teto   = d_lag1 * teto,
    d_lag2_teto   = d_lag2 * teto,
    primario_teto = primario_sobre_rcl * teto
  )

# =============================================================================
# 6. Exporta panel_final_v2.csv
# =============================================================================

# Reordena: variáveis novas logo após as originais
panel_v2 <- panel_ext %>%
  select(
    # Identificação
    uf, nome, id_ente, year,
    # Dummies
    eleicao_estadual, pre_eleicao, pos_eleicao,
    # Fiscais SICONFI (brutos)
    dcl, rcl_rgf, resultado_primario, receitas_primarias,
    despesas_primarias, encargos_divida,
    # Ratios originais (SICONFI, NA para 2002-2014)
    dcl_sobre_rcl, primario_sobre_rcl, encargos_sobre_rcl,
    # SISTN histórico (brutos, NA para 2015+)
    dcl_hist, rcl_hist, dcl_sobre_rcl_hist,
    # SÉRIE ESTENDIDA (principal variável dependente/endógena)
    dcl_sobre_rcl_ext, fonte_dcl_rcl,
    # PIB
    pib_nominal_mil, pib_real_mil, crescimento_pib_pct, ln_pib_real,
    # IPCA
    ipca_aa, deflator_2010,
    # Teto contratual
    teto,
    # Variáveis modelagem
    d_lag1, d_lag2, yvar,
    d_lag1_teto, d_lag2_teto, primario_teto
  ) %>%
  arrange(uf, year)

write_csv(panel_v2, "output/panel_final_v2.csv")
cat("✓ panel_final_v2.csv:", nrow(panel_v2), "obs ×",
    ncol(panel_v2), "variáveis\n\n")

# =============================================================================
# 7. Relatório de cobertura
# =============================================================================
cat("════════════════════════════════════════════════════════\n")
cat(" RELATÓRIO DE COBERTURA — panel_final_v2.csv\n")
cat("════════════════════════════════════════════════════════\n\n")

cat("--- Obs por ano (dcl_sobre_rcl_ext) ---\n")
cob_ano <- panel_v2 %>%
  group_by(year) %>%
  summarise(
    n_dcl_ext  = sum(!is.na(dcl_sobre_rcl_ext)),
    n_d_lag1   = sum(!is.na(d_lag1)),
    n_d_lag2   = sum(!is.na(d_lag2)),
    n_pib      = sum(!is.na(pib_real_mil)),
    n_yvar     = sum(!is.na(yvar)),
    n_primario = sum(!is.na(primario_sobre_rcl)),
    fonte      = paste(sort(unique(na.omit(fonte_dcl_rcl))), collapse="+"),
    .groups    = "drop"
  )
print(cob_ano, n=23)

cat("\n--- Cobertura total das variáveis-chave ---\n")
vars_key <- c("dcl_sobre_rcl_ext","d_lag1","d_lag2","yvar",
              "primario_sobre_rcl","d_lag1_teto","d_lag2_teto","primario_teto")
miss_tbl <- tibble(
  variavel  = vars_key,
  n_total   = nrow(panel_v2),
  n_presente = map_int(vars_key, ~sum(!is.na(panel_v2[[.x]]))),
  n_ausente  = map_int(vars_key, ~sum(is.na(panel_v2[[.x]]))),
  cobertura_pct = round(100 * n_presente / n_total, 1)
)
print(miss_tbl)

cat("\n--- Comparação: obs usáveis para 2SLS ---\n")
cat("  Antes (SICONFI apenas, 2015-2024):\n")
cat("    d_lag1 + d_lag2 não-NA + primario não-NA: ",
    panel_v2 %>%
      filter(year >= 2017) %>%
      filter(!is.na(d_lag1), !is.na(d_lag2), !is.na(primario_sobre_rcl)) %>%
      nrow(), " obs\n", sep="")

cat("  Depois (SISTN+SICONFI, 2004-2024):\n")
cat("    d_lag1 + d_lag2 não-NA: ",
    panel_v2 %>%
      filter(!is.na(d_lag1), !is.na(d_lag2)) %>%
      nrow(), " obs\n", sep="")
cat("    d_lag1 + d_lag2 + primario não-NA: ",
    panel_v2 %>%
      filter(!is.na(d_lag1), !is.na(d_lag2), !is.na(primario_sobre_rcl)) %>%
      nrow(), " obs\n", sep="")

cat("\n--- Cor(d_lag1, d_lag2) — relevância do instrumento ---\n")
cor_full <- panel_v2 %>%
  filter(!is.na(d_lag1), !is.na(d_lag2)) %>%
  summarise(n=n(), r=round(cor(d_lag1, d_lag2), 4))
cat("  N =", cor_full$n, "| cor =", cor_full$r, "\n")

cat("\n--- Continuidade SISTN→SICONFI em 2014/2015 (primeiras 8 UFs) ---\n")
junction <- panel_v2 %>%
  filter(year %in% 2013:2016, !is.na(dcl_sobre_rcl_ext)) %>%
  select(uf, year, dcl_sobre_rcl_ext, fonte_dcl_rcl) %>%
  pivot_wider(names_from=year, values_from=c(dcl_sobre_rcl_ext, fonte_dcl_rcl)) %>%
  arrange(uf) %>% head(8)
print(junction)

cat("\n✓ Concluído — output/panel_final_v2.csv\n")
