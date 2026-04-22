# =============================================================================
# build_panel_v3.R
# Complementa panel_final_v2.csv com encargos da dívida 2002-2014
# Fonte: STN — "Serviço da Dívida de Estados e do DF"
#        https://www.tesourotransparente.gov.br (Tesouro Transparente)
#
# Resultado primário 2002-2014: NÃO disponível em fonte aberta estruturada.
# DF DCL/RCL 2002-2014: NÃO recuperável sem RCL histórica do DF (ausente no SISTN).
# =============================================================================

pkgs <- c("dplyr","tidyr","readr","purrr","stringr","mFilter","httr","janitor")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos="https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only=TRUE))

# ============================================================
# PARTE 1 — Serviço da Dívida: extrai encargos 2002-2014
# ============================================================
URL_SERVICO <- paste0(
  "https://www.tesourotransparente.gov.br/ckan/dataset/",
  "38354dac-a7d2-42cd-bc66-999f23914505/resource/",
  "b629e251-0ef4-455d-9376-adf17c635863/download/relatorio-servico-final.csv"
)

STATES_25 <- c("AC","AL","AM","BA","CE","DF","ES","GO","MA","MG",
               "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO",
               "RR","RS","SC","SE","SP")

parse_br <- function(x) {
  x <- str_replace_all(as.character(x), "\\.", "")
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

fetch_servico <- function() {
  cache <- "data/raw/siconfi/stn_servico_divida.rds"
  if (file.exists(cache)) { cat("  Cache:", cache, "\n"); return(readRDS(cache)) }

  cat("  Baixando Serviço da Dívida...\n")
  resp <- GET(URL_SERVICO, timeout(60))
  if (status_code(resp) != 200) stop("HTTP ", status_code(resp))
  txt  <- content(resp, as="text", encoding="latin1")
  saveRDS(txt, cache)
  txt
}

parse_servico_encargos <- function(txt) {
  lines <- strsplit(txt, "\n")[[1]]

  # Linha 3: ;ANO;AC;AC;AL;AL;... (estado aparece 2x: Encargos, Amortizações)
  # Linha 4: Discriminação;;Encargos;Amortizações;Encargos;Amortizações;...
  # Linha 5+: dados (categoria ; ano ; val ; val ; ...)
  # Categ relevante: "DÍVIDA TOTAL" (primeira linha de cada bloco anual) — encargos globais
  # Mas queremos encargos TOTAIS (Adm. Direta e Indireta) por estado e ano.

  # Monta mapa de colunas: estado → coluna de Encargos (1-indexed)
  state_row  <- strsplit(lines[3], ";")[[1]]   # ex: c("","ANO","AC","AC","AL","AL",...)
  metric_row <- strsplit(lines[4], ";")[[1]]   # ex: c("Discrim","","Encargos","Amortiz",...)
  # Para cada estado único: encontra a coluna com Encargos
  state_enc_cols <- map_int(STATES_25, function(uf) {
    idxs <- which(state_row == uf)
    # Entre essas posições, pega a que tem "Encargos" em metric_row
    enc_idx <- idxs[grepl("ncargo", metric_row[idxs], ignore.case=TRUE)]
    if (length(enc_idx) == 0) NA_integer_ else enc_idx[1]
  })
  names(state_enc_cols) <- STATES_25
  missing_states <- names(state_enc_cols)[is.na(state_enc_cols)]
  if (length(missing_states) > 0)
    message("  AVISO: coluna Encargos não encontrada para: ",
            paste(missing_states, collapse=", "))

  # Filtra linhas de dados: categoria = DÍVIDA TOTAL, ano em 2002-2014
  data_lines <- lines[5:length(lines)]
  total_rows <- data_lines[grepl("^D.V.D.\\s*TOTAL|^D.VITA\\s*TOTAL", data_lines,
                                  ignore.case=TRUE)]

  # Parse cada linha TOTAL
  encargos_list <- map_dfr(total_rows, function(ln) {
    sp  <- strsplit(ln, ";")[[1]]
    yr  <- suppressWarnings(as.integer(str_extract(sp[2], "\\d{4}")))
    if (is.na(yr) || !(yr %in% 2002:2014)) return(NULL)

    enc_vals <- map_dbl(STATES_25, function(uf) {
      col_i <- state_enc_cols[uf]
      if (is.na(col_i) || col_i > length(sp)) return(NA_real_)
      parse_br(sp[col_i]) / 1000   # R$ → R$ mil
    })
    tibble(year = yr) %>%
      bind_cols(setNames(as.list(enc_vals), STATES_25))
  })

  if (nrow(encargos_list) == 0) {
    message("  AVISO: nenhuma linha TOTAL extraída. Verifique padrão regex.")
    return(NULL)
  }

  # Pivot para formato longo
  encargos_long <- encargos_list %>%
    pivot_longer(-year, names_to="uf", values_to="encargos_hist") %>%
    filter(year %in% 2002:2014) %>%
    arrange(uf, year)

  cat("  Encargos extraídos:", nrow(encargos_long), "obs |",
      n_distinct(encargos_long$year), "anos ×",
      n_distinct(encargos_long$uf), "estados\n")
  cat("  Anos:", paste(sort(unique(encargos_long$year)), collapse=" "), "\n")
  encargos_long
}

cat("=== Serviço da Dívida — Encargos 2002-2014 ===\n")
servico_txt <- fetch_servico()
encargos_hist <- parse_servico_encargos(servico_txt)

# ============================================================
# PARTE 2 — Carrega panel_final_v2 e integra encargos
# ============================================================
cat("\n=== Carregando panel_final_v2.csv ===\n")
panel <- read_csv("output/panel_final_v2.csv", show_col_types=FALSE)
if ("pib_real_mil_2010" %in% names(panel) && !"pib_real_mil" %in% names(panel))
  panel <- rename(panel, pib_real_mil = pib_real_mil_2010)
cat("Panel v2:", nrow(panel), "obs ×", ncol(panel), "vars\n")

# Merge encargos históricos
panel_v3 <- panel %>%
  left_join(encargos_hist, by=c("uf","year")) %>%
  mutate(
    # Série unificada de encargos: SISTN para ≤2014, SICONFI para ≥2015
    encargos_ext = case_when(
      year >= 2015 & !is.na(encargos_divida) ~ encargos_divida,
      year <= 2014 & !is.na(encargos_hist)   ~ encargos_hist,
      TRUE ~ NA_real_
    ),
    fonte_encargos = case_when(
      year >= 2015 & !is.na(encargos_divida) ~ "SICONFI",
      year <= 2014 & !is.na(encargos_hist)   ~ "STN-Servico",
      TRUE ~ NA_character_
    ),
    # encargos_sobre_rcl estendido — usa rcl_hist para 2002-2014
    rcl_ext = case_when(
      year >= 2015 & !is.na(rcl_rgf)  ~ rcl_rgf,
      year <= 2014 & !is.na(rcl_hist) ~ rcl_hist,
      TRUE ~ NA_real_
    ),
    encargos_sobre_rcl_ext = if_else(
      !is.na(encargos_ext) & !is.na(rcl_ext) & rcl_ext > 0,
      encargos_ext / rcl_ext, NA_real_
    )
  )

cat("  encargos_ext preenchido:",
    sum(!is.na(panel_v3$encargos_ext)), "obs\n")

# ============================================================
# PARTE 3 — Recalcula lags, HP-filter e interações
# ============================================================

# Lags de dcl_sobre_rcl_ext (já existe no v2; recalculamos para garantir)
panel_v3 <- panel_v3 %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(
    d_lag1 = lag(dcl_sobre_rcl_ext, 1),
    d_lag2 = lag(dcl_sobre_rcl_ext, 2)
  ) %>%
  ungroup()

# HP-filter output gap por estado
hp_gap <- panel_v3 %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  group_modify(function(df, key) {
    pib  <- df$pib_real_mil
    ok   <- !is.na(pib)
    yvar <- rep(NA_real_, nrow(df))
    if (sum(ok) >= 8) {
      hp <- tryCatch(mFilter::hpfilter(pib[ok], freq=100, type="lambda"),
                     error=function(e) NULL)
      if (!is.null(hp)) yvar[ok] <- as.numeric(hp$cycle)
    }
    df$yvar <- yvar; df
  }) %>%
  ungroup() %>%
  select(uf, year, yvar)

panel_v3 <- panel_v3 %>%
  select(-any_of("yvar")) %>%
  left_join(hp_gap, by=c("uf","year"))

# Interações
panel_v3 <- panel_v3 %>%
  mutate(
    d_lag1_teto   = d_lag1 * teto,
    d_lag2_teto   = d_lag2 * teto,
    primario_teto = primario_sobre_rcl * teto
  )

# ============================================================
# PARTE 4 — Reordena colunas e exporta
# ============================================================
panel_v3 <- panel_v3 %>%
  select(
    uf, nome, id_ente, year,
    eleicao_estadual, pre_eleicao, pos_eleicao,
    # Fiscais SICONFI (2015+)
    dcl, rcl_rgf, resultado_primario,
    receitas_primarias, despesas_primarias, encargos_divida,
    # Histórico SISTN (2002-2014)
    dcl_hist, rcl_hist, dcl_sobre_rcl_hist,
    encargos_hist,
    # Séries unificadas (principal)
    dcl_sobre_rcl_ext, fonte_dcl_rcl,
    rcl_ext, encargos_ext, encargos_sobre_rcl_ext, fonte_encargos,
    # Ratios originais
    dcl_sobre_rcl, primario_sobre_rcl, encargos_sobre_rcl,
    # PIB e preços
    pib_nominal_mil, pib_real_mil, crescimento_pib_pct, ln_pib_real,
    ipca_aa, deflator_2010,
    # Teto e variáveis do modelo
    teto, d_lag1, d_lag2, yvar,
    d_lag1_teto, d_lag2_teto, primario_teto
  ) %>%
  arrange(uf, year)

write_csv(panel_v3, "output/panel_final_v3.csv")
cat("\n✓ panel_final_v3.csv:", nrow(panel_v3), "obs ×",
    ncol(panel_v3), "variáveis\n")

# ============================================================
# PARTE 5 — Relatório de cobertura
# ============================================================
cat("\n════════════════════════════════════════════════════════\n")
cat(" RELATÓRIO DE COBERTURA — panel_final_v3.csv\n")
cat("════════════════════════════════════════════════════════\n\n")

cat("--- Obs por variável e ano (2002-2024) ---\n")
cob <- panel_v3 %>%
  group_by(year) %>%
  summarise(
    dcl_rcl   = sum(!is.na(dcl_sobre_rcl_ext)),
    d_lag1    = sum(!is.na(d_lag1)),
    d_lag2    = sum(!is.na(d_lag2)),
    primario  = sum(!is.na(primario_sobre_rcl)),
    encargos  = sum(!is.na(encargos_ext)),
    enc_rcl   = sum(!is.na(encargos_sobre_rcl_ext)),
    pib       = sum(!is.na(pib_real_mil)),
    yvar      = sum(!is.na(yvar)),
    fonte     = paste(sort(unique(na.omit(fonte_dcl_rcl))), collapse="+"),
    .groups   = "drop"
  )
print(cob, n=23)

cat("\n--- Estados com missing em dcl_sobre_rcl_ext ---\n")
miss_uf <- panel_v3 %>%
  filter(is.na(dcl_sobre_rcl_ext)) %>%
  group_by(uf) %>%
  summarise(anos_miss = paste(year, collapse=","),
            n_miss = n(), .groups="drop") %>%
  arrange(desc(n_miss))
if (nrow(miss_uf) == 0) cat("  Nenhum!\n") else print(miss_uf)

cat("\n--- Estados com missing em encargos_ext ---\n")
miss_enc <- panel_v3 %>%
  filter(is.na(encargos_ext)) %>%
  group_by(uf) %>%
  summarise(anos_miss = paste(year, collapse=","),
            n_miss = n(), .groups="drop") %>%
  arrange(desc(n_miss))
if (nrow(miss_enc) == 0) cat("  Nenhum!\n") else print(miss_enc)

cat("\n--- Cobertura total das variáveis-chave ---\n")
vars_key <- c("dcl_sobre_rcl_ext","d_lag1","d_lag2","encargos_ext",
              "encargos_sobre_rcl_ext","primario_sobre_rcl",
              "d_lag1_teto","d_lag2_teto","primario_teto","yvar")
cob_total <- tibble(
  variavel      = vars_key,
  n_presente    = map_int(vars_key, ~sum(!is.na(panel_v3[[.x]]))),
  n_ausente     = map_int(vars_key, ~sum(is.na(panel_v3[[.x]]))),
  cobertura_pct = round(100 * n_presente / nrow(panel_v3), 1)
)
print(cob_total)

cat("\n--- DCL/RCL médio por estado (seleção) ---\n")
media_dcl <- panel_v3 %>%
  filter(!is.na(dcl_sobre_rcl_ext)) %>%
  group_by(uf) %>%
  summarise(
    n      = n(),
    media  = round(mean(dcl_sobre_rcl_ext, na.rm=TRUE), 3),
    dp     = round(sd(dcl_sobre_rcl_ext,   na.rm=TRUE), 3),
    min    = round(min(dcl_sobre_rcl_ext,  na.rm=TRUE), 3),
    max    = round(max(dcl_sobre_rcl_ext,  na.rm=TRUE), 3),
    .groups="drop"
  ) %>%
  arrange(desc(media))
print(media_dcl, n=25)

cat("\n--- cor(d_lag1, d_lag2) — relevância do instrumento ---\n")
cr <- panel_v3 %>% filter(!is.na(d_lag1), !is.na(d_lag2)) %>%
  summarise(n=n(), r=round(cor(d_lag1,d_lag2),4))
cat("  N =", cr$n, "| cor(d_lag1, d_lag2) =", cr$r, "\n")

cat("\n--- Obs usáveis para 2SLS (d_lag1+d_lag2 não-NA) ---\n")
cat("  Total:", sum(!is.na(panel_v3$d_lag1) & !is.na(panel_v3$d_lag2)), "\n")
cat("  + primario não-NA:",
    sum(!is.na(panel_v3$d_lag1) & !is.na(panel_v3$d_lag2) &
        !is.na(panel_v3$primario_sobre_rcl)), "\n")
cat("  + encargos não-NA:",
    sum(!is.na(panel_v3$d_lag1) & !is.na(panel_v3$d_lag2) &
        !is.na(panel_v3$encargos_sobre_rcl_ext)), "\n")

cat("\n--- Nota sobre fontes ---\n")
cat("  dcl_sobre_rcl_ext : SISTN 2002-2014 + SICONFI 2015-2024\n")
cat("  encargos_ext      : STN-Serviço 2002-2014 + SICONFI 2015-2024\n")
cat("  primario_sobre_rcl: SICONFI apenas (2015-2024)\n")
cat("  DF DCL/RCL 2002-2014: indisponível (ausente no SISTN; sem RCL separada)\n")
cat("  Resultado primário 2002-2014: não disponível em fonte aberta estruturada\n")

cat("\n✓ Concluído — output/panel_final_v3.csv\n")
