# =============================================================================
# 03_bcb_ipca.R
# Coleta IPCA mensal do BCB/SGS (série 433) e converte para índice anual
# Documentação: https://www.bcb.gov.br/estabilidadefinanceira/historicoseriestemporais
# =============================================================================

source("R/00_setup.R")

# SGS série 433 = IPCA (% ao mês)
BCB_SGS_IPCA <- 433

# =============================================================================
# Coleta IPCA mensal via API BCB/SGS
# =============================================================================

fetch_ipca_monthly <- function() {
  out_raw <- "data/raw/bcb/raw_ipca_mensal.rds"

  if (file.exists(out_raw)) {
    message("  Usando cache: ", out_raw)
    return(readRDS(out_raw))
  }

  message("  Consultando BCB/SGS série ", BCB_SGS_IPCA, " (IPCA mensal)...")

  url <- glue("{BCB_URL}.{BCB_SGS_IPCA}/dados")
  resp <- safe_get(url, query = list(
    formato      = "json",
    dataInicial  = "01/01/2001",  # começa 2001 para calcular inflação de 2002
    dataFinal    = "31/12/2024"
  ))

  df <- parse_json_resp(resp)

  if (is.null(df)) stop("Falha ao baixar IPCA do BCB/SGS.")

  saveRDS(df, out_raw)
  message("  Salvo: ", out_raw, " — ", nrow(df), " linhas")
  df
}

# =============================================================================
# Processa: converte mensais em métricas anuais
# =============================================================================

process_ipca <- function(df_raw) {
  df <- df_raw %>%
    janitor::clean_names() %>%
    mutate(
      data  = dmy(data),
      year  = year(data),
      month = month(data),
      valor = as.numeric(valor)   # % ao mês
    ) %>%
    filter(!is.na(valor), year %in% (min(YEARS)-1):max(YEARS))

  # Acumula índice: base = 1 em jan/2001
  df <- df %>%
    arrange(data) %>%
    mutate(
      fator_mensal = 1 + valor / 100,
      indice       = cumprod(fator_mensal)
    )

  # Índice em dezembro de cada ano (para deflacionar estoques em dez)
  indice_dez <- df %>%
    filter(month == 12) %>%
    select(year, indice_dez = indice)

  # Índice médio anual (para deflacionar fluxos anuais)
  indice_medio <- df %>%
    group_by(year) %>%
    summarise(indice_medio = mean(indice), .groups = "drop")

  # IPCA acumulado no ano (% a.a.)
  ipca_anual <- df %>%
    group_by(year) %>%
    summarise(
      ipca_aa = prod(1 + valor / 100) - 1,
      .groups = "drop"
    ) %>%
    mutate(ipca_aa = ipca_aa * 100)

  out <- indice_dez %>%
    left_join(indice_medio, by = "year") %>%
    left_join(ipca_anual,   by = "year") %>%
    filter(year %in% YEARS) %>%
    arrange(year)

  # Ano-base para PIB real: 2010 = 100 (compatível com IBGE)
  base2010 <- out %>% filter(year == 2010) %>% pull(indice_dez)
  if (length(base2010) > 0 && !is.na(base2010)) {
    out <- out %>%
      mutate(
        deflator_2010 = indice_dez / base2010 * 100,   # índice base 2010
        deflator_medio_2010 = indice_medio / base2010 * 100
      )
  } else {
    out <- out %>% mutate(deflator_2010 = NA_real_, deflator_medio_2010 = NA_real_)
  }

  message(glue(
    "  IPCA: {nrow(out)} anos | IPCA 2024 acum.: ",
    "{round(out$ipca_aa[out$year == 2024], 2) %||% 'N/A'}%"
  ))

  out
}

# =============================================================================
# Execução
# =============================================================================

collect_bcb_ipca <- function() {
  message("\n=== Coletando IPCA (BCB/SGS série 433) ===")
  raw <- fetch_ipca_monthly()
  df  <- process_ipca(raw)
  write_csv(df, "data/processed/ipca_anual.csv")
  message("✓ ipca_anual.csv — ", nrow(df), " linhas")
  invisible(df)
}

if (!exists("SOURCED_BCB")) {
  ipca_data <- collect_bcb_ipca()
  SOURCED_BCB <- TRUE
}
