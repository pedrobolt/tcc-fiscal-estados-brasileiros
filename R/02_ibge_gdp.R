# =============================================================================
# 02_ibge_gdp.R
# Coleta PIB nominal por estado via IBGE SIDRA (Contas Regionais)
# Tabela 5938 — PIB a preços correntes (Mil Reais), nível N3 (UFs)
# Documentação: https://servicodados.ibge.gov.br/api/docs/agregados
# =============================================================================

source("R/00_setup.R")

# SIDRA tabela 5938 — Contas Regionais (disponível aprox. 2002-2021 com atraso ~2 anos)
# Variável 37: Produto interno bruto a preços correntes (Mil Reais)
# Variável 513: Taxa de crescimento real do PIB (%)
# Nível geográfico: N3 = Unidades da Federação

SIDRA_TABLE     <- 5938
SIDRA_VAR_PIB   <- 37      # PIB nominal (Mil R$)
SIDRA_VAR_CRESC <- 6575    # Variação real do PIB (%) — tabela pode não ter para todos anos
SIDRA_NIVEL     <- "N3"    # Estado

# =============================================================================
# Função: busca SIDRA para um conjunto de anos
# SIDRA limita a ~200 períodos por query — dividimos em blocos se necessário
# =============================================================================

fetch_sidra_pib <- function() {

  out_raw <- "data/raw/ibge/raw_sidra_pib.rds"
  if (file.exists(out_raw)) {
    message("  Usando cache: ", out_raw)
    return(readRDS(out_raw))
  }

  # Períodos disponíveis no SIDRA (Contas Regionais têm defasagem)
  # Tabela 5938 cobre 2002-2021 (atualizado anualmente)
  anos_sidra <- YEARS[YEARS <= 2022]  # SIDRA 5938 não tem 2023/2024 ainda
  periodos_str <- paste(anos_sidra, collapse = "|")

  url_pib <- glue(
    "{SIDRA_URL}/{SIDRA_TABLE}/periodos/{periodos_str}",
    "/variaveis/{SIDRA_VAR_PIB}",
    "?localidades={SIDRA_NIVEL}[all]&view=flat"
  )

  message("  Consultando SIDRA tabela ", SIDRA_TABLE, " (PIB nominal)...")
  resp <- safe_get(url_pib)

  if (is.null(resp)) {
    # Segunda tentativa: tabela alternativa 5938 sem filtro de anos
    message("  Tentando tabela alternativa 6579 (SCN estadual mais recente)...")
    url_alt <- glue(
      "{SIDRA_URL}/6579/periodos/{periodos_str}",
      "/variaveis/37?localidades={SIDRA_NIVEL}[all]&view=flat"
    )
    resp <- safe_get(url_alt)
  }

  df <- parse_json_resp(resp)

  if (is.null(df)) {
    message("  ATENÇÃO: SIDRA indisponível. Criando placeholder vazio.")
    return(NULL)
  }

  saveRDS(df, out_raw)
  message("  Salvo: ", out_raw, " — ", nrow(df), " linhas")
  df
}

# =============================================================================
# Limpa e padroniza resposta do SIDRA
# =============================================================================

process_sidra_pib <- function(df_raw) {
  if (is.null(df_raw)) {
    message("  AVISO: dados SIDRA ausentes — criando coluna pib_nominal com NA")
    return(
      expand_grid(uf = STATES$uf, year = YEARS) %>%
        mutate(pib_nominal_mil = NA_real_)
    )
  }

  # O retorno flat do SIDRA usa colunas curtas:
  #   V   = valor, D1C = código UF, D2C = ano
  # A primeira linha contém os rótulos das dimensões — remover
  df <- df_raw %>%
    filter(NC != "Nível Territorial (Código)") %>%   # descarta linha-cabeçalho
    transmute(
      cod_uf = as.integer(D1C),
      year   = as.integer(D2C),
      pib_nominal_mil = as.numeric(str_replace_all(as.character(V), "[^0-9.-]", ""))
    ) %>%
    filter(!is.na(pib_nominal_mil), !is.na(year), year %in% YEARS)

  # Join com tabela de estados por código IBGE de 2 dígitos
  df <- df %>%
    left_join(STATES %>% select(uf, id_ente), by = c("cod_uf" = "id_ente")) %>%
    filter(!is.na(uf)) %>%
    select(uf, year, pib_nominal_mil) %>%
    arrange(uf, year)

  # Verifica cobertura
  n_obs <- nrow(df)
  n_exp <- nrow(STATES) * length(YEARS[YEARS <= 2022])
  message(glue("  PIB: {n_obs} obs de {n_exp} esperadas ({round(100*n_obs/n_exp)}% cobertura)"))

  df
}

# =============================================================================
# Execução
# =============================================================================

collect_ibge_gdp <- function() {
  message("\n=== Coletando PIB estadual (IBGE SIDRA) ===")
  raw <- fetch_sidra_pib()
  df  <- process_sidra_pib(raw)
  write_csv(df, "data/processed/pib_estados.csv")
  message("✓ pib_estados.csv — ", nrow(df), " linhas")
  invisible(df)
}

if (!exists("SOURCED_IBGE")) {
  pib_data <- collect_ibge_gdp()
  SOURCED_IBGE <- TRUE
}
