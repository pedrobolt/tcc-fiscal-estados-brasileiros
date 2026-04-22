# =============================================================================
# 04_build_panel.R
# Monta o painel balanceado: 27 estados × 23 anos (2002-2024)
# Calcula variáveis derivadas e exporta panel_estados_brasil.csv
# =============================================================================

source("R/00_setup.R")

build_panel <- function() {

  message("\n=== Construindo painel balanceado ===")

  # ---- Carrega dados processados --------------------------------------------
  siconfi <- read_csv("data/processed/siconfi_processado.csv",
                      show_col_types = FALSE)
  pib     <- read_csv("data/processed/pib_estados.csv",
                      show_col_types = FALSE)
  ipca    <- read_csv("data/processed/ipca_anual.csv",
                      show_col_types = FALSE)

  # ---- Esqueleto balanceado -------------------------------------------------
  painel_base <- expand_grid(
    uf   = STATES$uf,
    year = YEARS
  ) %>%
    left_join(STATES %>% select(uf, id_ente, nome), by = "uf")

  stopifnot(nrow(painel_base) == 27 * 23)   # 621 obs
  message("  Esqueleto: ", nrow(painel_base), " obs (", 27, " UFs × ", 23, " anos)")

  # ---- Junta SICONFI --------------------------------------------------------
  painel <- painel_base %>%
    left_join(
      siconfi %>% select(uf, year, dcl, rcl_rgf, resultado_primario,
                         receitas_primarias, despesas_primarias, encargos_divida),
      by = c("uf", "year")
    )

  # ---- Junta PIB nominal ----------------------------------------------------
  painel <- painel %>%
    left_join(pib %>% select(uf, year, pib_nominal_mil), by = c("uf", "year"))

  # ---- Junta IPCA -----------------------------------------------------------
  painel <- painel %>%
    left_join(
      ipca %>% select(year, ipca_aa, deflator_2010, deflator_medio_2010),
      by = "year"
    )

  # ---- Dummy eleitoral (eleições estaduais: 2002, 2006, 2010, 2014, 2018, 2022) ----
  painel <- painel %>%
    mutate(
      eleicao_estadual = as.integer(year %in% ELECTION_YEARS),
      pre_eleicao      = as.integer((year - 1) %in% ELECTION_YEARS),
      pos_eleicao      = as.integer((year + 1) %in% ELECTION_YEARS)
    )

  # ---- Variáveis derivadas --------------------------------------------------
  painel <- painel %>%
    mutate(
      # Ratios fiscais (valores SICONFI estão em R$ mil)
      dcl_sobre_rcl         = if_else(!is.na(dcl) & !is.na(rcl_rgf) & rcl_rgf > 0,
                                       dcl / rcl_rgf, NA_real_),
      primario_sobre_rcl    = if_else(!is.na(resultado_primario) & !is.na(rcl_rgf) & rcl_rgf > 0,
                                       resultado_primario / rcl_rgf, NA_real_),
      encargos_sobre_rcl    = if_else(!is.na(encargos_divida) & !is.na(rcl_rgf) & rcl_rgf > 0,
                                       encargos_divida / rcl_rgf, NA_real_),

      # PIB real (base 2010 = 100): pib_real em R$ mil de 2010
      pib_real_mil_2010 = if_else(
        !is.na(pib_nominal_mil) & !is.na(deflator_medio_2010) & deflator_medio_2010 > 0,
        pib_nominal_mil / deflator_medio_2010 * 100,
        NA_real_
      )
    )

  # ---- Crescimento real do PIB por estado -----------------------------------
  painel <- painel %>%
    arrange(uf, year) %>%
    group_by(uf) %>%
    mutate(
      pib_real_mil_lag1   = lag(pib_real_mil_2010, 1),
      crescimento_pib_pct = if_else(
        !is.na(pib_real_mil_2010) & !is.na(pib_real_mil_lag1) & pib_real_mil_lag1 > 0,
        (pib_real_mil_2010 / pib_real_mil_lag1 - 1) * 100,
        NA_real_
      ),
      # Ln do PIB real (útil para modelos log-nível)
      ln_pib_real = log(pib_real_mil_2010)
    ) %>%
    ungroup()

  # ---- RCL per capita (se população disponível) -----------------------------
  # Nota: população estadual anual pode ser adicionada via IBGE Estimativas
  # Aqui registramos placeholder para extensão futura
  # painel <- painel %>% mutate(rcl_per_capita = rcl_rgf / populacao_mil * 1000)

  # ---- Reordena colunas -----------------------------------------------------
  painel <- painel %>%
    select(
      # Identificação
      uf, nome, id_ente, year,
      # Dummies
      eleicao_estadual, pre_eleicao, pos_eleicao,
      # Fiscais (R$ mil)
      dcl, rcl_rgf, resultado_primario, receitas_primarias,
      despesas_primarias, encargos_divida,
      # Ratios fiscais
      dcl_sobre_rcl, primario_sobre_rcl, encargos_sobre_rcl,
      # PIB
      pib_nominal_mil, pib_real_mil_2010, pib_real_mil_lag1,
      crescimento_pib_pct, ln_pib_real,
      # Preços
      ipca_aa, deflator_2010, deflator_medio_2010
    ) %>%
    arrange(uf, year)

  # ---- Exporta ---------------------------------------------------------------
  write_csv(painel, "output/panel_estados_brasil.csv")
  message("\n✓ panel_estados_brasil.csv exportado")
  message("  Dimensões: ", nrow(painel), " obs × ", ncol(painel), " variáveis")
  message("  Período  : ", min(painel$year), "–", max(painel$year))
  message("  Estados  : ", n_distinct(painel$uf))

  # Verifica balanceamento
  contagem <- painel %>% count(uf)
  if (all(contagem$n == 23)) {
    message("  Painel balanceado: OK (23 obs por estado)")
  } else {
    message("  AVISO: painel NÃO balanceado!")
    print(contagem %>% filter(n != 23))
  }

  invisible(painel)
}

# Executa se rodado diretamente
if (!exists("SOURCED_PANEL")) {
  panel <- build_panel()
  SOURCED_PANEL <- TRUE
}
