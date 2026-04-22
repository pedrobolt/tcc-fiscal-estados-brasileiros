# =============================================================================
# 01_siconfi.R
# Coleta DCL, RCL, resultado primário e encargos da dívida
# Fontes: STN/SICONFI — RGF (Anexo 02) e RREO (Anexo 06)
# API: https://apidatalake.tesouro.gov.br/ords/siconfi/tt/
#
# ESTRUTURA REAL DA API (verificada empiricamente):
#   coluna "conta"  = descrição da linha (ex: "DÍVIDA CONSOLIDADA LÍQUIDA...")
#   coluna "coluna" = período/tipo (ex: "Até o 3º Quadrimestre", "VALOR")
#   coluna "rotulo" = sempre "Padrão" — NÃO usar para filtrar
#   valores estão em R$ (não R$ mil) → dividimos por 1000 para consistência
# =============================================================================

source("R/00_setup.R")

# =============================================================================
# FUNÇÕES DE COLETA — salvam a resposta bruta em cache por estado/ano
# =============================================================================

raw_path_rgf     <- function(id, yr) glue("data/raw/siconfi/rgf_{id}_{yr}.rds")
raw_path_rreo6   <- function(id, yr) glue("data/raw/siconfi/rreo6_{id}_{yr}.rds")
raw_path_encarg  <- function(id, yr) glue("data/raw/siconfi/enc_{id}_{yr}.rds")

fetch_and_cache <- function(path, url, query, sleep = 0.4) {
  if (file.exists(path)) return(readRDS(path))
  resp <- safe_get(url, query = query)
  df   <- parse_json_resp(resp)
  # Salva mesmo se NULL (evita re-tentar infinitamente em anos sem dados)
  saveRDS(df, path)
  Sys.sleep(sleep)
  df
}

fetch_rgf <- function(id_ente, year) {
  path <- raw_path_rgf(id_ente, year)
  if (file.exists(path)) return(readRDS(path))

  # Tenta quadrimestral (3º quad = dez), depois semestral (2º sem = dez)
  for (perd in list(
    list(per = "Q", nr = 3),
    list(per = "S", nr = 2)
  )) {
    resp <- safe_get(glue("{SICONFI_URL}/rgf"), query = list(
      an_exercicio          = year,
      in_periodicidade      = perd$per,
      nr_periodo            = perd$nr,
      co_tipo_demonstrativo = "RGF",
      co_esfera             = "E",
      co_poder              = "E",
      id_ente               = id_ente
    ))
    df <- parse_json_resp(resp)
    if (!is.null(df) && nrow(df) > 0) {
      saveRDS(df, path)
      Sys.sleep(0.4)
      return(df)
    }
    Sys.sleep(0.3)
  }
  saveRDS(NULL, path)
  NULL
}

fetch_rreo6 <- function(id_ente, year) {
  fetch_and_cache(
    raw_path_rreo6(id_ente, year),
    glue("{SICONFI_URL}/rreo"),
    list(an_exercicio=year, in_periodicidade="B", nr_periodo=6,
         co_tipo_demonstrativo="RREO", no_anexo="RREO-Anexo 06",
         co_esfera="E", co_poder="E", id_ente=id_ente)
  )
}

fetch_encargos <- function(id_ente, year) {
  # Encargos estão no mesmo Anexo 06 do RREO — reutiliza cache
  path <- raw_path_rreo6(id_ente, year)
  if (file.exists(path)) return(readRDS(path))
  fetch_rreo6(id_ente, year)
}

# =============================================================================
# FUNÇÕES DE EXTRAÇÃO — usam conta/coluna (não rotulo)
# =============================================================================

# Helper: pega valor de uma linha pela conta e coluna
get_siconfi_val <- function(df, conta_pat, coluna_pat, divisor = 1000) {
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  df <- janitor::clean_names(df)
  # Garante que colunas existem
  if (!all(c("conta", "coluna", "valor") %in% names(df))) return(NA_real_)
  rows <- df %>%
    filter(
      str_detect(conta,  regex(conta_pat,  ignore_case = TRUE)),
      str_detect(coluna, regex(coluna_pat, ignore_case = TRUE))
    )
  if (nrow(rows) == 0) return(NA_real_)
  val <- suppressWarnings(as.numeric(rows$valor[1]))
  if (is.na(val)) return(NA_real_)
  val / divisor   # converte R$ → R$ mil
}

extract_rgf_vars <- function(df, id_ente, uf, year) {
  # DCL: coluna "Até o 3º Quadrimestre" (ou 2º Semestre para periodicidade S)
  coluna_pat_dcl <- "3.*Quadrimestre|2.*Semestre|Quadrimestre de Refer"
  dcl    <- get_siconfi_val(df,
               "DÍVIDA CONSOLIDADA LÍQUIDA|DIVIDA CONSOLIDADA LIQUIDA",
               coluna_pat_dcl)
  # RCL: mesma coluna temporal do Anexo 02
  rcl    <- get_siconfi_val(df,
               "RECEITA CORRENTE LÍQ.*RCL.*\\(IV\\)|RECEITA CORRENTE LIQ.*RCL.*\\(IV\\)",
               coluna_pat_dcl)
  # Fallback RCL com padrão mais amplo
  if (is.na(rcl))
    rcl  <- get_siconfi_val(df,
               "RECEITA CORRENTE L.QUIDA - RCL",
               coluna_pat_dcl)
  tibble(id_ente=id_ente, uf=uf, year=year, dcl=dcl, rcl_rgf=rcl)
}

extract_primario <- function(df, id_ente, uf, year) {
  # "VALOR INCORRIDO" (2015-2019) ou "VALOR" (2020+) — padrão sem âncora
  col_pat_val <- "^VALOR"
  prim <- get_siconfi_val(df,
            "RESULTADO PRIMÁRIO.*Acima|RESULTADO PRIMARIO.*Acima",
            col_pat_val)
  # Fallback: "RESULTADO PRIMÁRIO (XIX)" — formato anterior ao Anexo 06 reformulado
  if (is.na(prim))
    prim <- get_siconfi_val(df,
              "RESULTADO PRIMÁRIO \\(XIX\\)|RESULTADO PRIMARIO \\(XIX\\)",
              "Até o Bimestre|^VALOR")
  # Fallback amplo
  if (is.na(prim))
    prim <- get_siconfi_val(df, "RESULTADO PRIM", col_pat_val)

  receitas <- get_siconfi_val(df, "RECEITA PRIMÁRIA TOTAL|RECEITAS PRIMÁRIAS TOTAL",
                               "REALIZADAS|^VALOR")
  despesas <- get_siconfi_val(df, "DESPESA PRIMÁRIA TOTAL",
                               "PAGAS|LIQUIDADAS|^VALOR")
  tibble(id_ente=id_ente, uf=uf, year=year,
         resultado_primario=prim,
         receitas_primarias=receitas,
         despesas_primarias=despesas)
}

extract_encargos <- function(df, id_ente, uf, year) {
  # "VALOR INCORRIDO" (2015-2019) ou "VALOR" (2020+)
  enc <- get_siconfi_val(df,
           "Juros, Encargos e Variações Monetárias Passivos",
           "^VALOR")
  if (is.na(enc))
    enc <- get_siconfi_val(df,
             "Juros.*Encargos|Encargos.*Dívida",
             "^VALOR|PAGAS|LIQUIDADAS")
  tibble(id_ente=id_ente, uf=uf, year=year, encargos_divida=enc)
}

# =============================================================================
# LOOP PRINCIPAL
# =============================================================================

collect_siconfi <- function(force_refresh = FALSE) {

  # Se force_refresh, apaga todos os caches individuais
  if (force_refresh) {
    old <- list.files("data/raw/siconfi",
                      pattern = "^(rgf|rreo6|enc)_\\d+_\\d+\\.rds$",
                      full.names = TRUE)
    file.remove(old)
    message("  Cache limpo: ", length(old), " arquivos removidos")
  }

  grid <- expand_grid(year = YEARS, id_ente = STATES$id_ente) %>%
    left_join(STATES, by = "id_ente")
  n <- nrow(grid)

  # ---- RGF -------------------------------------------------------------------
  message("\n=== [1/3] RGF (DCL + RCL) — ", n, " combinações ===")
  pb <- progress_bar$new(total=n, format="[:bar] :current/:total :uf :year eta::eta")
  raw_rgf <- map_dfr(seq_len(n), function(i) {
    row <- grid[i,]
    pb$tick(tokens=list(uf=row$uf, year=row$year))
    df_raw <- fetch_rgf(row$id_ente, row$year)
    extract_rgf_vars(df_raw, row$id_ente, row$uf, row$year)
  })

  # ---- RREO Anexo 06 ---------------------------------------------------------
  message("\n=== [2/3] RREO Anexo 06 (Resultado Primário) — ", n, " combinações ===")
  pb <- progress_bar$new(total=n, format="[:bar] :current/:total :uf :year eta::eta")
  raw_primario <- map_dfr(seq_len(n), function(i) {
    row <- grid[i,]
    pb$tick(tokens=list(uf=row$uf, year=row$year))
    df_raw <- fetch_rreo6(row$id_ente, row$year)
    extract_primario(df_raw, row$id_ente, row$uf, row$year)
  })

  # ---- Encargos (do mesmo Anexo 06) ------------------------------------------
  message("\n=== [3/3] Encargos da Dívida (RREO Anexo 06) ===")
  raw_encargos <- map_dfr(seq_len(n), function(i) {
    row <- grid[i,]
    df_raw <- readRDS(raw_path_rreo6(row$id_ente, row$year))  # usa cache
    extract_encargos(df_raw, row$id_ente, row$uf, row$year)
  })

  # ---- Merge e exporta -------------------------------------------------------
  siconfi <- raw_rgf %>%
    left_join(raw_primario, by = c("id_ente","uf","year")) %>%
    left_join(raw_encargos, by = c("id_ente","uf","year")) %>%
    arrange(uf, year)

  write_csv(siconfi, "data/processed/siconfi_processado.csv")

  # Sumário de cobertura
  cob <- siconfi %>% summarise(across(c(dcl,rcl_rgf,resultado_primario,encargos_divida),
                                       ~round(100*mean(!is.na(.)), 1)))
  message("\n✓ siconfi_processado.csv — ", nrow(siconfi), " linhas")
  message("  Cobertura (%): DCL=", cob$dcl, " RCL=", cob$rcl_rgf,
          " Primário=", cob$resultado_primario, " Encargos=", cob$encargos_divida)
  invisible(siconfi)
}

if (!exists("SOURCED_SICONFI")) {
  siconfi_data <- collect_siconfi()
  SOURCED_SICONFI <- TRUE
}
