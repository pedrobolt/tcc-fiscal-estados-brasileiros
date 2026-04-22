# Re-processa extração a partir do cache (sem API calls)
library(httr); library(jsonlite); library(dplyr); library(tidyr)
library(purrr); library(readr); library(stringr); library(janitor); library(glue)

source("R/00_setup.R", local=TRUE)

# Funções auxiliares (copiadas de 01_siconfi.R sem o progress package)
raw_path_rgf   <- function(id, yr) glue("data/raw/siconfi/rgf_{id}_{yr}.rds")
raw_path_rreo6 <- function(id, yr) glue("data/raw/siconfi/rreo6_{id}_{yr}.rds")

get_siconfi_val <- function(df, conta_pat, coluna_pat, divisor=1000) {
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  df <- janitor::clean_names(df)
  if (!all(c("conta","coluna","valor") %in% names(df))) return(NA_real_)
  rows <- df %>%
    filter(str_detect(conta,  regex(conta_pat,  ignore_case=TRUE)),
           str_detect(coluna, regex(coluna_pat, ignore_case=TRUE)))
  if (nrow(rows) == 0) return(NA_real_)
  val <- suppressWarnings(as.numeric(rows$valor[1]))
  if (is.na(val)) return(NA_real_)
  val / divisor
}

extract_rgf_vars <- function(df, id_ente, uf, year) {
  col_dcl <- "3.*Quadrimestre|2.*Semestre|Quadrimestre de Refer"
  dcl <- get_siconfi_val(df, "DÍVIDA CONSOLIDADA LÍQUIDA|DIVIDA CONSOLIDADA LIQUIDA", col_dcl)
  rcl <- get_siconfi_val(df, "RECEITA CORRENTE L.QUIDA - RCL", col_dcl)
  tibble(id_ente=id_ente, uf=uf, year=year, dcl=dcl, rcl_rgf=rcl)
}

extract_primario <- function(df, id_ente, uf, year) {
  col_val <- "^VALOR"
  prim <- get_siconfi_val(df, "RESULTADO PRIMÁRIO.*Acima|RESULTADO PRIMARIO.*Acima", col_val)
  if (is.na(prim))
    prim <- get_siconfi_val(df, "RESULTADO PRIMÁRIO \\(XIX\\)|RESULTADO PRIMARIO \\(XIX\\)",
                             "Até o Bimestre|^VALOR")
  if (is.na(prim))
    prim <- get_siconfi_val(df, "RESULTADO PRIM", col_val)
  receitas <- get_siconfi_val(df, "RECEITA PRIMÁRIA TOTAL|RECEITAS PRIMÁRIAS TOTAL", "REALIZADAS|^VALOR")
  despesas <- get_siconfi_val(df, "DESPESA PRIMÁRIA TOTAL", "PAGAS|LIQUIDADAS|^VALOR")
  tibble(id_ente=id_ente, uf=uf, year=year,
         resultado_primario=prim, receitas_primarias=receitas, despesas_primarias=despesas)
}

extract_encargos <- function(df, id_ente, uf, year) {
  enc <- get_siconfi_val(df, "Juros, Encargos e Variações Monetárias Passivos", "^VALOR")
  if (is.na(enc))
    enc <- get_siconfi_val(df, "Juros.*Encargos|Encargos.*Dívida", "^VALOR|PAGAS|LIQUIDADAS")
  tibble(id_ente=id_ente, uf=uf, year=year, encargos_divida=enc)
}

# Grid completo
grid <- expand_grid(year=YEARS, id_ente=STATES$id_ente) %>%
  left_join(STATES, by="id_ente")
n <- nrow(grid)

cat("Re-processando RGF (", n, " linhas do cache)...\n")
raw_rgf <- map_dfr(seq_len(n), function(i) {
  row <- grid[i,]
  df  <- if (file.exists(raw_path_rgf(row$id_ente, row$year)))
           readRDS(raw_path_rgf(row$id_ente, row$year)) else NULL
  extract_rgf_vars(df, row$id_ente, row$uf, row$year)
})

cat("Re-processando Primário + Encargos...\n")
raw_prim_enc <- map_dfr(seq_len(n), function(i) {
  row <- grid[i,]
  df  <- if (file.exists(raw_path_rreo6(row$id_ente, row$year)))
           readRDS(raw_path_rreo6(row$id_ente, row$year)) else NULL
  p <- extract_primario(df, row$id_ente, row$uf, row$year)
  e <- extract_encargos(df, row$id_ente, row$uf, row$year)
  left_join(p, e, by=c("id_ente","uf","year"))
})

siconfi <- raw_rgf %>%
  left_join(raw_prim_enc, by=c("id_ente","uf","year")) %>%
  arrange(uf, year)

write_csv(siconfi, "data/processed/siconfi_processado.csv")

cob <- siconfi %>% summarise(across(c(dcl,rcl_rgf,resultado_primario,encargos_divida),
                                     ~round(100*mean(!is.na(.)),1)))
cat("✓ siconfi_processado.csv —", nrow(siconfi), "linhas\n")
cat("  Cobertura (%): DCL=", cob$dcl, " RCL=", cob$rcl_rgf,
    " Primário=", cob$resultado_primario, " Encargos=", cob$encargos_divida, "\n")

# Cobertura por ano
cob_ano <- siconfi %>% group_by(year) %>%
  summarise(dcl=sum(!is.na(dcl)), prim=sum(!is.na(resultado_primario)), .groups="drop")
print(cob_ano, n=23)
