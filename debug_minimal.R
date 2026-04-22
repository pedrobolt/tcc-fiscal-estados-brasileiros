# Script mínimo — sem progress, sem gt, sem scales
# Testa extração em SP 2022 usando cache já baixado
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(janitor)
library(glue)
library(readr)

cat("Pacotes OK\n")

SICONFI_URL <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt"

safe_get <- function(url, query=list()) {
  tryCatch(httr::GET(url, query=query, httr::timeout(30)), error=function(e) NULL)
}
parse_json_resp <- function(resp) {
  if (is.null(resp)) return(NULL)
  obj <- tryCatch(jsonlite::fromJSON(httr::content(resp, as="text", encoding="UTF-8"), flatten=TRUE), error=function(e) NULL)
  if (is.null(obj)) return(NULL)
  if (!is.null(obj$items) && is.data.frame(obj$items)) return(obj$items)
  if (is.data.frame(obj)) return(obj)
  NULL
}

get_siconfi_val <- function(df, conta_pat, coluna_pat) {
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  df <- janitor::clean_names(df)
  if (!all(c("conta","coluna","valor") %in% names(df))) {
    cat("  Colunas disponíveis:", paste(names(df), collapse=", "), "\n")
    return(NA_real_)
  }
  rows <- df %>%
    filter(str_detect(conta,  regex(conta_pat,  ignore_case=TRUE)),
           str_detect(coluna, regex(coluna_pat, ignore_case=TRUE)))
  if (nrow(rows) == 0) return(NA_real_)
  as.numeric(rows$valor[1]) / 1000
}

# Usa cache RGF do SP 2022 se existir, senão baixa
cache_sp <- "data/raw/siconfi/rgf_35_2022.rds"
if (file.exists(cache_sp)) {
  cat("Usando cache RGF SP 2022\n")
  df_rgf <- readRDS(cache_sp)
} else {
  cat("Baixando RGF SP 2022...\n")
  resp <- safe_get(glue("{SICONFI_URL}/rgf"), query=list(
    an_exercicio=2022, in_periodicidade="Q", nr_periodo=3,
    co_tipo_demonstrativo="RGF", co_esfera="E", co_poder="E", id_ente=35))
  df_rgf <- parse_json_resp(resp)
  if (!is.null(df_rgf)) saveRDS(df_rgf, cache_sp)
}

cat("RGF rows:", nrow(df_rgf), "\n")

dcl <- get_siconfi_val(df_rgf,
  "DÍVIDA CONSOLIDADA LÍQUIDA|DIVIDA CONSOLIDADA LIQUIDA",
  "3.*Quadrimestre|2.*Semestre")
rcl <- get_siconfi_val(df_rgf,
  "RECEITA CORRENTE L.QUIDA - RCL",
  "3.*Quadrimestre|2.*Semestre")

cat(sprintf("SP 2022 — DCL: R$ %s mil | RCL: R$ %s mil\n",
            format(round(dcl), big.mark="."),
            format(round(rcl), big.mark=".")))
if (!is.na(dcl) && !is.na(rcl) && rcl > 0)
  cat(sprintf("DCL/RCL = %.3f\n", dcl/rcl))

# Teste RREO Anexo 06
cache_r6 <- "data/raw/siconfi/rreo6_35_2022.rds"
if (file.exists(cache_r6)) {
  df_r6 <- readRDS(cache_r6)
} else {
  cat("Baixando RREO Anexo 06 SP 2022...\n")
  resp6 <- safe_get(glue("{SICONFI_URL}/rreo"), query=list(
    an_exercicio=2022, in_periodicidade="B", nr_periodo=6,
    co_tipo_demonstrativo="RREO", no_anexo="RREO-Anexo 06",
    co_esfera="E", co_poder="E", id_ente=35))
  df_r6 <- parse_json_resp(resp6)
  if (!is.null(df_r6)) saveRDS(df_r6, cache_r6)
}

if (!is.null(df_r6)) {
  cat("RREO Anexo 06 rows:", nrow(df_r6), "\n")
  prim <- get_siconfi_val(df_r6, "RESULTADO PRIM.*Acima|RESULTADO PRIM", "^VALOR$")
  enc  <- get_siconfi_val(df_r6, "Juros.*Encargos.*Passivos|Juros.*Encargos", "^VALOR$")
  cat(sprintf("SP 2022 — Primário: R$ %s mil | Encargos: R$ %s mil\n",
              format(round(prim), big.mark="."),
              format(round(enc),  big.mark=".")))
} else {
  cat("RREO Anexo 06: NULL\n")
}

cat("\nTeste OK\n")
