library(httr); library(jsonlite)

get_rgf <- function(id_ente, year) {
  resp <- GET("https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf",
    query = list(an_exercicio=year, in_periodicidade="Q", nr_periodo=3,
                 co_tipo_demonstrativo="RGF", co_esfera="E", co_poder="E",
                 id_ente=id_ente), timeout(30))
  fromJSON(content(resp, as="text", encoding="UTF-8"), flatten=TRUE)$items
}

get_rreo <- function(id_ente, year, anexo_nome) {
  resp <- GET("https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rreo",
    query = list(an_exercicio=year, in_periodicidade="B", nr_periodo=6,
                 co_tipo_demonstrativo="RREO", no_anexo=anexo_nome,
                 co_esfera="E", co_poder="E", id_ente=id_ente), timeout(30))
  fromJSON(content(resp, as="text", encoding="UTF-8"), flatten=TRUE)$items
}

# SP 2022
df_rgf <- get_rgf(35, 2022)

cat("=== RGF Anexo 02 — DCL e RCL ===\n")
a2 <- df_rgf[df_rgf$anexo == "RGF-Anexo 02", c("conta","coluna","valor")]
print(a2)

cat("\n=== RREO Anexo 06 — Resultado Primário ===\n")
df_rreo6 <- get_rreo(35, 2022, "RREO-Anexo 06")
if (!is.null(df_rreo6) && nrow(df_rreo6) > 0) {
  cat("Colunas:", paste(names(df_rreo6), collapse=", "), "\n")
  cat("Contas únicas:\n"); print(unique(df_rreo6$conta))
  cat("\nColunas únicas:\n"); print(unique(df_rreo6$coluna))
  # Resultado primário
  sub <- df_rreo6[grepl("Resultado|RESULTADO|Primário|PRIMÁRIO", df_rreo6$conta), c("conta","coluna","valor")]
  cat("\nLinhas resultado primário:\n"); print(sub)
} else cat("RREO Anexo 06 vazio\n")

cat("\n=== RREO Anexo 10 — Encargos ===\n")
df_rreo10 <- get_rreo(35, 2022, "RREO-Anexo 10")
if (!is.null(df_rreo10) && nrow(df_rreo10) > 0) {
  cat("Contas:\n"); print(unique(df_rreo10$conta))
} else cat("RREO Anexo 10 vazio\n")
