library(httr)
library(jsonlite)

# Testa a API SICONFI ao vivo para SP, 2022
url <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf"
resp <- GET(url, query = list(
  an_exercicio          = 2022,
  in_periodicidade      = "Q",
  nr_periodo            = 3,
  co_tipo_demonstrativo = "RGF",
  co_esfera             = "E",
  co_poder              = "E",
  id_ente               = 35
), timeout(30))

cat("Status:", status_code(resp), "\n")
txt <- content(resp, as = "text", encoding = "UTF-8")
obj <- fromJSON(txt, flatten = TRUE)

cat("\nClasse do objeto:", class(obj), "\n")
cat("Nomes do objeto:", paste(names(obj), collapse = ", "), "\n\n")

if (!is.null(obj$items)) {
  df <- obj$items
  cat("Dimensoes items:", nrow(df), "x", ncol(df), "\n")
  cat("Colunas:", paste(names(df), collapse = ", "), "\n\n")
  cat("Primeiras 10 linhas:\n")
  print(head(df[, c(1:min(8, ncol(df)))], 10))
} else if (is.data.frame(obj)) {
  cat("Dimensoes:", nrow(obj), "x", ncol(obj), "\n")
  cat("Colunas:", paste(names(obj), collapse = ", "), "\n\n")
  print(head(obj[, c(1:min(8, ncol(obj)))], 10))
} else {
  cat("Estrutura:\n")
  str(obj)
}
