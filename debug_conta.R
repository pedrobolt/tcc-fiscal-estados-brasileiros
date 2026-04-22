library(httr)
library(jsonlite)

resp <- GET("https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf",
  query = list(
    an_exercicio = 2022, in_periodicidade = "Q", nr_periodo = 3,
    co_tipo_demonstrativo = "RGF", co_esfera = "E", co_poder = "E", id_ente = 35
  ), timeout(30)
)
df <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)$items

cat("Contas unicas (primeiras 40):\n")
print(unique(df$conta)[1:40])

cat("\n\nAnexos presentes:\n")
print(unique(df$anexo))

# Filtra coluna = "TOTAL (ÚLTIMOS 12 MESES) (a)" para ver valores de cada conta
cat("\n\nLinhas com TOTAL 12 MESES:\n")
sub <- df[df$coluna == "TOTAL (ÚLTIMOS 12 MESES) (a)", c("anexo","conta","coluna","valor")]
print(sub)

# Também testa coluna = "Valor"
cat("\nLinhas com coluna='Valor':\n")
sub2 <- df[df$coluna == "Valor", c("anexo","conta","coluna","valor")]
print(head(sub2, 20))
