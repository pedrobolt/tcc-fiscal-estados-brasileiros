library(httr)
library(jsonlite)

resp <- GET("https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf",
  query = list(
    an_exercicio = 2022, in_periodicidade = "Q", nr_periodo = 3,
    co_tipo_demonstrativo = "RGF", co_esfera = "E", co_poder = "E", id_ente = 35
  ), timeout(30)
)
df <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)$items

cat("Rotulos unicos:\n")
print(unique(df$rotulo))

cat("\nColunas unicas:\n")
print(unique(df$coluna))

cat("\nLinhas com 'iquid' no rotulo:\n")
sub <- df[grepl("iquid", df$rotulo, ignore.case = TRUE), c("rotulo", "coluna", "conta", "valor")]
print(sub)
