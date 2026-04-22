library(httr); library(jsonlite)
resp <- GET("https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf",
  query = list(an_exercicio=2022, in_periodicidade="Q", nr_periodo=3,
               co_tipo_demonstrativo="RGF", co_esfera="E", co_poder="E",
               id_ente=35), timeout(30))
df <- fromJSON(content(resp, as="text", encoding="UTF-8"), flatten=TRUE)$items
a2 <- df[df$anexo == "RGF-Anexo 02", c("conta","coluna","valor")]

cat("Colunas únicas no Anexo 02:\n")
print(unique(a2$coluna))

cat("\nLinhas com DCL (líquida):\n")
print(a2[grepl("LÍQUIDA|LIQUIDA", a2$conta), ])

cat("\nLinhas com RCL:\n")
print(a2[grepl("CORRENTE", a2$conta), ])
