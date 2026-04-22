library(jsonlite); library(dplyr); library(janitor)

# Usa cache SP 2018 (já baixado)
df <- readRDS("data/raw/siconfi/rreo6_35_2018.rds")
if (is.null(df)) { cat("NULL\n"); q() }
df <- janitor::clean_names(df)
cat("Colunas:", paste(names(df), collapse=", "), "\n")
cat("Colunas únicas (coluna):\n"); print(unique(df$coluna))
cat("\nContas com 'Primário'/'primário':\n")
print(df[grepl("prim", df$conta, ignore.case=TRUE), c("conta","coluna","valor")])
cat("\nContas com 'Resultado':\n")
print(df[grepl("resultado", df$conta, ignore.case=TRUE), c("conta","coluna","valor")])
