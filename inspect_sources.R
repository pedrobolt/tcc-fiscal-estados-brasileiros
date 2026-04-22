options(warn=-1)
library(httr); library(readr); library(stringr)

get_raw <- function(url, enc="latin1") {
  resp <- GET(url, timeout(30))
  content(resp, as="text", encoding=enc)
}

# ---- SERVICO DA DIVIDA ----
cat("=== SERVICO DA DIVIDA ===\n")
txt <- get_raw("https://www.tesourotransparente.gov.br/ckan/dataset/38354dac-a7d2-42cd-bc66-999f23914505/resource/b629e251-0ef4-455d-9376-adf17c635863/download/relatorio-servico-final.csv")
lines <- strsplit(txt, "\n")[[1]]
cat("Total rows:", length(lines), "\n")
for(i in 1:5) cat(sprintf("Row %d: %s\n", i, substr(lines[i],1,180)))
total_idx <- which(grepl("TOTAL.*Adm|DVITA TOTAL|VIDA TOTAL", lines, ignore.case=TRUE))
cat("TOTAL row indices:", head(total_idx, 5), "\n")
if(length(total_idx)>0) cat("Row", total_idx[1], ":", substr(lines[total_idx[1]],1,300), "\n")

# Count unique years (col 2 of TOTAL row should have year)
# Split that row to count columns
if(length(total_idx)>0) {
  sp <- strsplit(lines[total_idx[1]], ";")[[1]]
  cat("N cols in TOTAL row:", length(sp), "\n")
  cat("Year (col2):", sp[2], "\n")
  cat("Cols 3-8:", paste(sp[3:min(8,length(sp))], collapse=" | "), "\n")
}

# ---- ESTOQUE DA DIVIDA ----
cat("\n=== ESTOQUE DA DIVIDA ===\n")
txt2 <- get_raw("https://www.tesourotransparente.gov.br/ckan/dataset/0e763dcf-c7f9-4928-aa3d-61318de7141f/resource/f9a981c0-6281-43b2-9238-5270eacd4a77/download/relatorio-estoque-final.csv")
lines2 <- strsplit(txt2, "\n")[[1]]
cat("Total rows:", length(lines2), "\n")
for(i in 1:7) cat(sprintf("Row %d: %s\n", i, substr(lines2[i],1,200)))
df_rows <- which(grepl(";DF;", lines2))
cat("Rows with DF:", head(df_rows, 3), "\n")
if(length(df_rows)>0) cat("Sample DF row:", substr(lines2[df_rows[1]],1,200), "\n")
