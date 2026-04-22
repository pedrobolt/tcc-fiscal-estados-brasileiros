# =============================================================================
# 05_quality_report.R
# Relatório de qualidade dos dados: missing values, cobertura, estatísticas
# Exporta: output/quality_report.html e output/quality_report.csv
# =============================================================================

source("R/00_setup.R")
library(knitr)

generate_quality_report <- function() {

  message("\n=== Gerando relatório de qualidade ===")

  panel <- read_csv("output/panel_estados_brasil.csv", show_col_types = FALSE)

  vars_fiscais <- c("dcl", "rcl_rgf", "resultado_primario",
                    "encargos_divida", "dcl_sobre_rcl",
                    "primario_sobre_rcl", "encargos_sobre_rcl")
  vars_pib     <- c("pib_nominal_mil", "pib_real_mil_2010",
                    "crescimento_pib_pct", "ln_pib_real")
  vars_ipca    <- c("ipca_aa", "deflator_2010")
  vars_all     <- c(vars_fiscais, vars_pib, vars_ipca)

  # ===========================================================================
  # 1. Cobertura geral por variável
  # ===========================================================================
  miss_var <- map_dfr(vars_all, function(v) {
    x <- panel[[v]]
    tibble(
      variavel      = v,
      n_total       = length(x),
      n_presente    = sum(!is.na(x)),
      n_ausente     = sum(is.na(x)),
      cobertura_pct = round(100 * mean(!is.na(x)), 1)
    )
  })

  message("\n--- Cobertura por variável ---")
  print(miss_var, n = 30)

  # ===========================================================================
  # 2. Cobertura por estado (variável mais crítica: DCL)
  # ===========================================================================
  miss_uf <- panel %>%
    group_by(uf, nome) %>%
    summarise(
      obs_total     = n(),
      dcl_pct       = round(100 * mean(!is.na(dcl)), 1),
      rcl_pct       = round(100 * mean(!is.na(rcl_rgf)), 1),
      primario_pct  = round(100 * mean(!is.na(resultado_primario)), 1),
      pib_pct       = round(100 * mean(!is.na(pib_nominal_mil)), 1),
      .groups = "drop"
    ) %>%
    arrange(uf)

  message("\n--- Cobertura por estado (%) ---")
  print(miss_uf, n = 30)

  # ===========================================================================
  # 3. Cobertura por ano (quantos estados têm dados em cada ano)
  # ===========================================================================
  miss_year <- panel %>%
    group_by(year) %>%
    summarise(
      estados_dcl      = sum(!is.na(dcl)),
      estados_rcl      = sum(!is.na(rcl_rgf)),
      estados_primario = sum(!is.na(resultado_primario)),
      estados_pib      = sum(!is.na(pib_nominal_mil)),
      .groups = "drop"
    )

  message("\n--- Estados com dados por ano ---")
  print(miss_year, n = 30)

  # ===========================================================================
  # 4. Estatísticas descritivas por variável
  # ===========================================================================
  desc_stats <- map_dfr(vars_all, function(v) {
    x <- panel[[v]][!is.na(panel[[v]])]
    if (length(x) == 0) return(NULL)
    tibble(
      variavel = v,
      n        = length(x),
      media    = round(mean(x), 4),
      mediana  = round(median(x), 4),
      dp       = round(sd(x), 4),
      min      = round(min(x), 4),
      p25      = round(quantile(x, 0.25), 4),
      p75      = round(quantile(x, 0.75), 4),
      max      = round(max(x), 4)
    )
  })

  message("\n--- Estatísticas descritivas ---")
  print(desc_stats, n = 30)

  # ===========================================================================
  # 5. Detecta valores suspeitos (outliers extremos)
  # ===========================================================================
  outliers <- map_dfr(c("dcl_sobre_rcl", "primario_sobre_rcl", "encargos_sobre_rcl"), function(v) {
    x <- panel[[v]]
    q <- quantile(x, c(0.01, 0.99), na.rm = TRUE)
    panel %>%
      filter(!is.na(.data[[v]]),
             .data[[v]] < q[1] | .data[[v]] > q[2]) %>%
      mutate(variavel = v, valor = .data[[v]]) %>%
      select(variavel, uf, year, valor)
  })

  if (nrow(outliers) > 0) {
    message("\n--- Potenciais outliers (p1/p99) ---")
    print(outliers %>% arrange(variavel, abs(valor)), n = 30)
  }

  # ===========================================================================
  # 6. Exporta relatório em CSV
  # ===========================================================================
  write_csv(miss_var,    "output/qr_cobertura_variaveis.csv")
  write_csv(miss_uf,     "output/qr_cobertura_estados.csv")
  write_csv(miss_year,   "output/qr_cobertura_anos.csv")
  write_csv(desc_stats,  "output/qr_estatisticas_descritivas.csv")
  write_csv(outliers,    "output/qr_outliers.csv")

  # ===========================================================================
  # 7. Gera HTML simples com knitr
  # ===========================================================================
  rmd_content <- '
---
title: "Relatório de Qualidade — Painel Estados Brasileiros"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
library(dplyr); library(readr); library(knitr)
miss_var   <- read_csv("output/qr_cobertura_variaveis.csv")
miss_uf    <- read_csv("output/qr_cobertura_estados.csv")
miss_year  <- read_csv("output/qr_cobertura_anos.csv")
desc_stats <- read_csv("output/qr_estatisticas_descritivas.csv")
outliers   <- read_csv("output/qr_outliers.csv")
```

## 1. Cobertura por variável
```{r}
kable(miss_var, caption = "Missings por variável")
```

## 2. Cobertura por estado
```{r}
kable(miss_uf, caption = "% de anos com dado por estado")
```

## 3. Cobertura por ano
```{r}
kable(miss_year, caption = "Nº de estados com dado por ano")
```

## 4. Estatísticas descritivas
```{r}
kable(desc_stats, caption = "Estatísticas descritivas")
```

## 5. Outliers potenciais (p1/p99)
```{r}
if(nrow(outliers) > 0) kable(outliers) else cat("Nenhum outlier detectado.")
```
'
  writeLines(rmd_content, "output/quality_report.Rmd")

  tryCatch({
    rmarkdown::render("output/quality_report.Rmd",
                      output_file = "quality_report.html",
                      output_dir  = "output",
                      quiet       = TRUE)
    message("✓ quality_report.html gerado")
  }, error = function(e) {
    message("  AVISO: rmarkdown não disponível — CSVs exportados em output/")
  })

  message("\n✓ Relatório de qualidade completo. Arquivos em output/")
  invisible(list(
    miss_var   = miss_var,
    miss_uf    = miss_uf,
    miss_year  = miss_year,
    desc_stats = desc_stats,
    outliers   = outliers
  ))
}

if (!exists("SOURCED_QR")) {
  qr <- generate_quality_report()
  SOURCED_QR <- TRUE
}
