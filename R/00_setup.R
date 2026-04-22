# =============================================================================
# 00_setup.R
# Instala pacotes, define constantes, cria estrutura de pastas
# =============================================================================

# ---- Pacotes necessários ----------------------------------------------------
pkgs <- c(
  "httr", "jsonlite", "dplyr", "tidyr", "purrr",
  "readr", "lubridate", "stringr", "janitor", "glue",
  "progress", "gt", "knitr", "scales"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  message("Instalando: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Diretórios -------------------------------------------------------------
dirs <- c(
  "data/raw/siconfi",
  "data/raw/ibge",
  "data/raw/bcb",
  "data/processed",
  "output"
)
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ---- Constantes: estados ----------------------------------------------------
# 27 unidades da federação com código IBGE (id_ente no SICONFI)
STATES <- tribble(
  ~uf,  ~id_ente, ~nome,
  "RO",  11, "Rondônia",
  "AC",  12, "Acre",
  "AM",  13, "Amazonas",
  "RR",  14, "Roraima",
  "PA",  15, "Pará",
  "AP",  16, "Amapá",
  "TO",  17, "Tocantins",
  "MA",  21, "Maranhão",
  "PI",  22, "Piauí",
  "CE",  23, "Ceará",
  "RN",  24, "Rio Grande do Norte",
  "PB",  25, "Paraíba",
  "PE",  26, "Pernambuco",
  "AL",  27, "Alagoas",
  "SE",  28, "Sergipe",
  "BA",  29, "Bahia",
  "MG",  31, "Minas Gerais",
  "ES",  32, "Espírito Santo",
  "RJ",  33, "Rio de Janeiro",
  "SP",  35, "São Paulo",
  "PR",  41, "Paraná",
  "SC",  42, "Santa Catarina",
  "RS",  43, "Rio Grande do Sul",
  "MS",  50, "Mato Grosso do Sul",
  "MT",  51, "Mato Grosso",
  "GO",  52, "Goiás",
  "DF",  53, "Distrito Federal"
)

# ---- Constantes: período ----------------------------------------------------
YEARS       <- 2002:2024
YEARS_STR   <- as.character(YEARS)

# ---- Constantes: variáveis eleitorais ---------------------------------------
# Eleições estaduais: 1998, 2002, 2006, 2010, 2014, 2018, 2022 (ciclo 4 anos)
ELECTION_YEARS <- c(2002, 2006, 2010, 2014, 2018, 2022)

# ---- URLs base --------------------------------------------------------------
SICONFI_URL <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt"
SIDRA_URL   <- "https://servicodados.ibge.gov.br/api/v3/agregados"
BCB_URL     <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs"

# ---- Utilitários ------------------------------------------------------------

# Faz GET com retry automático em caso de erro 429/5xx
safe_get <- function(url, query = list(), max_tries = 4, sleep = 1.5) {
  for (i in seq_len(max_tries)) {
    resp <- tryCatch(
      httr::GET(url, query = query, httr::timeout(60)),
      error = function(e) NULL
    )
    if (is.null(resp)) {
      Sys.sleep(sleep * i); next
    }
    if (httr::status_code(resp) == 200) return(resp)
    if (httr::status_code(resp) %in% c(429, 503)) {
      wait <- as.numeric(httr::headers(resp)[["retry-after"]]) %||% (sleep * 2^i)
      message(glue("  HTTP {httr::status_code(resp)} — aguardando {round(wait)}s"))
      Sys.sleep(wait)
    } else {
      message(glue("  HTTP {httr::status_code(resp)} para {url}"))
      return(NULL)
    }
  }
  NULL
}

# Extrai data frame de resposta JSON (campo "items" ou array raiz)
parse_json_resp <- function(resp) {
  if (is.null(resp)) return(NULL)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  obj <- tryCatch(jsonlite::fromJSON(txt, flatten = TRUE), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  if (is.data.frame(obj)) return(obj)
  if (!is.null(obj$items) && is.data.frame(obj$items)) return(obj$items)
  NULL
}

# Operador %||% (null-coalescing)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

message("✓ setup.R carregado — ", nrow(STATES), " UFs, ",
        min(YEARS), "-", max(YEARS))
