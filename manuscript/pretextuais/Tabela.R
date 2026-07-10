# =============================================================================
# scripts/gerar_tabela_refinanciamento.R
# Gera tabela LaTeX automaticamente para o TCC
# Saída:
#   /tables/tabela_refinanciamento.tex
# =============================================================================

# Instalar pacotes se necessário
pkgs <- c("dplyr", "kableExtra")

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]

if(length(to_install) > 0){
  install.packages(to_install)
}

library(dplyr)
library(kableExtra)

# =============================================================================
# Dados
# =============================================================================

tab_refin <- data.frame(
  
  UF = c(
    "Acre","Amazonas","Pará","Rondônia","Amapá","Roraima","Tocantins",
    "Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
    "Pernambuco","Alagoas","Sergipe","Bahia",
    "Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo",
    "Paraná","Santa Catarina","Rio Grande do Sul",
    "Mato Grosso","Mato Grosso do Sul","Goiás","Distrito Federal"
  ),
  
  Prazo = c(
    30,30,30,30,NA,30,NA,
    30,15,15,15,30,
    30,30,30,30,
    30,30,30,30,
    30,30,30,
    30,30,30,30
  ),
  
  Limite = c(
    "12,0","12,0","15,0","15,0","-","12,0","-",
    "13,0","13,0","12,0","11,5 a 13,0","11,0 a 13,0",
    "12,0","15,0","11,5 a 13,0","11,5 a 13,0",
    "6,79 a 13,0","13,0","12,0 a 13,0","8,86 a 13,0",
    "12,0 a 13,0","12,0 a 13,0","12,0 a 13,0",
    "15,0","14,0 a 15,0","13,0 a 15,0","13,0"
  ),
  
  Encargos = c(
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 7,5% a.a.",
    "IGP-DI + 6,0% a.a.",
    "-",
    "IGP-DI + 6,0% a.a.",
    "-",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 7,5% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 7,5% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a.",
    "IGP-DI + 6,0% a.a."
  ),
  
  stringsAsFactors = FALSE
)

# =============================================================================
# Ajustes
# =============================================================================

tab_refin <- tab_refin %>%
  mutate(
    Prazo = ifelse(is.na(Prazo), "--", Prazo)
  )

# =============================================================================
# Gerar tabela LaTeX
# =============================================================================

latex_table <- tab_refin %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    escape = TRUE,
    caption = "Condições de refinanciamento da dívida estadual",
    col.names = c(
      "UF",
      "Prazo (anos)",
      "Limite de comprometimento (%)",
      "Encargos"
    ),
    align = c("l", "c", "c", "l"),
    linesep = ""
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE,
    font_size = 9
  )


# =============================================================================
# Salvar .tex
# =============================================================================

writeLines(
  as.character(latex_table),
  "pretextuais/tabela_refinanciamento.tex"
)

cat("\nTabela LaTeX gerada com sucesso em:\n")
cat("tabela_refinanciamento.tex\n\n")