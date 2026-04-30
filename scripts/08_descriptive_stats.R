# =============================================================================
# scripts/08_descriptive_stats.R
# Tabela de estatísticas descritivas (Paineis A, B, C)
# Saída: output/tables/estatisticas_descritivas.{html,tex}
# =============================================================================
pkgs <- c("dplyr","readr","tidyr")
suppressPackageStartupMessages(lapply(pkgs[pkgs %in% rownames(installed.packages())], library, character.only=TRUE))
setwd("C:/Users/pltun/tcc")
dir.create("output/tables", recursive=TRUE, showWarnings=FALSE)

panel <- read_csv("data/processed/panel_slim.csv", show_col_types=FALSE)

vars  <- c("dcl_sobre_rcl_ext","primario_sobre_rcl_ext","encargos_sobre_rcl_ext",
           "crescimento_pib_pct","yvar","teto")
labs  <- c("DCL/RCL","Prim\u00e1rio/RCL","Encargos/RCL",
           "Crescimento PIB (%)","Hiato do produto","Teto contratual (%)")

# ── PANEL A ──────────────────────────────────────────────────────────────────
panA <- lapply(seq_along(vars), function(i) {
  x <- panel[[vars[i]]]
  x <- x[!is.na(x)]
  c(N=length(x), Mean=mean(x), SD=sd(x),
    Min=min(x), Median=median(x), Max=max(x))
})
panA <- do.call(rbind, panA)
rownames(panA) <- labs

# ── PANEL B ──────────────────────────────────────────────────────────────────
teto_groups <- c(12L,13L,15L)
panB_vars   <- c("dcl_sobre_rcl_ext","primario_sobre_rcl_ext")
panB_labs   <- c("DCL/RCL","Prim\u00e1rio/RCL")

panB <- lapply(panB_vars, function(v) {
  sapply(teto_groups, function(t) {
    x <- panel[[v]][panel$teto == t & !is.na(panel[[v]])]
    c(N=length(x), Mean=mean(x), SD=sd(x))
  })
})

# t-tests DCL/RCL
dcl <- panel$dcl_sobre_rcl_ext
t12v15 <- t.test(dcl[panel$teto==12 & !is.na(dcl)],
                 dcl[panel$teto==15 & !is.na(dcl)])
t12v13 <- t.test(dcl[panel$teto==12 & !is.na(dcl)],
                 dcl[panel$teto==13 & !is.na(dcl)])
cat("t-test DCL (12 vs 15): p=", round(t12v15$p.value,4), "\n")
cat("t-test DCL (12 vs 13): p=", round(t12v13$p.value,4), "\n")

# ── PANEL C ──────────────────────────────────────────────────────────────────
mat <- panel[vars]; mat <- mat[complete.cases(mat),]
cormat <- round(cor(mat), 3)
colnames(cormat) <- rownames(cormat) <- labs

fmt <- function(x, d=3) formatC(x, digits=d, format="f")
star <- function(p) ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.10,"*","")))

# ── HTML OUTPUT ───────────────────────────────────────────────────────────────
css <- '
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
  body { font-family: "Palatino Linotype","Book Antiqua",Palatino,Georgia,"Times New Roman",serif;
         font-size:14px; color:#111; background:#ede9e1; padding:56px 24px; line-height:1.5; }
  .page { max-width:1000px; margin:0 auto; background:#fff; padding:56px 64px 48px;
          box-shadow:0 2px 24px rgba(0,0,0,0.10),0 1px 4px rgba(0,0,0,0.06); }
  .tbl-label { font-size:11px; font-weight:bold; letter-spacing:.12em; text-transform:uppercase;
               color:#888; margin-bottom:7px; }
  .tbl-title { font-size:15.5px; font-weight:bold; line-height:1.45; color:#111;
               margin-bottom:24px; max-width:820px; }
  .panel-head { font-size:11px; font-weight:bold; letter-spacing:.08em; text-transform:uppercase;
                color:#444; margin:28px 0 8px; }
  table { width:100%; border-collapse:collapse; font-size:12px;
          font-variant-numeric:tabular-nums lining-nums; margin-bottom:8px; }
  th { font-size:11px; font-weight:bold; text-align:center; padding:6px 5px 7px;
       border-top:2px solid #111; border-bottom:1px solid #111; color:#111; }
  th:first-child { text-align:left; }
  td { text-align:center; padding:4px 5px; font-size:11.5px; }
  td:first-child { text-align:left; font-style:italic; color:#333; }
  .botrule td { border-bottom:2px solid #111; }
  .gofsep td { border-top:1px solid #ccc; }
  .bold { font-weight:bold; }
  .note { font-size:11px; color:#555; line-height:1.7; margin-top:10px; padding-top:6px; }
  .note strong { color:#222; }
  .sp th { border-top:none; border-bottom:1px solid #bbb; font-style:italic;
           font-weight:normal; font-size:11px; color:#555; }
  @media print { body{background:#fff;padding:0;} .page{box-shadow:none;padding:24px;} }
'

build_html <- function() {
  h <- c('<!DOCTYPE html><html lang="pt-BR"><head><meta charset="UTF-8">',
         '<title>Estat\u00edsticas Descritivas</title>',
         '<style>', css, '</style></head><body><div class="page">',
         '<p class="tbl-label">Tabela 2</p>',
         '<p class="tbl-title">Estat\u00edsticas Descritivas &mdash; Painel de Estados Brasileiros (2002&ndash;2024)</p>',

         # Panel A
         '<p class="panel-head">Painel A &mdash; Amostra completa (25 estados, 2002&ndash;2024)</p>',
         '<table><thead><tr>',
         '<th>Vari\u00e1vel</th><th>N</th><th>M\u00e9dia</th><th>D.P.</th>',
         '<th>M\u00ednimo</th><th>Mediana</th><th>M\u00e1ximo</th></tr></thead><tbody>')

  for (i in seq_len(nrow(panA))) {
    is_last <- (i == nrow(panA))
    cls <- if(is_last) ' class="botrule"' else ''
    h <- c(h, sprintf('<tr%s><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
      cls, rownames(panA)[i],
      as.integer(panA[i,"N"]),
      fmt(panA[i,"Mean"]), fmt(panA[i,"SD"]),
      fmt(panA[i,"Min"]),  fmt(panA[i,"Median"]), fmt(panA[i,"Max"])))
  }
  h <- c(h, '</tbody></table>')

  # Panel B
  h <- c(h, '<p class="panel-head">Painel B &mdash; M\u00e9dias por grupo de teto</p>',
         '<table><thead>',
         '<tr class="sp"><th></th>',
         '<th colspan="3">Teto 12%</th><th colspan="3">Teto 13%</th><th colspan="3">Teto 15%</th></tr>',
         '<tr><th>Vari\u00e1vel</th>',
         '<th>N</th><th>M\u00e9dia</th><th>D.P.</th>',
         '<th>N</th><th>M\u00e9dia</th><th>D.P.</th>',
         '<th>N</th><th>M\u00e9dia</th><th>D.P.</th></tr></thead><tbody>')

  for (i in seq_along(panB_vars)) {
    m <- panB[[i]]
    is_last <- (i == length(panB_vars))
    cls <- if(is_last) ' class="botrule"' else ''
    h <- c(h, sprintf(
      '<tr%s><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%d</td><td>%s</td><td>%s</td></tr>',
      cls, panB_labs[i],
      m["N",1], fmt(m["Mean",1]), fmt(m["SD",1]),
      m["N",2], fmt(m["Mean",2]), fmt(m["SD",2]),
      m["N",3], fmt(m["Mean",3]), fmt(m["SD",3])))
  }
  # t-test row
  h <- c(h, sprintf(
    '<tr class="gofsep botrule"><td colspan="7"><em>Teste-t DCL/RCL: 12%% vs 13%% &mdash; p=%s &nbsp;|&nbsp; 12%% vs 15%% &mdash; p=%s</em></td><td colspan="3"></td></tr>',
    fmt(t12v13$p.value,4), fmt(t12v15$p.value,4)))
  h <- c(h, '</tbody></table>')

  # Panel C
  h <- c(h, '<p class="panel-head">Painel C &mdash; Matriz de Correla\u00e7\u00f5es</p>',
         '<table><thead><tr><th></th>')
  for (l in labs) h <- c(h, sprintf('<th>%s</th>', l))
  h <- c(h, '</tr></thead><tbody>')

  for (i in seq_len(nrow(cormat))) {
    is_last <- (i == nrow(cormat))
    cls <- if(is_last) ' class="botrule"' else ''
    h <- c(h, sprintf('<tr%s><td>%s</td>', cls, rownames(cormat)[i]))
    for (j in seq_len(ncol(cormat))) {
      val <- cormat[i,j]
      if (i == j) {
        h <- c(h, '<td>1.000</td>')
      } else if (j < i) {
        bold <- abs(val) > 0.5
        tag  <- if(bold) '<td class="bold">' else '<td>'
        h <- c(h, sprintf('%s%s</td>', tag, fmt(val)))
      } else {
        h <- c(h, '<td></td>')
      }
    }
    h <- c(h, '</tr>')
  }
  h <- c(h, '</tbody></table>',
         '<p class="note"><strong>Notas:</strong> D.P. = Desvio padr\u00e3o. ',
         'Painel B: teste-t de Welch de diferen\u00e7a de m\u00e9dias entre grupos de teto. ',
         'Painel C: correla\u00e7\u00f5es de Pearson; valores em <strong>negrito</strong> indicam |r| > 0,50. ',
         'Amostra: 25 estados brasileiros (excl. AP e TO), 2002&ndash;2024. ',
         'Somente observa\u00e7\u00f5es com dados completos para todas as vari\u00e1veis.</p>',
         '</div></body></html>')
  paste(h, collapse="\n")
}

writeLines(build_html(), "output/tables/estatisticas_descritivas.html", useBytes=FALSE)
cat("  output/tables/estatisticas_descritivas.html\n")

# ── LATEX OUTPUT ──────────────────────────────────────────────────────────────
tex <- c(
  "% Tabela 2 — Estatísticas Descritivas",
  "\\begin{table}[htbp]",
  "\\caption{Estatísticas Descritivas — Painel de Estados Brasileiros (2002--2024)}",
  "\\label{tab:descritivas}",
  "\\small",
  "",
  "\\noindent\\textbf{Painel A — Amostra completa}\\\\[4pt]",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Variável & N & Média & D.P. & Mín. & Mediana & Máx. \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(panA))) {
  tex <- c(tex, sprintf("%s & %d & %s & %s & %s & %s & %s \\\\",
    rownames(panA)[i],
    as.integer(panA[i,"N"]),
    fmt(panA[i,"Mean"]), fmt(panA[i,"SD"]),
    fmt(panA[i,"Min"]),  fmt(panA[i,"Median"]), fmt(panA[i,"Max"])))
}
tex <- c(tex, "\\bottomrule", "\\end{tabular}\\\\[12pt]",
  "\\noindent\\textbf{Painel B — Médias por grupo de teto}\\\\[4pt]",
  "\\begin{tabular}{lrrr@{\\hspace{8pt}}rrr@{\\hspace{8pt}}rrr}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Teto 12\\%} & \\multicolumn{3}{c}{Teto 13\\%} & \\multicolumn{3}{c}{Teto 15\\%} \\\\",
  "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}",
  "Variável & N & Média & D.P. & N & Média & D.P. & N & Média & D.P. \\\\",
  "\\midrule"
)
for (i in seq_along(panB_vars)) {
  m <- panB[[i]]
  tex <- c(tex, sprintf("%s & %d & %s & %s & %d & %s & %s & %d & %s & %s \\\\",
    panB_labs[i],
    m["N",1], fmt(m["Mean",1]), fmt(m["SD",1]),
    m["N",2], fmt(m["Mean",2]), fmt(m["SD",2]),
    m["N",3], fmt(m["Mean",3]), fmt(m["SD",3])))
}
tex <- c(tex, "\\midrule",
  sprintf("\\multicolumn{10}{l}{\\textit{Teste-t DCL/RCL: 12\\%% vs 13\\%% — $p$=%s; 12\\%% vs 15\\%% — $p$=%s}} \\\\",
    fmt(t12v13$p.value,4), fmt(t12v15$p.value,4)),
  "\\bottomrule", "\\end{tabular}\\\\[12pt]",
  "\\noindent\\textbf{Painel C — Matriz de Correlações}\\\\[4pt]",
  sprintf("\\begin{tabular}{l%s}", paste(rep("r",length(labs)),collapse="")),
  "\\toprule",
  paste(c("", labs), collapse=" & "), " \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(cormat))) {
  row_vals <- sapply(seq_len(ncol(cormat)), function(j) {
    if (i==j) "1.000"
    else if (j < i) { v <- cormat[i,j]; if(abs(v)>0.5) sprintf("\\textbf{%s}",fmt(v)) else fmt(v) }
    else ""
  })
  tex <- c(tex, paste(c(rownames(cormat)[i], row_vals), collapse=" & "), " \\\\")
}
tex <- c(tex, "\\bottomrule", "\\end{tabular}",
  "\\begin{tablenotes}\\small",
  "\\item Notas: D.P.~=~Desvio padrão. Painel~B: teste-t de Welch. Painel~C: Pearson; \\textbf{negrito} indica $|r|>0{,}50$. Amostra: 25 estados brasileiros (excl.~AP e TO), 2002--2024.",
  "\\end{tablenotes}",
  "\\end{table}")

writeLines(tex, "output/tables/estatisticas_descritivas.tex")
cat("  output/tables/estatisticas_descritivas.tex\n")
cat("Done.\n")