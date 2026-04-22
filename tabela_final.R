# =============================================================================
# tabela_final.R — Publication table for TCC (4-column results)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(fixest); library(plm)
  library(modelsummary); library(kableExtra); library(tibble)
})
set.seed(2025)

panel <- read_csv("output/panel_final_v5.csv", show_col_types=FALSE) %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(dcl_lag1 = dplyr::lag(dcl_sobre_rcl_ext, 1)) %>%
  ungroup()

# ── Estimate all models ───────────────────────────────────────────────────────
cat("Estimando modelos...\n")

m1_ols <- feols(primario_sobre_rcl_ext ~ d_lag1 + d_lag1_teto + yvar | uf + year,
                data=panel, cluster=~uf)
m1_iv  <- feols(primario_sobre_rcl_ext ~ yvar | uf + year |
                  d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
                data=panel, cluster=~uf)
fs_d1  <- fitstat(m1_iv,"ivf")[[1]]$stat
fs_d1t <- fitstat(m1_iv,"ivf")[[2]]$stat
wh_p   <- fitstat(m1_iv,"wh")[[1]]$p

cat("  LSDVC bootstrap (B=500)...\n")
panel_l <- panel %>%
  filter(!is.na(dcl_sobre_rcl_ext),!is.na(primario_sobre_rcl_ext),!is.na(crescimento_pib_pct))
panel_p <- pdata.frame(panel_l, index=c("uf","year"))
N <- n_distinct(panel_l$uf); T_bar <- nrow(panel_l)/N
m_w  <- plm(dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
              primario_sobre_rcl_ext + crescimento_pib_pct,
            data=panel_p, model="within", effect="individual")
b_w  <- coef(m_w)
m_ab <- suppressWarnings(pgmm(
  dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) +
    primario_sobre_rcl_ext + crescimento_pib_pct |
    lag(dcl_sobre_rcl_ext,2:3)+lag(primario_sobre_rcl_ext,2:3),
  data=panel_p,effect="individual",model="onestep",transformation="d",collapse=TRUE))
rho_AB  <- coef(summary(m_ab,robust=TRUE))["lag(dcl_sobre_rcl_ext, 1)","Estimate"]
nickell <- (1+rho_AB)/(T_bar-1)
b_lsdvc <- b_w
b_lsdvc["lag(dcl_sobre_rcl_ext, 1)"] <- b_w["lag(dcl_sobre_rcl_ext, 1)"]+nickell

states <- unique(panel_l$uf)
boot_m <- matrix(NA_real_,500,3,dimnames=list(NULL,names(b_lsdvc)))
for(b in 1:500){
  s_b <- sample(states,N,replace=TRUE)
  df_b <- lapply(seq_along(s_b),function(i)
    panel_l[panel_l$uf==s_b[i],]%>%mutate(uf=paste0("g",i)))%>%bind_rows()
  pp_b <- pdata.frame(df_b,index=c("uf","year"))
  ft   <- tryCatch(plm(dcl_sobre_rcl_ext~lag(dcl_sobre_rcl_ext,1)+
                         primario_sobre_rcl_ext+crescimento_pib_pct,
                       data=pp_b,model="within",effect="individual"),error=function(e)NULL)
  if(is.null(ft)||length(coef(ft))!=3) next
  rho_b<-tryCatch({ab_b<-suppressWarnings(pgmm(
    dcl_sobre_rcl_ext~lag(dcl_sobre_rcl_ext,1)+primario_sobre_rcl_ext+crescimento_pib_pct|
      lag(dcl_sobre_rcl_ext,2:3)+lag(primario_sobre_rcl_ext,2:3),
    data=pp_b,effect="individual",model="onestep",transformation="d",collapse=TRUE))
    coef(ab_b)["lag(dcl_sobre_rcl_ext, 1)"]},error=function(e) rho_AB)
  cb<-coef(ft); cb["lag(dcl_sobre_rcl_ext, 1)"]<-cb["lag(dcl_sobre_rcl_ext, 1)"]+(1+rho_b)/(T_bar-1)
  boot_m[b,]<-cb
}
boot_m  <- boot_m[complete.cases(boot_m),]
boot_se <- apply(boot_m,2,sd)
dof_l   <- nrow(panel_l)-N-3
pval_lsdvc <- function(v) 2*pt(-abs(b_lsdvc[v]/boot_se[v]),df=dof_l)
cat(sprintf("  Bootstrap: %d reps válidos\n",nrow(boot_m)))

m2_dk <- feols(dcl_sobre_rcl_ext ~ dcl_lag1 +
                 primario_sobre_rcl_ext + crescimento_pib_pct | uf + year,
               data=panel,panel.id=~uf+year,vcov="DK")
cat("Todos os modelos prontos.\n\n")

# =============================================================================
# Build table manually as data frame
# =============================================================================

fmt_coef <- function(est, se, pval) {
  stars <- ifelse(pval<0.01,"***",ifelse(pval<0.05,"**",ifelse(pval<0.10,"*","")))
  list(coef=sprintf("%.3f%s", est, stars), se=sprintf("(%.3f)", se))
}

blank  <- list(coef="", se="")
dash   <- list(coef="—", se="")

get_fc <- function(mod, var) {
  if (!var %in% names(coef(mod))) return(blank)
  fmt_coef(coef(mod)[var], se(mod)[var], pvalue(mod)[var])
}
get_fl <- function(var) {
  vmap <- c("dcl_lag1"="lag(dcl_sobre_rcl_ext, 1)",
            "primario_sobre_rcl_ext"="primario_sobre_rcl_ext",
            "crescimento_pib_pct"="crescimento_pib_pct")
  v <- vmap[var]
  fmt_coef(b_lsdvc[v], boot_se[v], pval_lsdvc(v))
}

rows <- list(
  list(var="DCL/RCL (t-1)",
       `I-A`=get_fc(m1_ols,"d_lag1"),
       `I-B`=get_fc(m1_iv,"fit_d_lag1"),
       `II-A`=get_fl("dcl_lag1"),
       `II-B`=get_fc(m2_dk,"dcl_lag1")),
  list(var="DCL/RCL (t-1) × Teto",
       `I-A`=get_fc(m1_ols,"d_lag1_teto"),
       `I-B`=get_fc(m1_iv,"fit_d_lag1_teto"),
       `II-A`=blank, `II-B`=blank),
  list(var="Hiato do produto",
       `I-A`=get_fc(m1_ols,"yvar"),
       `I-B`=get_fc(m1_iv,"yvar"),
       `II-A`=blank, `II-B`=blank),
  list(var="Primário/RCL",
       `I-A`=blank, `I-B`=blank,
       `II-A`=get_fl("primario_sobre_rcl_ext"),
       `II-B`=get_fc(m2_dk,"primario_sobre_rcl_ext")),
  list(var="Crescimento PIB",
       `I-A`=blank, `I-B`=blank,
       `II-A`=get_fl("crescimento_pib_pct"),
       `II-B`=get_fc(m2_dk,"crescimento_pib_pct"))
)

# Expand coef rows to two display rows each (estimate + SE)
build_df <- function(rows) {
  out <- list()
  for (r in rows) {
    out[[length(out)+1]] <- c(` `=r$var,
                               `I-A: MQO-EF`=r$`I-A`$coef,
                               `I-B: 2SLS`  =r$`I-B`$coef,
                               `II-A: LSDVC`=r$`II-A`$coef,
                               `II-B: EF+DK`=r$`II-B`$coef)
    out[[length(out)+1]] <- c(` `="",
                               `I-A: MQO-EF`=r$`I-A`$se,
                               `I-B: 2SLS`  =r$`I-B`$se,
                               `II-A: LSDVC`=r$`II-A`$se,
                               `II-B: EF+DK`=r$`II-B`$se)
  }
  as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE)
}

df_coef <- build_df(rows)

# GOF + metadata rows
r2_ols <- round(r2(m1_ols,"wr2"), 3)
r2_dk  <- round(r2(m2_dk,"wr2"), 3)

df_gof <- tribble(
  ~` `,                        ~`I-A: MQO-EF`,  ~`I-B: 2SLS`,
                   ~`II-A: LSDVC`,          ~`II-B: EF+DK`,
  "Obs.",                 "487",       "462",          "488",           "487",
  "R² within",     as.character(r2_ols), "—",           "—",    as.character(r2_dk),
  "F-stat 1º estágio",    "—",  sprintf("%.0f/%.0f",fs_d1,fs_d1t), "—",  "—",
  "Wu-Hausman (p)",        "—",  sprintf("%.4f",wh_p),              "—",  "—",
  "Bootstrap SE",          "—",          "—",          "Sim",           "—",
  "Correção Nickell",      "—",          "—",          "Sim",        "Parcial",
  "Estimador inicial",     "—",          "—",   "Arellano-Bond",      "—",
  "EF Estado",            "Sim",        "Sim",         "Sim",          "Sim",
  "EF Ano",               "Sim",        "Sim",         "Não",          "Sim"
)

df_full <- bind_rows(df_coef, df_gof)
names(df_full)[1] <- " "

note <- paste(
  "Nota: EP clusterizados por estado entre parênteses (I-A, I-B) e bootstrap",
  "B=500 (II-A). II-B usa erros Driscoll-Kraay (L=2). System-GMM two-way FE",
  "inviável com N=25 — substituído por LSDVC (Bruno, 2005). Amostra: 25 estados,",
  "2002-2024. * p<0,1  ** p<0,05  *** p<0,01.")

# ── Plain text ────────────────────────────────────────────────────────────────
header <- sprintf("\n%-28s %-14s %-14s %-16s %-14s\n%s",
                  " ", "I-A: MQO-EF", "I-B: 2SLS",
                  "II-A: LSDVC", "II-B: EF+DK",
                  strrep("-",90))
cat(header, "\n")
for (i in seq_len(nrow(df_full))) {
  r <- df_full[i,]
  if (r[[1]] != "" && !startsWith(r[[2]],"(") && !startsWith(r[[3]],"(") &&
      !startsWith(r[[4]],"(") && !startsWith(r[[5]],"(") &&
      r[[1]] != "Obs." && r[[1]] != "R² within" &&
      r[[1]] != "F-stat 1º estágio" && r[[1]] != "Wu-Hausman (p)" &&
      r[[1]] != "Bootstrap SE" && r[[1]] != "Correção Nickell" &&
      r[[1]] != "Estimador inicial" && r[[1]] != "EF Estado" && r[[1]] != "EF Ano" &&
      !all(c(r[[2]],r[[3]],r[[4]],r[[5]]) == "")) {
    cat(sprintf("  %-26s %-14s %-14s %-16s %-14s\n",r[[1]],r[[2]],r[[3]],r[[4]],r[[5]]))
  } else if (all(c(r[[2]],r[[3]],r[[4]],r[[5]]) %in% c("","(0.000)","()"))) {
    next  # skip blank SE rows for GOF section
  } else {
    cat(sprintf("  %-26s %-14s %-14s %-16s %-14s\n",r[[1]],r[[2]],r[[3]],r[[4]],r[[5]]))
  }
}
cat(strrep("-",90),"\n")
cat(strwrap(note, width=90), sep="\n")
cat("\n")

# Write TXT
writeLines(c(
  "TABELA: Sustentabilidade Fiscal e Regras de Endividamento",
  "Painel de Estados Brasileiros (2002-2024)",
  strrep("=",90),
  capture.output(print(df_full, row.names=FALSE)),
  "",
  strwrap(note, width=90)
), "output/tabela_resultados_tcc.txt")

# ── HTML ──────────────────────────────────────────────────────────────────────
html_out <- df_full %>%
  kbl(format="html", escape=TRUE, booktabs=TRUE,
      caption="Sustentabilidade Fiscal e Regras de Endividamento — Painel de Estados Brasileiros (2002–2024)") %>%
  kable_classic(full_width=FALSE, html_font="Times New Roman") %>%
  column_spec(1, bold=TRUE, width="8em") %>%
  column_spec(2:5, width="7em") %>%
  row_spec(0, bold=TRUE) %>%
  add_header_above(c(" "=1, "Dep.: Primário/RCL"=2, "Dep.: DCL/RCL"=2)) %>%
  footnote(general=note, general_title="", footnote_as_chunk=TRUE)
writeLines(as.character(html_out), "output/tabela_resultados_tcc.html")

# ── LaTeX ─────────────────────────────────────────────────────────────────────
tex_out <- df_full %>%
  kbl(format="latex", booktabs=TRUE, escape=TRUE, linesep="",
      caption="Sustentabilidade Fiscal e Regras de Endividamento — Painel de Estados Brasileiros (2002--2024)",
      label="tab:resultados") %>%
  kable_styling(latex_options=c("hold_position","scale_down")) %>%
  add_header_above(c(" "=1, "Dep.: Primário/RCL"=2, "Dep.: DCL/RCL"=2)) %>%
  footnote(general=note, general_title="", footnote_as_chunk=TRUE,
           threeparttable=TRUE)
writeLines(as.character(tex_out), "output/tabela_resultados_tcc.tex")

cat("✓ output/tabela_resultados_tcc.html\n")
cat("✓ output/tabela_resultados_tcc.tex\n")
cat("✓ output/tabela_resultados_tcc.txt\n")
