# =============================================================================
# scripts/06_tables.R
# Tabela de resultados publicável — 7 colunas (AER style)
# Cols: I-A OLS-FE | I-B 2SLS | II-A LSDVC | II-B EF+DK |
#       Rob1 LSDVC-yvar | Rob2 2SLS-noCOVID | Rob3 Binding
# Entrada: data/processed/panel_slim.csv
# Saída:   output/tables/tabela_final.{html,tex,txt}
# =============================================================================

pkgs <- c("dplyr", "readr", "fixest", "plm", "kableExtra", "tibble")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
set.seed(2025)

panel <- read_csv("data/processed/panel_slim.csv", show_col_types = FALSE) %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(dcl_lag1 = dplyr::lag(dcl_sobre_rcl_ext, 1)) %>%
  ungroup() %>%
  mutate(
    teto_frac           = teto / 100,
    binding             = as.integer(!is.na(encargos_sobre_rcl_ext) &
                                       !is.na(teto_frac) &
                                       (encargos_sobre_rcl_ext / teto_frac) > 0.70),
    d_lag1_teto_binding = d_lag1 * teto * binding,
    d_lag2_teto_binding = d_lag2 * teto * binding
  )

# =============================================================================
# 1. Estima todos os 7 modelos
# =============================================================================
cat("Estimando modelos...\n")

# Cols 1-2: Model I
m_ols <- feols(primario_sobre_rcl_ext ~ d_lag1 + d_lag1_teto + yvar | uf + year,
               data = panel, cluster = ~uf)
m_iv  <- feols(primario_sobre_rcl_ext ~ yvar | uf + year |
                 d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
               data = panel, cluster = ~uf)
fs_iv <- fitstat(m_iv, "ivf"); wh_iv <- fitstat(m_iv, "wh")[[1]]$p

# Col 4: Model II-B FE+DK
m_dk  <- feols(dcl_sobre_rcl_ext ~ dcl_lag1 + primario_sobre_rcl_ext +
                 crescimento_pib_pct | uf + year,
               data = panel, panel.id = ~uf + year, vcov = "DK")

# Col 6: Rob2 — 2SLS excluding COVID
panel_nc <- panel %>% filter(!year %in% c(2020, 2021))
m_nc  <- feols(primario_sobre_rcl_ext ~ yvar | uf + year |
                 d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
               data = panel_nc, cluster = ~uf)
fs_nc <- fitstat(m_nc, "ivf"); wh_nc <- fitstat(m_nc, "wh")[[1]]$p

# Col 7: Rob3 — 2SLS with binding triple interaction
panel_rob3 <- panel %>%
  filter(!is.na(primario_sobre_rcl_ext), !is.na(d_lag1), !is.na(d_lag2),
         !is.na(d_lag1_teto), !is.na(d_lag2_teto),
         !is.na(d_lag1_teto_binding), !is.na(d_lag2_teto_binding), !is.na(yvar))
m_rob3 <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_teto + d_lag1_teto_binding ~
    d_lag2 + d_lag2_teto + d_lag2_teto_binding,
  data = panel_rob3, cluster = ~uf)
fs_r3 <- fitstat(m_rob3, "ivf"); wh_r3 <- fitstat(m_rob3, "wh")[[1]]$p

# LSDVC helper
run_lsdvc <- function(panel_l, third_var, B = 500) {
  panel_p <- pdata.frame(panel_l, index = c("uf", "year"))
  N <- n_distinct(panel_l$uf); T_bar <- nrow(panel_l) / N
  fml    <- as.formula(paste0("dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) + primario_sobre_rcl_ext + ", third_var))
  fml_iv <- as.formula(paste0("dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) + primario_sobre_rcl_ext + ", third_var,
                               " | lag(dcl_sobre_rcl_ext,2:3) + lag(primario_sobre_rcl_ext,2:3)"))
  m_w  <- plm(fml, data = panel_p, model = "within", effect = "individual")
  b_w  <- coef(m_w)
  m_ab <- suppressWarnings(pgmm(fml_iv, data = panel_p, effect = "individual",
                                 model = "onestep", transformation = "d", collapse = TRUE))
  rho_AB <- coef(summary(m_ab, robust = TRUE))["lag(dcl_sobre_rcl_ext, 1)", "Estimate"]
  b_c  <- b_w
  b_c["lag(dcl_sobre_rcl_ext, 1)"] <- b_w["lag(dcl_sobre_rcl_ext, 1)"] + (1 + rho_AB) / (T_bar - 1)
  states <- unique(panel_l$uf); vn <- names(b_c)
  boot <- matrix(NA_real_, B, 3, dimnames = list(NULL, vn))
  for (b in seq_len(B)) {
    s_b  <- sample(states, N, replace = TRUE)
    df_b <- lapply(seq_along(s_b), function(i)
      panel_l[panel_l$uf == s_b[i], ] %>% mutate(uf = paste0("g", i))) %>% bind_rows()
    pp_b <- pdata.frame(df_b, index = c("uf", "year"))
    ft   <- tryCatch(plm(fml, data = pp_b, model = "within", effect = "individual"), error = function(e) NULL)
    if (is.null(ft) || length(coef(ft)) != 3) next
    rho_b <- tryCatch({
      ab_b <- suppressWarnings(pgmm(fml_iv, data = pp_b, effect = "individual",
                                     model = "onestep", transformation = "d", collapse = TRUE))
      coef(ab_b)["lag(dcl_sobre_rcl_ext, 1)"]
    }, error = function(e) rho_AB)
    cb <- coef(ft)
    cb["lag(dcl_sobre_rcl_ext, 1)"] <- cb["lag(dcl_sobre_rcl_ext, 1)"] + (1 + rho_b) / (T_bar - 1)
    boot[b, ] <- cb
  }
  boot <- boot[complete.cases(boot), ]; bse <- apply(boot, 2, sd)
  dof  <- nrow(panel_l) - N - 3
  pv   <- function(v) 2 * pt(-abs(b_c[v] / bse[v]), df = dof)
  list(coef = b_c, se = bse, pval = pv, nobs = nrow(panel_l), nboot = nrow(boot))
}

cat("  LSDVC baseline (B=500)...\n")
lsdvc3 <- run_lsdvc(panel %>% filter(!is.na(dcl_sobre_rcl_ext), !is.na(primario_sobre_rcl_ext), !is.na(crescimento_pib_pct)), "crescimento_pib_pct")
cat("  LSDVC Rob1/yvar (B=500)...\n")
lsdvc5 <- run_lsdvc(panel %>% filter(!is.na(dcl_sobre_rcl_ext), !is.na(primario_sobre_rcl_ext), !is.na(yvar)), "yvar")
cat("Todos os modelos prontos.\n\n")

# =============================================================================
# 2. Formatacao
# =============================================================================
mk_stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.10,"*","")))
fc    <- function(est,se,p) list(coef=sprintf("%.3f%s",est,mk_stars(p)), se=sprintf("(%.3f)",se))
fc4   <- function(est,se,p) list(coef=sprintf("%.4f%s",est,mk_stars(p)), se=sprintf("(%.4f)",se))
blank <- list(coef="", se="")
gf    <- function(mod,var) { if (!var %in% names(coef(mod))) return(blank); fc(coef(mod)[var],se(mod)[var],pvalue(mod)[var]) }
gf4   <- function(mod,var) { if (!var %in% names(coef(mod))) return(blank); fc4(coef(mod)[var],se(mod)[var],pvalue(mod)[var]) }
gl    <- function(ls,var)  { b<-ls$coef; s<-ls$se; pv<-ls$pval; if (!var %in% names(b)) return(blank); fc(b[var],s[var],pv(var)) }

cn <- c(" ","I-A: MQO-EF","I-B: 2SLS","II-A: LSDVC","II-B: EF+DK",
        "Rob1: LSDVC-yvar","Rob2: 2SLS-noCOV","Rob3: Binding")

rows <- list(
  list(label="DCL/RCL (t-1)",
       c1=gf(m_ols,"d_lag1"),       c2=gf(m_iv,"fit_d_lag1"),
       c3=gl(lsdvc3,"lag(dcl_sobre_rcl_ext, 1)"),
       c4=gf(m_dk,"dcl_lag1"),
       c5=gl(lsdvc5,"lag(dcl_sobre_rcl_ext, 1)"),
       c6=gf(m_nc,"fit_d_lag1"),    c7=gf(m_rob3,"fit_d_lag1")),

  list(label="DCL/RCL (t-1) \u00d7 Teto",
       c1=gf(m_ols,"d_lag1_teto"),  c2=gf(m_iv,"fit_d_lag1_teto"),
       c3=blank, c4=blank, c5=blank,
       c6=gf(m_nc,"fit_d_lag1_teto"), c7=gf(m_rob3,"fit_d_lag1_teto")),

  list(label="DCL/RCL (t-1) \u00d7 Teto \u00d7 Binding",
       c1=blank, c2=blank, c3=blank, c4=blank, c5=blank, c6=blank,
       c7=gf4(m_rob3,"fit_d_lag1_teto_binding")),

  list(label="Hiato do produto",
       c1=gf(m_ols,"yvar"),         c2=gf(m_iv,"yvar"),
       c3=blank, c4=blank,          c5=gl(lsdvc5,"yvar"),
       c6=gf(m_nc,"yvar"),          c7=gf(m_rob3,"yvar")),

  list(label="Prim\u00e1rio/RCL",
       c1=blank, c2=blank,
       c3=gl(lsdvc3,"primario_sobre_rcl_ext"),
       c4=gf(m_dk,"primario_sobre_rcl_ext"),
       c5=gl(lsdvc5,"primario_sobre_rcl_ext"),
       c6=blank, c7=blank),

  list(label="Crescimento PIB (%)",
       c1=blank, c2=blank,
       c3=gl(lsdvc3,"crescimento_pib_pct"),
       c4=gf(m_dk,"crescimento_pib_pct"),
       c5=blank, c6=blank, c7=blank)
)

build_df <- function(rows, cn) {
  out <- list()
  for (r in rows) {
    out[[length(out)+1]] <- setNames(
      c(r$label,r$c1$coef,r$c2$coef,r$c3$coef,r$c4$coef,r$c5$coef,r$c6$coef,r$c7$coef), cn)
    out[[length(out)+1]] <- setNames(
      c("",r$c1$se,r$c2$se,r$c3$se,r$c4$se,r$c5$se,r$c6$se,r$c7$se), cn)
  }
  as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE)
}
df_coef <- build_df(rows, cn)

# =============================================================================
# 3. GOF rows
# =============================================================================
df_gof <- tribble(
  ~` `,~`I-A: MQO-EF`,~`I-B: 2SLS`,~`II-A: LSDVC`,~`II-B: EF+DK`,
        ~`Rob1: LSDVC-yvar`,~`Rob2: 2SLS-noCOV`,~`Rob3: Binding`,
  "Obs.",
    as.character(m_ols$nobs), as.character(m_iv$nobs),
    as.character(lsdvc3$nobs), as.character(m_dk$nobs),
    as.character(lsdvc5$nobs), as.character(m_nc$nobs),
    as.character(m_rob3$nobs),
  "R\u00b2 within",
    as.character(round(r2(m_ols,"wr2"),3)), "\u2014", "\u2014",
    as.character(round(r2(m_dk,"wr2"),3)), "\u2014", "\u2014", "\u2014",
  "F-stat 1\u00ba est\u00e1gio",
    "\u2014",
    sprintf("%.0f/%.0f", fs_iv[[1]]$stat, fs_iv[[2]]$stat),
    "\u2014", "\u2014", "\u2014",
    sprintf("%.0f/%.0f", fs_nc[[1]]$stat, fs_nc[[2]]$stat),
    sprintf("%.0f/%.0f/%.0f", fs_r3[[1]]$stat, fs_r3[[2]]$stat, fs_r3[[3]]$stat),
  "Wu-Hausman (p)",
    "\u2014", sprintf("%.4f",wh_iv), "\u2014", "\u2014", "\u2014",
    sprintf("%.4f",wh_nc), sprintf("%.4f",wh_r3),
  "Bootstrap B=500",
    "\u2014","\u2014","Sim","\u2014","Sim","\u2014","\u2014",
  "Corre\u00e7\u00e3o Nickell",
    "\u2014","\u2014","Sim","\u2014","Sim","\u2014","\u2014",
  "EF Estado",  "Sim","Sim","Sim","Sim","Sim","Sim","Sim",
  "EF Ano",     "Sim","Sim","N\u00e3o","Sim","N\u00e3o","Sim","Sim"
)

df_full <- bind_rows(df_coef, df_gof); names(df_full)[1] <- " "

note <- paste(
  "Erros padr\u00e3o clusterizados por estado entre par\u00eanteses (Modelos I e Rob).",
  "Bootstrap com B=500 replica\u00e7\u00f5es (Modelos II LSDVC).",
  "System-GMM two-way FE invi\u00e1vel com N=25 \u2014 substitu\u00eddo por LSDVC (Bruno, 2005).",
  "Rob3: binding=1 se encargos/(teto/100)>0,70; inclui tripla interacao d×teto×binding.",
  "Amostra: 25 estados brasileiros (excl. AP e TO), 2002\u20132024.",
  "* p<0,1  ** p<0,05  *** p<0,01.")

# =============================================================================
# 4. Terminal
# =============================================================================
w <- c(28,10,9,11,10,13,13,12); sep <- strrep("-",sum(w)+length(w)-1)
fmt_row <- function(r) paste(sprintf(paste0("%-",w,"s"),unlist(r)),collapse=" ")
cat(fmt_row(as.list(names(df_full))),"\n"); cat(sep,"\n")
for (i in seq_len(nrow(df_full))) { r <- as.list(df_full[i,])
  if (all(unlist(r[-1])=="")) next; cat(fmt_row(r),"\n") }
cat(sep,"\n"); cat(strwrap(note,width=sum(w)+length(w)-1),sep="\n"); cat("\n")

# =============================================================================
# 5. TXT
# =============================================================================
writeLines(c("TABELA: Sustentabilidade Fiscal e Regras de Endividamento (7 colunas)",
             "Painel de Estados Brasileiros (2002-2024)",
             strrep("=",sum(w)+length(w)-1),
             capture.output(print(df_full,row.names=FALSE)),
             "", strwrap(note,width=sum(w)+length(w)-1)),
           "output/tables/tabela_final.txt")

# =============================================================================
# 6. HTML
# =============================================================================
html_out <- df_full %>%
  kbl(format="html",escape=TRUE,booktabs=TRUE,
      caption=paste("Sustentabilidade Fiscal e Regras de Endividamento",
                    "\u2014 Painel de Estados Brasileiros (2002\u20132024)")) %>%
  kable_classic(full_width=FALSE, html_font="Times New Roman") %>%
  column_spec(1,bold=TRUE,width="10em") %>% column_spec(2:8,width="4.8em") %>%
  row_spec(0,bold=TRUE) %>%
  add_header_above(c(" "=1,"Dep.: Prim\u00e1rio/RCL"=2,"Dep.: DCL/RCL"=3,
                     "Dep.: Prim\u00e1rio/RCL"=2)) %>%
  add_header_above(c(" "=1,"Modelos Principais"=4,"Verifica\u00e7\u00f5es de Robustez"=3)) %>%
  footnote(general=note,general_title="",footnote_as_chunk=TRUE)
writeLines(as.character(html_out),"output/tables/tabela_final.html")

# =============================================================================
# 7. LaTeX
# =============================================================================
tex_out <- df_full %>%
  kbl(format="latex",booktabs=TRUE,escape=TRUE,linesep="",
      caption=paste("Sustentabilidade Fiscal e Regras de Endividamento",
                    "--- Painel de Estados Brasileiros (2002--2024)"),
      label="tab:resultados7") %>%
  kable_styling(latex_options=c("hold_position","scale_down")) %>%
  add_header_above(c(" "=1,"Dep.: Primario/RCL"=2,"Dep.: DCL/RCL"=3,
                     "Dep.: Primario/RCL"=2)) %>%
  add_header_above(c(" "=1,"Modelos Principais"=4,"Verificacoes de Robustez"=3)) %>%
  footnote(general=note,general_title="",footnote_as_chunk=TRUE,threeparttable=TRUE)
writeLines(as.character(tex_out),"output/tables/tabela_final.tex")

cat("\u2713 output/tables/tabela_final.html\n")
cat("\u2713 output/tables/tabela_final.tex\n")
cat("\u2713 output/tables/tabela_final.txt\n")
