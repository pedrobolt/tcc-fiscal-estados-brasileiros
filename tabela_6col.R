# =============================================================================
# tabela_6col.R — 6-column publication table
# Cols: I-A OLS-FE | I-B 2SLS | II-A LSDVC | II-B FE+DK | Rob1 LSDVC-yvar | Rob2 2SLS-noCOVID
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(fixest); library(plm)
  library(kableExtra); library(tibble)
})
set.seed(2025)

panel <- read_csv("output/panel_final_v5.csv", show_col_types = FALSE) %>%
  arrange(uf, year) %>%
  group_by(uf) %>%
  mutate(dcl_lag1 = dplyr::lag(dcl_sobre_rcl_ext, 1)) %>%
  ungroup()

# =============================================================================
# 1. Estimate all 6 models
# =============================================================================
cat("Estimando modelos...\n")

## Cols 1-2: Model I (baseline)
m_ols <- feols(primario_sobre_rcl_ext ~ d_lag1 + d_lag1_teto + yvar | uf + year,
               data = panel, cluster = ~uf)

m_iv  <- feols(primario_sobre_rcl_ext ~ yvar | uf + year |
                 d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
               data = panel, cluster = ~uf)
fs_iv <- fitstat(m_iv, "ivf")
wh_iv <- fitstat(m_iv, "wh")[[1]]$p

## Col 4: Model II-B FE+DK
m_dk <- feols(dcl_sobre_rcl_ext ~ dcl_lag1 + primario_sobre_rcl_ext +
                crescimento_pib_pct | uf + year,
              data = panel, panel.id = ~uf + year, vcov = "DK")

## Col 6: Rob2 — 2SLS excluding COVID years
panel_nc <- panel %>% filter(!year %in% c(2020, 2021))
m_nc  <- feols(primario_sobre_rcl_ext ~ yvar | uf + year |
                 d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
               data = panel_nc, cluster = ~uf)
fs_nc <- fitstat(m_nc, "ivf")
wh_nc <- fitstat(m_nc, "wh")[[1]]$p

## LSDVC helper (plm Within + Nickell + block bootstrap) ----------------------
run_lsdvc <- function(panel_l, third_var, B = 500) {
  panel_p <- pdata.frame(panel_l, index = c("uf", "year"))
  N      <- n_distinct(panel_l$uf)
  T_bar  <- nrow(panel_l) / N
  fml    <- as.formula(paste0(
    "dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) + primario_sobre_rcl_ext + ",
    third_var))
  fml_iv <- as.formula(paste0(
    "dcl_sobre_rcl_ext ~ lag(dcl_sobre_rcl_ext,1) + primario_sobre_rcl_ext + ",
    third_var,
    " | lag(dcl_sobre_rcl_ext,2:3) + lag(primario_sobre_rcl_ext,2:3)"))
  m_w    <- plm(fml, data = panel_p, model = "within", effect = "individual")
  b_w    <- coef(m_w)
  m_ab   <- suppressWarnings(pgmm(fml_iv, data = panel_p, effect = "individual",
                                   model = "onestep", transformation = "d", collapse = TRUE))
  rho_AB <- coef(summary(m_ab, robust = TRUE))["lag(dcl_sobre_rcl_ext, 1)", "Estimate"]
  nick   <- (1 + rho_AB) / (T_bar - 1)
  b_c    <- b_w
  b_c["lag(dcl_sobre_rcl_ext, 1)"] <- b_w["lag(dcl_sobre_rcl_ext, 1)"] + nick

  states <- unique(panel_l$uf)
  vn     <- names(b_c)
  boot   <- matrix(NA_real_, B, 3, dimnames = list(NULL, vn))
  for (b in seq_len(B)) {
    s_b  <- sample(states, N, replace = TRUE)
    df_b <- lapply(seq_along(s_b), function(i)
      panel_l[panel_l$uf == s_b[i], ] %>% mutate(uf = paste0("g", i))) %>% bind_rows()
    pp_b <- pdata.frame(df_b, index = c("uf", "year"))
    ft   <- tryCatch(plm(fml, data = pp_b, model = "within", effect = "individual"),
                     error = function(e) NULL)
    if (is.null(ft) || length(coef(ft)) != 3) next
    rho_b <- tryCatch({
      ab_b <- suppressWarnings(pgmm(fml_iv, data = pp_b, effect = "individual",
                                     model = "onestep", transformation = "d", collapse = TRUE))
      coef(ab_b)["lag(dcl_sobre_rcl_ext, 1)"]
    }, error = function(e) rho_AB)
    cb <- coef(ft)
    cb["lag(dcl_sobre_rcl_ext, 1)"] <- cb["lag(dcl_sobre_rcl_ext, 1)"] +
      (1 + rho_b) / (T_bar - 1)
    boot[b, ] <- cb
  }
  boot <- boot[complete.cases(boot), ]
  bse  <- apply(boot, 2, sd)
  dof  <- nrow(panel_l) - N - 3
  pv   <- function(v) 2 * pt(-abs(b_c[v] / bse[v]), df = dof)
  list(coef = b_c, se = bse, pval = pv, nobs = nrow(panel_l), nboot = nrow(boot))
}

## Col 3: II-A LSDVC baseline (crescimento_pib_pct)
cat("  LSDVC baseline (B=500)...\n")
panel_l3 <- panel %>%
  filter(!is.na(dcl_sobre_rcl_ext), !is.na(primario_sobre_rcl_ext),
         !is.na(crescimento_pib_pct))
lsdvc3 <- run_lsdvc(panel_l3, "crescimento_pib_pct")
cat(sprintf("    %d valid reps\n", lsdvc3$nboot))

## Col 5: Rob1 LSDVC with yvar
cat("  LSDVC Rob1/yvar (B=500)...\n")
panel_l5 <- panel %>%
  filter(!is.na(dcl_sobre_rcl_ext), !is.na(primario_sobre_rcl_ext), !is.na(yvar))
lsdvc5 <- run_lsdvc(panel_l5, "yvar")
cat(sprintf("    %d valid reps\n", lsdvc5$nboot))

cat("Todos os modelos prontos.\n\n")

# =============================================================================
# 2. Format helpers
# =============================================================================
mk_stars <- function(p)
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

fc <- function(est, se, p)
  list(coef = sprintf("%.3f%s", est, mk_stars(p)), se = sprintf("(%.3f)", se))

blank <- list(coef = "", se = "")

gf <- function(mod, var) {          # from feols
  if (!var %in% names(coef(mod))) return(blank)
  fc(coef(mod)[var], se(mod)[var], pvalue(mod)[var])
}
gl <- function(lsdvc, var) {        # from LSDVC list
  b <- lsdvc$coef; s <- lsdvc$se; pv <- lsdvc$pval
  if (!var %in% names(b)) return(blank)
  fc(b[var], s[var], pv(var))
}

# =============================================================================
# 3. Coefficient rows
# =============================================================================
cn <- c(" ", "I-A: MQO-EF", "I-B: 2SLS", "II-A: LSDVC",
        "II-B: EF+DK", "Rob1: LSDVC-yvar", "Rob2: 2SLS-noCOV")

rows <- list(
  list(label = "DCL/RCL (t-1)",
       c1 = gf(m_ols, "d_lag1"),
       c2 = gf(m_iv,  "fit_d_lag1"),
       c3 = gl(lsdvc3, "lag(dcl_sobre_rcl_ext, 1)"),
       c4 = gf(m_dk,  "dcl_lag1"),
       c5 = gl(lsdvc5, "lag(dcl_sobre_rcl_ext, 1)"),
       c6 = gf(m_nc,  "fit_d_lag1")),

  list(label = "DCL/RCL (t-1) \u00d7 Teto",
       c1 = gf(m_ols, "d_lag1_teto"),
       c2 = gf(m_iv,  "fit_d_lag1_teto"),
       c3 = blank, c4 = blank, c5 = blank,
       c6 = gf(m_nc,  "fit_d_lag1_teto")),

  list(label = "Hiato do produto",
       c1 = gf(m_ols, "yvar"),
       c2 = gf(m_iv,  "yvar"),
       c3 = blank, c4 = blank,
       c5 = gl(lsdvc5, "yvar"),
       c6 = gf(m_nc,  "yvar")),

  list(label = "Prim\u00e1rio/RCL",
       c1 = blank, c2 = blank,
       c3 = gl(lsdvc3, "primario_sobre_rcl_ext"),
       c4 = gf(m_dk,  "primario_sobre_rcl_ext"),
       c5 = gl(lsdvc5, "primario_sobre_rcl_ext"),
       c6 = blank),

  list(label = "Crescimento PIB (%)",
       c1 = blank, c2 = blank,
       c3 = gl(lsdvc3, "crescimento_pib_pct"),
       c4 = gf(m_dk,  "crescimento_pib_pct"),
       c5 = blank, c6 = blank)
)

build_df <- function(rows, cn) {
  out <- list()
  for (r in rows) {
    out[[length(out)+1]] <- setNames(
      c(r$label, r$c1$coef, r$c2$coef, r$c3$coef,
        r$c4$coef, r$c5$coef, r$c6$coef), cn)
    out[[length(out)+1]] <- setNames(
      c("", r$c1$se, r$c2$se, r$c3$se,
        r$c4$se, r$c5$se, r$c6$se), cn)
  }
  as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
}

df_coef <- build_df(rows, cn)

# =============================================================================
# 4. GOF / metadata rows
# =============================================================================
r2_ols <- round(r2(m_ols, "wr2"), 3)
r2_dk  <- round(r2(m_dk,  "wr2"), 3)

df_gof <- tribble(
  ~` `,                    ~`I-A: MQO-EF`,  ~`I-B: 2SLS`,    ~`II-A: LSDVC`,
                            ~`II-B: EF+DK`,  ~`Rob1: LSDVC-yvar`, ~`Rob2: 2SLS-noCOV`,
  "Obs.",
    as.character(m_ols$nobs), as.character(m_iv$nobs),
    as.character(lsdvc3$nobs), as.character(m_dk$nobs),
    as.character(lsdvc5$nobs), as.character(m_nc$nobs),
  "R\u00b2 within",
    as.character(r2_ols), "\u2014", "\u2014",
    as.character(r2_dk),  "\u2014", "\u2014",
  "F-stat 1\u00ba est\u00e1gio",
    "\u2014",
    sprintf("%.0f / %.0f", fs_iv[[1]]$stat, fs_iv[[2]]$stat),
    "\u2014", "\u2014", "\u2014",
    sprintf("%.0f / %.0f", fs_nc[[1]]$stat, fs_nc[[2]]$stat),
  "Wu-Hausman (p)",
    "\u2014", sprintf("%.4f", wh_iv),
    "\u2014", "\u2014", "\u2014", sprintf("%.4f", wh_nc),
  "Bootstrap B=500",
    "\u2014", "\u2014", "Sim", "\u2014", "Sim", "\u2014",
  "Corre\u00e7\u00e3o Nickell",
    "\u2014", "\u2014", "Sim", "\u2014", "Sim", "\u2014",
  "EF Estado",
    "Sim", "Sim", "Sim", "Sim", "Sim", "Sim",
  "EF Ano",
    "Sim", "Sim", "N\u00e3o", "Sim", "N\u00e3o", "Sim"
)

df_full <- bind_rows(df_coef, df_gof)
names(df_full)[1] <- " "

# =============================================================================
# 5. Note
# =============================================================================
note <- paste(
  "Erros padr\u00e3o clusterizados por estado entre par\u00eanteses (Modelos I).",
  "Bootstrap com B=500 replica\u00e7\u00f5es (Modelos II LSDVC).",
  "System-GMM two-way FE invi\u00e1vel com N=25 \u2014 substitu\u00eddo por",
  "LSDVC (Bruno, 2005). Amostra: 25 estados brasileiros (excl. AP e TO),",
  "2002\u20132024. * p<0,1  ** p<0,05  *** p<0,01.")

# =============================================================================
# 6. Plain-text terminal print
# =============================================================================
w   <- c(22, 11, 11, 12, 11, 14, 14)
sep <- strrep("-", sum(w) + length(w) - 1)
fmt_row <- function(r) paste(sprintf(paste0("%-", w, "s"), unlist(r)), collapse = " ")

cat(fmt_row(as.list(names(df_full))), "\n")
cat(sep, "\n")
for (i in seq_len(nrow(df_full))) {
  r <- as.list(df_full[i, ])
  if (all(unlist(r[-1]) == "")) next
  cat(fmt_row(r), "\n")
}
cat(sep, "\n")
cat(strwrap(note, width = sum(w) + length(w) - 1), sep = "\n")
cat("\n")

# =============================================================================
# 7. TXT export
# =============================================================================
writeLines(c(
  "TABELA: Sustentabilidade Fiscal e Regras de Endividamento (6 colunas)",
  "Painel de Estados Brasileiros (2002-2024)",
  strrep("=", sum(w) + length(w) - 1),
  capture.output(print(df_full, row.names = FALSE)),
  "", strwrap(note, width = sum(w) + length(w) - 1)
), "output/tabela_final.txt")

# =============================================================================
# 8. HTML export
# =============================================================================
html_out <- df_full %>%
  kbl(format = "html", escape = TRUE, booktabs = TRUE,
      caption = paste("Sustentabilidade Fiscal e Regras de Endividamento",
                      "\u2014 Painel de Estados Brasileiros (2002\u20132024)")) %>%
  kable_classic(full_width = FALSE, html_font = "Times New Roman") %>%
  column_spec(1, bold = TRUE, width = "9em") %>%
  column_spec(2:7, width = "5.5em") %>%
  row_spec(0, bold = TRUE) %>%
  add_header_above(c(" " = 1,
                     "Dep.: Prim\u00e1rio/RCL" = 2,
                     "Dep.: DCL/RCL" = 3,
                     "Dep.: Prim\u00e1rio/RCL" = 1)) %>%
  add_header_above(c(" " = 1,
                     "Modelos Principais" = 4,
                     "Robustez" = 2)) %>%
  footnote(general = note, general_title = "", footnote_as_chunk = TRUE)
writeLines(as.character(html_out), "output/tabela_final.html")

# =============================================================================
# 9. LaTeX export
# =============================================================================
tex_out <- df_full %>%
  kbl(format = "latex", booktabs = TRUE, escape = TRUE, linesep = "",
      caption = paste("Sustentabilidade Fiscal e Regras de Endividamento",
                      "--- Painel de Estados Brasileiros (2002--2024)"),
      label = "tab:resultados6") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(c(" " = 1,
                     "Dep.: Primario/RCL" = 2,
                     "Dep.: DCL/RCL" = 3,
                     "Dep.: Primario/RCL" = 1)) %>%
  add_header_above(c(" " = 1,
                     "Modelos Principais" = 4,
                     "Robustez" = 2)) %>%
  footnote(general = note, general_title = "", footnote_as_chunk = TRUE,
           threeparttable = TRUE)
writeLines(as.character(tex_out), "output/tabela_final.tex")

cat("\u2713 output/tabela_final.html\n")
cat("\u2713 output/tabela_final.tex\n")
cat("\u2713 output/tabela_final.txt\n")
