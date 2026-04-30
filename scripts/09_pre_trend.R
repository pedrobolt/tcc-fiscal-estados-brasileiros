# =============================================================================
# scripts/09_pre_trend.R
# Análise de pré-tendência: trajetória DCL/RCL por grupo de teto
# Saída: output/figures/fig5_pre_trend.{pdf,png}
#         output/figures/fig6_event_study.{pdf,png}
# =============================================================================
pkgs <- c("dplyr","readr","ggplot2","fixest","tibble")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos="https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only=TRUE))
setwd("C:/Users/pltun/tcc")
dir.create("output/figures", recursive=TRUE, showWarnings=FALSE)

panel <- read_csv("data/processed/panel_slim.csv", show_col_types=FALSE) %>%
  mutate(teto_f = factor(teto))

accent <- "#1F4E79"

theme_aer <- theme_minimal(base_family="serif", base_size=11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color="gray90", linewidth=0.3),
    axis.line  = element_line(color="gray30", linewidth=0.3),
    axis.ticks = element_line(color="gray30", linewidth=0.3),
    plot.title    = element_text(size=12, face="bold", hjust=0),
    plot.subtitle = element_text(size=9, color="gray40", hjust=0),
    legend.position = "bottom",
    legend.text  = element_text(size=9),
    plot.margin  = margin(10,15,10,10)
  )
theme_set(theme_aer)

save_fig <- function(p, name, w=7, h=4.5) {
  ggsave(file.path("output/figures", paste0(name,".pdf")), p, width=w, height=h, device=cairo_pdf)
  ggsave(file.path("output/figures", paste0(name,".png")), p, width=w, height=h, dpi=300)
  cat(sprintf("  Saved: output/figures/%s.pdf + .png\n", name))
}

# ─── STEP 1: Visual pre-trend plot ───────────────────────────────────────────
fig5_data <- panel %>%
  filter(!is.na(dcl_sobre_rcl_ext), !is.na(teto)) %>%
  mutate(teto_label = factor(paste0("Teto ", teto, "%"),
                             levels=c("Teto 12%","Teto 13%","Teto 15%"))) %>%
  group_by(year, teto_label) %>%
  summarise(
    mean_dcl = mean(dcl_sobre_rcl_ext, na.rm=TRUE),
    se_dcl   = sd(dcl_sobre_rcl_ext, na.rm=TRUE) / sqrt(n()),
    n        = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    lo95 = mean_dcl - 1.96 * se_dcl,
    hi95 = mean_dcl + 1.96 * se_dcl
  )

fig5 <- ggplot(fig5_data, aes(x=year, y=mean_dcl,
                               color=teto_label, linetype=teto_label,
                               fill=teto_label)) +
  geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha=0.15, color=NA) +
  geom_line(linewidth=0.7) +
  geom_vline(xintercept=2008, linetype="dashed", color="gray50", linewidth=0.4) +
  annotate("text", x=2008.2,
           y=max(fig5_data$mean_dcl, na.rm=TRUE)*0.97,
           label="Per\u00edodo de refer\u00eancia",
           hjust=0, size=2.5, color="gray45", family="serif") +
  scale_color_manual(values=c("Teto 12%"="gray20","Teto 13%"=accent,"Teto 15%"="gray55"),
                     name=NULL) +
  scale_fill_manual(values=c("Teto 12%"="gray20","Teto 13%"=accent,"Teto 15%"="gray55"),
                    name=NULL) +
  scale_linetype_manual(values=c("Teto 12%"="dashed","Teto 13%"="solid","Teto 15%"="dotted"),
                        name=NULL) +
  scale_x_continuous(breaks=seq(2002,2024,4)) +
  labs(
    title    = "Figura 5 \u2014 Trajet\u00f3ria da DCL/RCL por grupo de teto (2002\u20132024)",
    subtitle = "M\u00e9dia por estado-ano; bandas = IC 95%. Linha tracejada: fim do per\u00edodo de refer\u00eancia.",
    x=NULL, y="DCL / RCL (m\u00e9dia)"
  ) +
  guides(color=guide_legend(nrow=1), linetype=guide_legend(nrow=1), fill=guide_legend(nrow=1))

save_fig(fig5, "fig5_pre_trend")

# ─── STEP 2: Formal pre-trend test ───────────────────────────────────────────
cat("\n================================================================\n")
cat(" STEP 2 — Teste formal de pré-tendência (2002-2008)\n")
cat("================================================================\n\n")

pre <- panel %>% filter(year <= 2008, !is.na(dcl_sobre_rcl_ext))
cat("Obs no pré-período:", nrow(pre), "\n\n")

m_pre <- feols(
  dcl_sobre_rcl_ext ~ i(year, teto, ref=2002) | uf,
  data    = pre,
  cluster = ~uf
)
cat("--- Regressão pré-período (dep: DCL/RCL, FE estado) ---\n")
print(summary(m_pre))

# Joint F-test of all interactions = 0
ftest <- wald(m_pre, keep="year")
cat("\n--- Teste F conjunto (todas as interações year×teto = 0) ---\n")
print(ftest)

co_pre  <- coef(m_pre)
se_pre  <- se(m_pre)
pv_pre  <- pvalue(m_pre)

cat("\n--- Tabela de coeficientes pré-tendência ---\n")
cat(sprintf("  %-35s %8s %8s %8s %s\n", "Termo","Coef","SE","p-valor",""))
mk <- function(p) ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.10,"*","")))
for (nm in names(co_pre)) {
  cat(sprintf("  %-35s %+8.4f %8.4f %8.4f %s\n",
      nm, co_pre[nm], se_pre[nm], pv_pre[nm], mk(pv_pre[nm])))
}

sig_any <- any(pv_pre < 0.10, na.rm=TRUE)
cat("\n--- Conclusão ---\n")
if (!sig_any) {
  cat("  Nenhum coeficiente year×teto significativo no pré-período.\n")
  cat("  >>> PARALELO DE TENDÊNCIAS NÃO REJEITADO (p > 0.10 em todos os termos)\n")
} else {
  n_sig <- sum(pv_pre < 0.10, na.rm=TRUE)
  cat(sprintf("  %d termo(s) significativo(s) a 10%% no pré-período.\n", n_sig))
  cat("  >>> VERIFICAR: possível violação de tendências paralelas\n")
}

# ─── STEP 3: Event study — full period ───────────────────────────────────────
cat("\n================================================================\n")
cat(" STEP 3 — Event study — período completo (2002-2024)\n")
cat("================================================================\n\n")

m_full <- feols(
  dcl_sobre_rcl_ext ~ i(year, teto, ref=2002) | uf,
  data    = panel %>% filter(!is.na(dcl_sobre_rcl_ext)),
  cluster = ~uf
)

es_data <- tibble(
  term  = names(coef(m_full)),
  est   = coef(m_full),
  se    = se(m_full),
  pval  = pvalue(m_full)
) %>%
  mutate(
    year = as.integer(sub(".*::(\\d{4}).*","\\1", term)),
    lo95 = est - 1.96*se,
    hi95 = est + 1.96*se,
    period = ifelse(year <= 2008, "Pré (2002-2008)", "Pós (2009-2024)")
  ) %>%
  filter(!is.na(year))

# Add 2002 reference point (coef=0)
ref_row <- tibble(term="ref", est=0, se=0, pval=1, year=2002L,
                  lo95=0, hi95=0, period="Pré (2002-2008)")
es_data <- bind_rows(ref_row, es_data) %>% arrange(year)

cat("Event study coefficients (year x teto):\n")
print(es_data %>% select(year, est, se, pval, period), n=30)

fig6 <- ggplot(es_data, aes(x=year, y=est, color=period, shape=period)) +
  geom_hline(yintercept=0, color="gray60", linewidth=0.3) +
  geom_vline(xintercept=2002, linetype="dotted", color="gray70", linewidth=0.3) +
  geom_vline(xintercept=2008.5, linetype="dashed", color="gray50", linewidth=0.4) +
  annotate("text", x=2009, y=max(es_data$hi95, na.rm=TRUE)*0.95,
           label="Pós-2008", hjust=0, size=2.5, color="gray45", family="serif") +
  geom_ribbon(aes(ymin=lo95, ymax=hi95, fill=period), alpha=0.12, color=NA) +
  geom_line(linewidth=0.55) +
  geom_point(size=2.5) +
  scale_color_manual(values=c("Pré (2002-2008)"="gray40","Pós (2009-2024)"=accent),
                     name=NULL) +
  scale_fill_manual(values=c("Pré (2002-2008)"="gray40","Pós (2009-2024)"=accent),
                    name=NULL) +
  scale_shape_manual(values=c("Pré (2002-2008)"=16,"Pós (2009-2024)"=17),
                     name=NULL) +
  scale_x_continuous(breaks=seq(2002,2024,4)) +
  labs(
    title    = "Figura 6 \u2014 Event study: efeito anual do teto sobre DCL/RCL",
    subtitle = "Coeficientes de year\u00d7teto com IC 95%; ano de refer\u00eancia: 2002. FE estado, erro clusterizado por estado.",
    x=NULL, y="Coeficiente (year \u00d7 teto)"
  ) +
  guides(color=guide_legend(nrow=1), fill=guide_legend(nrow=1), shape=guide_legend(nrow=1))

save_fig(fig6, "fig6_event_study")
cat("\n=== scripts/09_pre_trend.R concluído ===\n")