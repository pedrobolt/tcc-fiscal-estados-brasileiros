# =============================================================================
# scripts/07_figures.R
# Figuras AER-style (4 paineis) para a tese
# Saida: output/figures/{fig1,fig2,fig3,fig4}.{pdf,png}
# =============================================================================

pkgs <- c("ggplot2", "dplyr", "readr", "tidyr", "patchwork", "scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

panel <- read_csv("data/processed/panel_slim.csv", show_col_types = FALSE)

# --- Tema base AER/QJE -------------------------------------------------------
theme_aer <- theme_minimal(base_family = "serif", base_size = 11) +
  theme(
    panel.grid.minor       = element_blank(),
    panel.grid.major.x     = element_blank(),
    panel.grid.major.y     = element_line(color = "gray90", linewidth = 0.3),
    axis.line              = element_line(color = "gray30", linewidth = 0.3),
    axis.ticks             = element_line(color = "gray30", linewidth = 0.3),
    plot.title             = element_text(size = 12, face = "bold", hjust = 0),
    plot.subtitle          = element_text(size = 9, color = "gray40", hjust = 0),
    legend.position        = "bottom",
    legend.text            = element_text(size = 9),
    plot.margin            = margin(10, 15, 10, 10)
  )
theme_set(theme_aer)

accent <- "#1F4E79"

save_fig <- function(p, name, w = 7, h = 4.5) {
  ggsave(file.path("output/figures", paste0(name, ".pdf")),
         p, width = w, height = h, device = cairo_pdf)
  ggsave(file.path("output/figures", paste0(name, ".png")),
         p, width = w, height = h, dpi = 300)
  cat(sprintf("  Salvo: output/figures/%s.pdf + .png\n", name))
}

# =============================================================================
# FIGURA 1 — DCL/RCL media por grupo de teto (2002-2024)
# =============================================================================
fig1_data <- panel %>%
  filter(!is.na(dcl_sobre_rcl_ext), !is.na(teto)) %>%
  mutate(teto_label = factor(paste0("Teto ", teto, "%"),
                             levels = c("Teto 12%", "Teto 13%", "Teto 15%"))) %>%
  group_by(year, teto_label) %>%
  summarise(mean_dcl = mean(dcl_sobre_rcl_ext, na.rm = TRUE), .groups = "drop")

fig1 <- ggplot(fig1_data, aes(x = year, y = mean_dcl,
                               linetype = teto_label, color = teto_label)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 2.0, linetype = "solid",
             color = "gray50", linewidth = 0.4) +
  annotate("text", x = 2003, y = 2.05, label = "Limite LRF",
           hjust = 0, size = 2.8, color = "gray40", family = "serif") +
  geom_vline(xintercept = 2015, linetype = "dashed",
             color = "gray60", linewidth = 0.35) +
  annotate("text", x = 2015.2, y = max(fig1_data$mean_dcl, na.rm = TRUE) * 0.97,
           label = "SICONFI", hjust = 0, size = 2.5, color = "gray50",
           family = "serif") +
  scale_color_manual(values = c("Teto 12%" = "gray20",
                                 "Teto 13%" = accent,
                                 "Teto 15%" = "gray55"),
                     name = NULL) +
  scale_linetype_manual(values = c("Teto 12%" = "dashed",
                                    "Teto 13%" = "solid",
                                    "Teto 15%" = "dotted"),
                        name = NULL) +
  scale_x_continuous(breaks = seq(2002, 2024, 4)) +
  labs(
    title    = "Figura 1 \u2014 DCL/RCL m\u00e9dia por grupo de teto (2002\u20132024)",
    subtitle = "Painel de 25 estados brasileiros (excl. AP e TO)",
    x        = NULL,
    y        = "DCL / RCL"
  ) +
  guides(color    = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1))

save_fig(fig1, "fig1_dcl_por_teto")

# =============================================================================
# FIGURA 2 — Coeficientes Modelo I: MQO-EF vs 2SLS
# =============================================================================
coef_data <- tibble(
  coef   = rep(c("beta1", "beta2"), 2),
  modelo = rep(c("MQO-EF", "2SLS"), each = 2),
  est    = c(0.324, -0.021, 0.653, -0.041),
  se     = c(0.111,  0.008, 0.170,  0.012)
) %>%
  mutate(
    lo95 = est - 1.96 * se,
    hi95 = est + 1.96 * se,
    coef_label = factor(coef,
                        levels = c("beta1", "beta2"),
                        labels = c("\u03b2\u2081  DCL/RCL (t-1)",
                                   "\u03b2\u2082  DCL/RCL \u00d7 Teto")),
    modelo = factor(modelo, levels = c("MQO-EF", "2SLS"))
  )

fig2 <- ggplot(coef_data, aes(x = modelo, y = est, color = modelo, shape = modelo)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo95, ymax = hi95),
                width = 0.12, linewidth = 0.5) +
  geom_point(size = 3) +
  facet_wrap(~coef_label, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("MQO-EF" = "gray40", "2SLS" = accent),
                     name = NULL) +
  scale_shape_manual(values = c("MQO-EF" = 16, "2SLS" = 17),
                     name = NULL) +
  labs(
    title    = "Figura 2 \u2014 Estimativas do Modelo I: MQO-EF vs 2SLS",
    subtitle = "Intervalos de confian\u00e7a de 95%, erros clusterizados por estado",
    x        = NULL,
    y        = "Estimativa pontual"
  ) +
  theme(
    strip.text      = element_text(size = 9, face = "italic"),
    legend.position = "none"
  )

save_fig(fig2, "fig2_coef_mqo_vs_2sls")

# =============================================================================
# FIGURA 3 — Resultado primario medio por grupo de teto (2002-2024)
# =============================================================================
fig3_data <- panel %>%
  filter(!is.na(primario_sobre_rcl_ext), !is.na(teto)) %>%
  mutate(teto_label = factor(paste0("Teto ", teto, "%"),
                             levels = c("Teto 12%", "Teto 13%", "Teto 15%"))) %>%
  group_by(year, teto_label) %>%
  summarise(mean_prim = mean(primario_sobre_rcl_ext, na.rm = TRUE), .groups = "drop")

fig3 <- ggplot(fig3_data, aes(x = year, y = mean_prim,
                               linetype = teto_label, color = teto_label)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid",
             color = "gray50", linewidth = 0.4) +
  annotate("text", x = 2003, y = 0.005,
           label = "Super\u00e1vit zero", hjust = 0, size = 2.8,
           color = "gray40", family = "serif") +
  geom_vline(xintercept = 2015, linetype = "dashed",
             color = "gray60", linewidth = 0.35) +
  annotate("text", x = 2015.2,
           y = max(fig3_data$mean_prim, na.rm = TRUE) * 0.92,
           label = "SICONFI", hjust = 0, size = 2.5,
           color = "gray50", family = "serif") +
  scale_color_manual(values = c("Teto 12%" = "gray20",
                                 "Teto 13%" = accent,
                                 "Teto 15%" = "gray55"),
                     name = NULL) +
  scale_linetype_manual(values = c("Teto 12%" = "dashed",
                                    "Teto 13%" = "solid",
                                    "Teto 15%" = "dotted"),
                        name = NULL) +
  scale_x_continuous(breaks = seq(2002, 2024, 4)) +
  labs(
    title    = "Figura 3 \u2014 Resultado prim\u00e1rio m\u00e9dio por grupo de teto (2002\u20132024)",
    subtitle = "Painel de 25 estados brasileiros (excl. AP e TO)",
    x        = NULL,
    y        = "Prim\u00e1rio / RCL"
  ) +
  guides(color    = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1))

save_fig(fig3, "fig3_primario_por_teto")

# =============================================================================
# FIGURA 4 — Forest plot: robustez dos coeficientes principais
# =============================================================================
forest_data <- tibble(
  spec = c(rep(c("2SLS\n(baseline)", "Rob2\n(sem COVID)", "Rob3\n(binding)"), 2),
           "Rob3\n(binding)"),
  coef = c(rep("beta1", 3), rep("beta2", 3), "beta3"),
  est  = c(0.653, 0.692, 0.670,  -0.041, -0.043, -0.042,  -0.0005),
  se   = c(0.170, 0.169, 0.170,   0.012,  0.012,  0.012,   0.0004)
) %>%
  mutate(
    lo95 = est - 1.96 * se,
    hi95 = est + 1.96 * se,
    coef_label = factor(coef,
                        levels = c("beta1", "beta2", "beta3"),
                        labels = c("\u03b2\u2081  DCL/RCL (t-1)",
                                   "\u03b2\u2082  DCL/RCL (t-1) \u00d7 Teto",
                                   "\u03b2\u2083  DCL/RCL (t-1) \u00d7 Teto \u00d7 Binding")),
    spec = factor(spec, levels = rev(c("2SLS\n(baseline)",
                                       "Rob2\n(sem COVID)",
                                       "Rob3\n(binding)")))
  )

fig4 <- ggplot(forest_data, aes(x = est, y = spec,
                                 color = coef_label, shape = coef_label)) +
  geom_vline(xintercept = 0, color = "gray60", linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                 height = 0.15, linewidth = 0.5) +
  geom_point(size = 3) +
  facet_wrap(~coef_label, scales = "free_x", nrow = 1) +
  scale_color_manual(
    values = c("\u03b2\u2081  DCL/RCL (t-1)"                      = accent,
               "\u03b2\u2082  DCL/RCL (t-1) \u00d7 Teto"              = "gray30",
               "\u03b2\u2083  DCL/RCL (t-1) \u00d7 Teto \u00d7 Binding"    = "gray55"),
    name = NULL) +
  scale_shape_manual(
    values = c("\u03b2\u2081  DCL/RCL (t-1)"                      = 16,
               "\u03b2\u2082  DCL/RCL (t-1) \u00d7 Teto"              = 17,
               "\u03b2\u2083  DCL/RCL (t-1) \u00d7 Teto \u00d7 Binding"    = 15),
    name = NULL) +
  labs(
    title    = "Figura 4 \u2014 Robustez dos coeficientes principais (Modelo I)",
    subtitle = "Intervalos de confian\u00e7a de 95%, erros clusterizados por estado",
    x        = "Estimativa pontual",
    y        = NULL
  ) +
  theme(
    strip.text      = element_text(size = 9, face = "italic"),
    legend.position = "none"
  )

save_fig(fig4, "fig4_forest_robustez")

cat("\n=== Figuras geradas com sucesso ===\n")
cat("  output/figures/fig1_dcl_por_teto.{pdf,png}\n")
cat("  output/figures/fig2_coef_mqo_vs_2sls.{pdf,png}\n")
cat("  output/figures/fig3_primario_por_teto.{pdf,png}\n")
cat("  output/figures/fig4_forest_robustez.{pdf,png}\n")
