library(dplyr)
library(ggplot2)

df <- read.csv("data/processed/panel_final_v5.csv", stringsAsFactors = FALSE)

# Usar dcl_sobre_rcl_ext (cobre 2002-2024 completo)
df_stats <- df %>%
  filter(!is.na(dcl_sobre_rcl_ext)) %>%
  group_by(year) %>%
  summarise(
    media   = mean(dcl_sobre_rcl_ext, na.rm = TRUE) * 100,
    mediana = median(dcl_sobre_rcl_ext, na.rm = TRUE) * 100,
    p25     = quantile(dcl_sobre_rcl_ext, 0.25, na.rm = TRUE) * 100,
    p75     = quantile(dcl_sobre_rcl_ext, 0.75, na.rm = TRUE) * 100,
    n       = n(),
    .groups = "drop"
  )

cat("Estatísticas por ano:\n")
print(as.data.frame(df_stats))

cat("\nMédia 2002:", round(df_stats$media[df_stats$year == 2002], 1), "%\n")
cat("Média 2023:", round(df_stats$media[df_stats$year == 2023], 1), "%\n")

p <- ggplot(df_stats, aes(x = year)) +
  # IQR sombreado
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "grey80", alpha = 0.7) +
  # Linhas verticais — período amostral
  geom_vline(xintercept = 2002, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  # Mediana tracejada
  geom_line(aes(y = mediana, linetype = "Mediana"), color = "black", linewidth = 0.7) +
  # Média sólida grossa
  geom_line(aes(y = media, linetype = "Média"), color = "black", linewidth = 1.1) +
  scale_linetype_manual(
    name   = NULL,
    values = c("Média" = "solid", "Mediana" = "dashed"),
    breaks = c("Média", "Mediana")
  ) +
  scale_x_continuous(
    breaks = seq(2002, 2024, by = 2),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  labs(x = "Ano", y = "DCL / RCL (%)") +
  theme_classic(base_size = 11) +
  theme(
    axis.text        = element_text(size = 10, color = "black"),
    axis.title       = element_text(size = 11),
    axis.line        = element_line(color = "black", linewidth = 0.4),
    axis.ticks       = element_line(color = "black"),
    legend.position  = c(0.92, 0.88),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width  = unit(1.2, "cm"),
    legend.text       = element_text(size = 10),
    panel.grid        = element_blank()
  )

out_path <- "manuscript/pretextuais/fig1_dcl_rcl_limpo.png"
ggsave(out_path, plot = p, width = 8, height = 4.5, dpi = 300, units = "in")
cat("\nGráfico salvo em:", out_path, "\n")
