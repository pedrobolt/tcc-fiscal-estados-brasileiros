library(tidyverse)

# REMONTA OS DADOS DO ZERO
hist <- readRDS("data/raw/siconfi/stn_hist_ratio.rds")

hist_long <- hist %>%
  pivot_longer(-1, names_to = "data", values_to = "valor") %>%
  rename(uf = 1) %>%
  mutate(
    uf   = trimws(uf),
    year = as.integer(str_extract(data, "\\d{4}")),
    dcl_rcl_pct = as.numeric(gsub(",", ".", valor)) * 100
  ) %>%
  filter(!is.na(year), !is.na(dcl_rcl_pct),
         !uf %in% c("AP", "TO")) %>%
  select(uf, year, dcl_rcl_pct) %>%
  filter(year %in% c(2000, 2001))

painel <- read_csv("data/processed/panel_final_v5.csv",
                   show_col_types = FALSE) %>%
  filter(!uf %in% c("AP", "TO")) %>%
  mutate(dcl_rcl_pct = dcl_sobre_rcl_ext * 100) %>%
  select(uf, year, dcl_rcl_pct)

# Corrige DF nos anos em que era NA no painel
df_hist_df <- readRDS("data/raw/siconfi/stn_hist_ratio.rds") %>%
  pivot_longer(-1, names_to = "data", values_to = "valor") %>%
  rename(uf = 1) %>%
  mutate(
    uf   = trimws(uf),
    mes  = as.integer(str_extract(data, "(?<=/)(\\d{2})(?=/\\d{4})")),
    year = as.integer(str_extract(data, "\\d{4}")),
    dcl_rcl_pct = as.numeric(gsub(",", ".", valor)) * 100
  ) %>%
  filter(uf == "DF", year %in% 2002:2014,
         mes == 12L,                        # só dezembro
         !is.na(dcl_rcl_pct)) %>%
  select(uf, year, dcl_rcl_pct)

painel <- painel %>%
  rows_patch(df_hist_df, by = c("uf", "year"))

# 2025 — arquivo em data/processed/, colunas dcl e rcl
pil_path <- "data/processed/rgf_2025_piloto.csv"
if (file.exists(pil_path)) {
  p25 <- read_csv(pil_path, show_col_types = FALSE) %>%
    filter(!uf %in% c("AP", "TO")) %>%
    mutate(dcl_rcl_pct = (dcl / rcl) * 100,
           year        = 2025L) %>%
    select(uf, year, dcl_rcl_pct)
  painel <- bind_rows(painel, p25)
}

df <- bind_rows(hist_long, painel) %>%
  arrange(uf, year)

# ESTATÍSTICAS
stats <- df %>%
  group_by(year) %>%
  summarise(
    media   = mean(dcl_rcl_pct, na.rm = TRUE),
    mediana = median(dcl_rcl_pct, na.rm = TRUE),
    p25     = quantile(dcl_rcl_pct, 0.25, na.rm = TRUE),
    p75     = quantile(dcl_rcl_pct, 0.75, na.rm = TRUE)
  )

# GRÁFICO — legenda nativa dentro do painel (estilo referência)
ggplot(stats, aes(x = year)) +
  geom_ribbon(aes(ymin = p25, ymax = p75,
                  fill = "Intervalo interquartil (P25–P75)"),
              alpha = 0.7) +
  geom_line(aes(y = mediana, linetype = "Mediana"),
            linewidth = 0.75, color = "grey30") +
  geom_line(aes(y = media, linetype = "Média"),
            linewidth = 1.15, color = "black") +
  scale_fill_manual(
    name   = NULL,
    values = c("Intervalo interquartil (P25–P75)" = "grey80"),
    guide  = guide_legend(
      override.aes = list(linetype = "blank", linewidth = 0, color = NA)
    )
  ) +
  scale_linetype_manual(
    name   = NULL,
    values = c("Média" = "solid", "Mediana" = "dashed"),
    breaks = c("Média", "Mediana"),
    guide  = guide_legend(
      override.aes = list(fill = NA)
    )
  ) +
  scale_x_continuous(
    breaks = seq(2000, max(stats$year), 2),
    expand = expansion(add = c(0, 0.5))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(x = "Ano", y = "DCL / RCL (%)") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text          = element_text(size = 10, color = "black"),
    axis.title         = element_text(size = 11),
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black"),
    legend.position    = c(0.84, 0.84),
    legend.background  = element_blank(),
    legend.key.width   = unit(1.2, "cm"),
    legend.text        = element_text(size = 10),
    legend.spacing.y   = unit(0.05, "cm"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    plot.margin        = margin(5, 15, 5, 5)
  )

ggsave(
  "manuscript/pretextuais/fig1_dcl_rcl_limpo.png",
  width = 8, height = 4.5, dpi = 300
)

# Verificar
cat("Anos:", range(stats$year), "\n")
cat("N por ano:\n")
print(df %>% count(year), n = 30)
