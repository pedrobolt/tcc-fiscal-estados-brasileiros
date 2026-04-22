# =============================================================================
# collect_pessoal.R — Collect Despesas com Pessoal from SICONFI RREO Anexo 01
# Coverage: 2015-2024 (SICONFI start), 25 states
# Variable: despesas_pessoal_rcl = pessoal_liquidado / rcl_ext
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(httr); library(jsonlite)
})

panel <- read_csv("output/panel_final_v5.csv", show_col_types = FALSE)

states <- panel %>%
  distinct(uf, id_ente) %>%
  arrange(uf)

cat(sprintf("States: %d | Years: 2015-2024\n\n", nrow(states)))

# SICONFI parameters
BASE   <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rreo"
PERIOD <- 6   # 6th bimester = full-year cumulative
TARGET_COL    <- "DESPESAS LIQUIDADAS ATÉ O BIMESTRE (h)"
TARGET_CONTAS <- c("PessoalEEncargosSociais", "PessoalEEncargosSociaisIntra")

fetch_pessoal <- function(co_uf, id_ente, year) {
  url <- modify_url(BASE, query = list(
    an_exercicio          = year,
    nr_periodo            = PERIOD,
    co_tipo_demonstrativo = "RREO",
    no_anexo              = "RREO-Anexo 01",
    co_esfera             = "E",
    co_uf                 = co_uf,
    id_ente               = id_ente
  ))
  resp <- tryCatch(GET(url, timeout(30)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NA_real_)
  items <- tryCatch(fromJSON(content(resp, "text", encoding="UTF-8"))$items,
                    error = function(e) NULL)
  if (is.null(items) || nrow(items) == 0) return(NA_real_)
  val <- items %>%
    filter(cod_conta %in% TARGET_CONTAS, coluna == TARGET_COL) %>%
    pull(valor) %>%
    sum(na.rm = TRUE)
  if (length(val) == 0 || val == 0) NA_real_ else val
}

# ── Collect ───────────────────────────────────────────────────────────────────
years   <- 2015:2024
results <- list()
total   <- nrow(states) * length(years)
done    <- 0L

for (i in seq_len(nrow(states))) {
  uf_i  <- states$uf[i]
  ent_i <- states$id_ente[i]
  for (yr in years) {
    done <- done + 1L
    val  <- fetch_pessoal(co_uf = ent_i, id_ente = ent_i, year = yr)
    results[[length(results) + 1]] <- tibble(uf = uf_i, year = yr,
                                              pessoal_liquidado_R = val)
    if (done %% 25 == 0)
      cat(sprintf("  [%d/%d] %s %d → %s\n", done, total, uf_i, yr,
                  if (is.na(val)) "NA" else formatC(val, format="f", digits=0, big.mark=",")))
    Sys.sleep(0.15)
  }
}

pessoal_raw <- bind_rows(results)
n_ok <- sum(!is.na(pessoal_raw$pessoal_liquidado_R))
cat(sprintf("\nAPI collection done: %d/%d state-years with data\n\n", n_ok, total))

# ── Merge and compute ratio ───────────────────────────────────────────────────
panel_v6 <- panel %>%
  left_join(pessoal_raw, by = c("uf", "year")) %>%
  mutate(
    despesas_pessoal_rcl = if_else(
      !is.na(pessoal_liquidado_R) & !is.na(rcl_ext) & rcl_ext > 0,
      pessoal_liquidado_R / (rcl_ext * 1000),   # rcl_ext in R$ thousands → R$
      NA_real_),
    fonte_pessoal = if_else(!is.na(pessoal_liquidado_R), "SICONFI-RREO", NA_character_)
  )

# ── Coverage report ───────────────────────────────────────────────────────────
cat("═══════════════════════════════════════════════════════\n")
cat(" Coverage: despesas_pessoal_rcl\n")
cat("═══════════════════════════════════════════════════════\n")

cov <- panel_v6 %>%
  group_by(uf) %>%
  summarise(
    n_total   = n(),
    n_pessoal = sum(!is.na(despesas_pessoal_rcl)),
    min_yr    = min(year[!is.na(despesas_pessoal_rcl)], default = NA),
    max_yr    = max(year[!is.na(despesas_pessoal_rcl)], default = NA),
    mean_val  = round(mean(despesas_pessoal_rcl, na.rm = TRUE), 3),
    .groups   = "drop"
  )
print(cov, n = 30)

cat(sprintf("\nTotal: %d / %d obs with despesas_pessoal_rcl (%.1f%%)\n",
            sum(!is.na(panel_v6$despesas_pessoal_rcl)),
            nrow(panel_v6),
            100 * mean(!is.na(panel_v6$despesas_pessoal_rcl))))

cat("\nRatio distribution (should be ~0.30–0.65 for states):\n")
q <- quantile(panel_v6$despesas_pessoal_rcl, na.rm = TRUE,
              probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))
print(round(q, 3))

# ── Export ───────────────────────────────────────────────────────────────────
write_csv(panel_v6, "output/panel_final_v6.csv")
cat(sprintf("\n✓ output/panel_final_v6.csv  [%d obs × %d vars]\n",
            nrow(panel_v6), ncol(panel_v6)))
cat("  New vars: pessoal_liquidado_R, despesas_pessoal_rcl, fonte_pessoal\n")
