# =============================================================================
# sanity_check.R  —  Final diagnostics before estimation
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})

panel <- read_csv("output/panel_final_v5.csv", show_col_types = FALSE)

hdr <- function(n, title) {
  cat(sprintf("\n%s\n %d. %s\n%s\n", strrep("═",56), n, title, strrep("═",56)))
}

flag <- function(cond, msg) if (cond) cat("  *** FLAG:", msg, "***\n") else cat("  ✓", msg, "\n")

# ── 1. PANEL STRUCTURE ───────────────────────────────────────────────────────
hdr(1, "PANEL STRUCTURE")

n_states <- n_distinct(panel$uf)
n_years  <- n_distinct(panel$year)
states   <- sort(unique(panel$uf))

cat(sprintf("  Obs: %d | States: %d | Years: %d (%d–%d)\n",
            nrow(panel), n_states, n_years, min(panel$year), max(panel$year)))
flag(nrow(panel) == 575, "575 obs")
flag(n_states == 25,     "25 states")
flag(n_years  == 23,     "23 years (2002–2024)")
flag(!"AP" %in% states,  "AP excluded")
flag(!"TO" %in% states,  "TO excluded")

cat("\n  States:", paste(states, collapse=" "), "\n")

obs_per_state <- panel %>% count(uf) %>% filter(n != 23)
flag(nrow(obs_per_state) == 0, "all states have 23 obs")
if (nrow(obs_per_state) > 0) print(obs_per_state)

# ── 2. SCALE CHECK ───────────────────────────────────────────────────────────
hdr(2, "SCALE CHECK — KEY VARIABLES")

vars_exp <- list(
  list(v="dcl_sobre_rcl_ext",      lo=0,    hi=3,   lbl="0 to ~3"),
  list(v="primario_sobre_rcl_ext", lo=-0.30,hi=0.30, lbl="-0.30 to +0.30"),
  list(v="encargos_sobre_rcl_ext", lo=0,    hi=0.30, lbl="0 to ~0.30"),
  list(v="yvar",                   lo=-0.20,hi=0.20, lbl="-0.20 to +0.20"),
  list(v="crescimento_pib_pct",    lo=-20,  hi=30,   lbl="-20 to +30"),
  list(v="teto",                   lo=12,   hi=15,   lbl="12/13/15 only"),
  list(v="d_lag1",                 lo=0,    hi=3,   lbl="0 to ~3"),
  list(v="d_lag2",                 lo=0,    hi=3,   lbl="0 to ~3")
)

for (ve in vars_exp) {
  x   <- panel[[ve$v]]
  mn  <- min(x,    na.rm=TRUE)
  mx  <- max(x,    na.rm=TRUE)
  med <- median(x, na.rm=TRUE)
  mu  <- mean(x,   na.rm=TRUE)
  sg  <- sd(x,     na.rm=TRUE)
  nas <- sum(is.na(x))
  warn <- (mn < ve$lo - abs(ve$lo)*0.5 - 0.01) | (mx > ve$hi + abs(ve$hi)*0.5 + 0.01)
  cat(sprintf("\n  %-28s  NA=%d\n    min=%.4f  med=%.4f  max=%.4f  mean=%.4f  sd=%.4f",
              ve$v, nas, mn, med, mx, mu, sg))
  if (warn) cat(sprintf("  *** OUTSIDE EXPECTED [%.2f, %.2f] ***", ve$lo, ve$hi))
  cat("\n")
}

cat("\n  teto values present:", paste(sort(unique(panel$teto)), collapse=", "), "\n")

# ── 3. INSTRUMENT RELEVANCE ──────────────────────────────────────────────────
hdr(3, "INSTRUMENT RELEVANCE")

ok1 <- panel %>% filter(!is.na(d_lag1), !is.na(d_lag2))
ok2 <- panel %>% filter(!is.na(d_lag1_teto), !is.na(d_lag2_teto))

r1 <- cor(ok1$d_lag1,      ok1$d_lag2)
r2 <- cor(ok2$d_lag1_teto, ok2$d_lag2_teto)

cat(sprintf("  cor(d_lag1, d_lag2)            = %.4f  (N=%d)\n", r1, nrow(ok1)))
cat(sprintf("  cor(d_lag1_teto, d_lag2_teto)  = %.4f  (N=%d)\n", r2, nrow(ok2)))
flag(r1 > 0.9, sprintf("cor(d_lag1, d_lag2) = %.4f > 0.90", r1))
flag(r2 > 0.9, sprintf("cor(d_lag1_teto, d_lag2_teto) = %.4f > 0.90", r2))

# ── 4. INTERACTION VARIABLES ─────────────────────────────────────────────────
hdr(4, "INTERACTION VARIABLES — SPOT CHECK")

set.seed(42)
samp <- panel %>%
  filter(!is.na(d_lag1), !is.na(d_lag2), !is.na(primario_sobre_rcl_ext)) %>%
  slice_sample(n = 5) %>%
  select(uf, year, teto, d_lag1, d_lag2, primario_sobre_rcl_ext,
         d_lag1_teto, d_lag2_teto, primario_teto)

print(samp)

cat("\n  Verify d_lag1_teto == d_lag1 * teto:\n")
check_int <- panel %>%
  filter(!is.na(d_lag1), !is.na(d_lag1_teto)) %>%
  mutate(
    diff1 = abs(d_lag1_teto     - d_lag1 * teto),
    diff2 = abs(d_lag2_teto     - d_lag2 * teto),
    diff3 = abs(primario_teto   - primario_sobre_rcl_ext * teto)
  )
cat(sprintf("  max |d_lag1_teto   - d_lag1*teto|   = %.2e\n", max(check_int$diff1, na.rm=TRUE)))
cat(sprintf("  max |d_lag2_teto   - d_lag2*teto|   = %.2e\n", max(check_int$diff2, na.rm=TRUE)))
cat(sprintf("  max |primario_teto - primario*teto| = %.2e\n", max(check_int$diff3, na.rm=TRUE)))

# ── 5. TETO ASSIGNMENT ───────────────────────────────────────────────────────
hdr(5, "TETO ASSIGNMENT")

expected <- list(
  `12` = c("AC","AM","CE","PE","RR"),
  `13` = c("BA","DF","ES","MA","MG","PB","PI","PR","RJ","RN","RS","SC","SE","SP"),
  `15` = c("AL","GO","MS","MT","PA","RO")
)

teto_actual <- panel %>% distinct(uf, teto) %>% arrange(teto, uf)
cat("\n  Count by teto:\n")
teto_actual %>% count(teto) %>% print()

for (t in names(expected)) {
  act <- teto_actual %>% filter(teto == as.integer(t)) %>% pull(uf) %>% sort()
  exp <- sort(expected[[t]])
  if (!identical(act, exp)) {
    cat(sprintf("  *** teto=%s: expected [%s] got [%s] ***\n",
                t, paste(exp, collapse=" "), paste(act, collapse=" ")))
  } else {
    cat(sprintf("  ✓ teto=%s: %s\n", t, paste(act, collapse=" ")))
  }
}

# ── 6. ECONOMIC SANITY ───────────────────────────────────────────────────────
hdr(6, "ECONOMIC SANITY CHECKS")

cat("\n  Mean dcl_sobre_rcl_ext by state (top 10):\n")
panel %>%
  group_by(uf) %>%
  summarise(mean_dcl = round(mean(dcl_sobre_rcl_ext, na.rm=TRUE), 3), .groups="drop") %>%
  arrange(desc(mean_dcl)) %>%
  print(n = 10)

cat("\n  Mean primario_sobre_rcl_ext by state:\n")
prim_means <- panel %>%
  group_by(uf) %>%
  summarise(mean_p = round(mean(primario_sobre_rcl_ext, na.rm=TRUE), 4), .groups="drop") %>%
  arrange(mean_p)
print(prim_means, n=25)

deficit_states <- prim_means %>% filter(mean_p < -0.05)
if (nrow(deficit_states) > 0) {
  cat("\n  *** States with persistent deficit (mean < -0.05):",
      paste(deficit_states$uf, collapse=", "), "***\n")
} else {
  cat("\n  ✓ No state has persistent mean deficit < -0.05\n")
}

cat("\n  Missing data by year (key vars):\n")
panel %>%
  group_by(year) %>%
  summarise(
    na_dcl  = sum(is.na(dcl_sobre_rcl_ext)),
    na_prim = sum(is.na(primario_sobre_rcl_ext)),
    na_enc  = sum(is.na(encargos_sobre_rcl_ext)),
    na_yvar = sum(is.na(yvar)),
    na_pib  = sum(is.na(crescimento_pib_pct)),
    .groups = "drop"
  ) %>%
  filter(na_dcl > 0 | na_prim > 0 | na_enc > 0 | na_yvar > 0 | na_pib > 0) %>%
  print(n = 30)

# ── 7. MODEL SAMPLE SIZES ────────────────────────────────────────────────────
hdr(7, "MODEL SAMPLE SIZES")

m1_vars <- c("primario_sobre_rcl_ext","d_lag1","d_lag2",
             "d_lag1_teto","d_lag2_teto","yvar")
m2_vars <- c("dcl_sobre_rcl_ext","d_lag1",
             "primario_sobre_rcl_ext","encargos_sobre_rcl_ext",
             "crescimento_pib_pct","primario_teto")

for (nm in c("Model I","Model II")) {
  vv <- if (nm == "Model I") m1_vars else m2_vars
  sub <- panel %>% filter(if_all(all_of(vv), ~ !is.na(.)))
  cat(sprintf("\n  %s (%s):\n    N = %d | years %d–%d | states: %d\n",
              nm, paste(vv, collapse=", "),
              nrow(sub), min(sub$year), max(sub$year), n_distinct(sub$uf)))
  cat("    Obs/year:\n")
  sub %>% count(year) %>% print(n=23)
}

# ── 8. FINAL VERDICT ─────────────────────────────────────────────────────────
hdr(8, "FINAL VERDICT")

issues <- c()

if (nrow(panel)   != 575)                              issues <- c(issues, "obs count ≠ 575")
if (n_states      != 25)                               issues <- c(issues, "states ≠ 25")
if ("AP" %in% states || "TO" %in% states)              issues <- c(issues, "AP or TO present")
if (max(abs(panel$dcl_sobre_rcl_ext), na.rm=TRUE) > 4) issues <- c(issues, "dcl_sobre_rcl_ext has implausible values")
if (max(abs(panel$primario_sobre_rcl_ext), na.rm=TRUE) > 0.5) issues <- c(issues, "primario_sobre_rcl_ext out of range")
if (max(panel$yvar, na.rm=TRUE) > 0.25 ||
    min(panel$yvar, na.rm=TRUE) < -0.25)               issues <- c(issues, "yvar has extreme values")
if (r1 < 0.9)                                          issues <- c(issues, "instrument correlation too low")
if (!all(sort(unique(panel$teto)) == c(12,13,15)))     issues <- c(issues, "unexpected teto values")

if (length(issues) == 0) {
  cat("\n  ✓✓✓  READY FOR ESTIMATION  ✓✓✓\n\n")
} else {
  cat("\n  ISSUES TO FIX BEFORE PROCEEDING:\n")
  for (i in seq_along(issues)) cat(sprintf("    %d. %s\n", i, issues[i]))
}
