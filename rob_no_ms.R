suppressPackageStartupMessages({library(dplyr);library(readr);library(fixest)})
panel <- read_csv("output/panel_final_v5.csv", show_col_types=FALSE) %>%
  filter(uf != "MS")
m <- feols(
  primario_sobre_rcl_ext ~ yvar | uf + year |
    d_lag1 + d_lag1_teto ~ d_lag2 + d_lag2_teto,
  data=panel, cluster=~uf)
s <- summary(m)
cat("N obs:", m$nobs, "| States:", n_distinct(panel$uf), "\n")
co <- coef(s)
se <- se(m)
pv <- pvalue(m)
cat(sprintf("β1 (d_lag1, IV)      = %+.4f  SE=%.4f  p=%.4f\n",
  co["fit_d_lag1"], se["fit_d_lag1"], pv["fit_d_lag1"]))
cat(sprintf("β2 (d_lag1_teto, IV) = %+.4f  SE=%.4f  p=%.4f\n",
  co["fit_d_lag1_teto"], se["fit_d_lag1_teto"], pv["fit_d_lag1_teto"]))
cat(sprintf("yvar                 = %+.4f  SE=%.4f  p=%.4f\n",
  co["yvar"], se["yvar"], pv["yvar"]))
print(fitstat(m, ~ivf + wh))
