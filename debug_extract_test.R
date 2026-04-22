# Testa extração corrigida em 3 estados × 2 anos antes do loop completo
source("R/00_setup.R")
source("R/01_siconfi.R")

test_cases <- list(
  list(id=35, uf="SP", year=2022),
  list(id=31, uf="MG", year=2020),
  list(id=53, uf="DF", year=2018)
)

for (tc in test_cases) {
  cat(sprintf("\n=== %s %d ===\n", tc$uf, tc$year))
  df_rgf <- fetch_rgf(tc$id, tc$year)
  res_rgf <- extract_rgf_vars(df_rgf, tc$id, tc$uf, tc$year)
  cat("  DCL (R$ mil):", format(res_rgf$dcl, big.mark="."), "\n")
  cat("  RCL (R$ mil):", format(res_rgf$rcl_rgf, big.mark="."), "\n")

  df_r6 <- fetch_rreo6(tc$id, tc$year)
  res_p <- extract_primario(df_r6, tc$id, tc$uf, tc$year)
  res_e <- extract_encargos(df_r6, tc$id, tc$uf, tc$year)
  cat("  Resultado Primário (R$ mil):", format(res_p$resultado_primario, big.mark="."), "\n")
  cat("  Encargos (R$ mil):          ", format(res_e$encargos_divida, big.mark="."), "\n")

  if (!is.na(res_rgf$dcl) && !is.na(res_rgf$rcl_rgf) && res_rgf$rcl_rgf > 0)
    cat("  DCL/RCL:", round(res_rgf$dcl / res_rgf$rcl_rgf, 3), "\n")
}
cat("\nTeste concluido.\n")
