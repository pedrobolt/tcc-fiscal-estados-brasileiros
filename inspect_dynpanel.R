library(dynpanel)
ns <- getNamespace("dynpanel")
all_fns <- ls(ns)
cat("All exported/internal functions:\n")
cat(paste(all_fns, collapse="\n"), "\n\n")

# Inspect dpd.default or dpd.formula
for (fn in grep("dpd", all_fns, value=TRUE)) {
  f <- get(fn, envir=ns)
  cat("---", fn, "---\n")
  cat(paste(head(deparse(formals(f)), 30), collapse="\n"), "\n\n")
}
