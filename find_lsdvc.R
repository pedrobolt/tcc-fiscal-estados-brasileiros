pkgs <- available.packages(repos="https://cloud.r-project.org")
hits <- grep("lsdvc|lsdv|kiviet|Bruno|dynpanel", rownames(pkgs),
             value=TRUE, ignore.case=TRUE)
cat("CRAN matches:", paste(hits, collapse=", "), "\n")
