#!/usr/bin/env Rscript

installPSpecteR <- function() {
    
  packages <- c( 
    
    # Shiny and DT - ALL ARE CRAN 
    "Rcpp", "BH", "magrittr", "rlang", "R6", 
    "later", "promises", "httpuv", "mime", "jsonlite",
    "xtable", "digest", "htmltools", "sourcetools", 
    "crayon", "shiny", "glue", "shinycssloaders", 
    "labeling", "munsell", "colorspace", "RColorBrewer", 
    "viridisLite", "scales", "shinyWidgets", "shinyBS", "gtools",
    "backports", "ellipsis", "zeallot", "stringi", 
    "assertthat", "utf8", "vctrs", "plyr", "stringr",
    "cli", "fansi", "pillar", "pkgconfig", "gtable", "reshape2",
    "tibble", "withr", "yaml", "lazyeval", "ggplot2", "htmlwidgets", 
    "crosstalk", "DT",
    
    # Plotly - ALL ARE CRAN
    "sys", "askpass", "curl", 
    "openssl", "purrr", "tidyselect",
    "lifecycle", "plogr", "base64enc", "dplyr", "tidyr", "hexbin",
    "data.table", "httr", "plotly",
    
    # Peptides, Seqinr, and later added packages - ALL ARE CRAN
    "Peptides", "ade4", "segmented", "seqinr", "shinyFiles",
    "webshot", "shinyjs", "bit", "bit64", "prettyunits", "blob",
    "DBI", "memoise", "SnowballC", "lsa",
    
    # BiocManager for Bioconductor packages 
    "BiocManager", "XML", "protViz", "RSQLite", "generics", "broom", "dbplyr", "forcats",
    "hms", "readr", 
    "haven", "lubridate", "modelr", "readxl", "reprex", "rvest", "xml2",
    "rstudioapi", "tidyverse"
  )
  
  for (pack in 1:length(packages)) {
    print(""); print(""); print(paste("Installing", packages[pack])); print(""); print("")
    install.packages(packages[pack], INSTALL_opts = c('--no-lock'), repos = "https://cran.rstudio.com")
    tryCatch({library(packages[pack], character.only = TRUE)}, error = function(e) {
      print(""); print(""); print(paste("Function", packages[pack], "did not properly install"))
      print(""); print(""); break
    })                 
  }
  
}

installPSpecteR()