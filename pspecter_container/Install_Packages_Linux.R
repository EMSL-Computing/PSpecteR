#!/usr/bin/env Rscript

installPSpecteR <- function() {
  
  down <- c()
  
  # Designate folder
  folder <- file.path("macOS_Tgz")
  ext <- ".tar.gz"
  
  ###################
  ## CRAN PACKAGES ##
  ###################
  
  # Create url builder for cran
  cranURL <- function(packVer) {
    pack <- unlist(strsplit(packVer, "_"))[1]
    paste("https://cran.r-project.org/src/contrib/Archive/", 
          pack, "/", paste(packVer, ext, sep = ""), sep = "")
  }
  
  # Packages to install 
  installCmd <- c(
    
    # Shiny and DT - ALL ARE CRAN 
    cranURL("Rcpp_1.0.2"),
    cranURL("BH_1.69.0-1"),
    "magrittr", # Current version is 1.5
    cranURL("rlang_0.4.0"),
    cranURL("R6_2.4.0"),
    cranURL("later_0.8.0"),
    cranURL("promises_1.0.1"),
    cranURL("httpuv_1.5.2"),
    cranURL("mime_0.7"), 
    cranURL("jsonlite_1.6"),
    "xtable", # Current version is 1.8.4
    cranURL("digest_0.6.21"),
    cranURL("htmltools_0.3.6"),
    "sourcetools", # Current version is 0.1.7
    "crayon", # Current version is 1.3.4
    cranURL("shiny_1.3.2"),
    cranURL("glue_1.3.1"),
    cranURL("shinycssloaders_0.2.0"),
    "colorspace", # Current version is 1.4-1
    "labeling", # Current version is 0.3
    "munsell", # Current version is 0.5.0
    "RColorBrewer", # Current version is 1.1-2
    "viridisLite", # Current version is 0.3.0
    cranURL("scales_1.0.0"),
    "shinyWidgets", # The 0.5.0 version installed into R 3.6.1 without issue
    "shinyBS", # Current version is 0.61
    cranURL("gtools_3.8.1"),
    cranURL("backports_1.1.4"),
    cranURL("ellipsis_0.3.0"),
    "zeallot", # Current version is 0.1.0
    cranURL("stringi_1.4.3"),
    "assertthat", # Current version is 0.2.1
    "utf8", # Current version is 1.1.4
    cranURL("vctrs_0.2.0"),
    cranURL("plyr_1.8.4"),
    cranURL("stringr_1.4.0"),
    cranURL("cli_1.1.0"),
    cranURL("fansi_0.4.0"),
    cranURL("pillar_1.4.2"),
    "pkgconfig", # Current version is 2.0.3
    "gtable", # Current version is 0.3.0
    "reshape2", # Current version is 1.4.3 
    cranURL("tibble_2.1.3"), 
    cranURL("withr_2.1.2"), 
    cranURL("yaml_2.2.0"),
    "lazyeval", # Current version is 0.2.2
    cranURL("ggplot2_3.2.1"), 
    cranURL("htmlwidgets_1.3"), 
    cranURL("crosstalk_1.0.0"), 
    cranURL("DT_0.9"),
    
    # Plotly - ALL ARE CRAN
    cranURL("sys_3.3"), 
    "askpass", # Current version is 1.1
    cranURL("curl_4.1"), 
    cranURL("openssl_1.4.1"), 
    cranURL("purrr_0.3.2"),
    cranURL("tidyselect_0.2.5"), 
    cranURL("lifecycle_0.1.0"), 
    "plogr", # Current version is 0.2.0 
    "base64enc", # Current version is 0.1.3
    cranURL("dplyr_0.8.3"), 
    cranURL("tidyr_1.0.0"), 
    cranURL("hexbin_1.27.3"), 
    cranURL("data.table_1.12.2"), 
    "httr", # Current version is 1.4.1
    cranURL("plotly_4.9.0"), 
    
    # Peptides, Seqinr, and later added packages - ALL ARE CRAN
    cranURL("Peptides_2.4.1"), 
    cranURL("ade4_1.7-13"), 
    cranURL("segmented_1.0-0"), 
    "seqinr", # Current version is 3.6.1
    "shinyFiles", # Current version is 0.7.3
    "webshot", # Current version is 0.5.1 
    cranURL("shinyjs_1.0"), 
    
    # Snowball dependencies
    cranURL("bit_1.1-14"),
    cranURL("bit64_0.9-7"),
    cranURL("prettyunits_1.0.2"), 
    cranURL("blob_1.2.0"),
    cranURL("DBI_1.0.0"),
    "memoise", # Current package is 1.1.0
    cranURL("SnowballC_0.6.0"), 
    cranURL("lsa_0.73.1"), 
    
    # mzR, MSnbase, and other BIOCONDUCTOR packages 
    cranURL("BiocManager_1.30.4"),
    "XML", # Current version is 3.98.1.20
    cranURL("protViz_0.4.0"),
    cranURL("RSQLite_2.1.2"),
    "generics", # Current Version is 0.0.2
    cranURL("broom_0.5.2"),
    cranURL("dbplyr_1.4.2"),
    cranURL("forcats_0.4.0"),
    cranURL("hms_0.5.1"),
    "readr", # Current Version is 1.3.1
    cranURL("haven_2.1.1"),
    cranURL("lubridate_1.7.4"),
    cranURL("modelr_0.1.5"),
    "readxl", # Current Version is 1.3.1
    "reprex", # Current Version is 0.3.0
    "rvest", # Current Version is 0.3.4
    cranURL("xml2_1.2.2"),
    cranURL("rstudioapi_0.10"),
    cranURL("tidyverse_1.2.1")
    
  )
  
  packages <- c( 
    
    # Shiny and DT - ALL ARE CRAN 
    "Rcpp", "BH", "magrittr", "rlang", "R6", 
    "later", "promises", "httpuv", "mime", "jsonlite",
    "xtable", "digest", "htmltools", "sourcetools", 
    "crayon", "shiny", "glue", "shinycssloaders", 
    "colorspace", "labeling", "munsell", "RColorBrewer", 
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
    install.packages(installCmd[pack], INSTALL_opts = c('--no-lock'))
    tryCatch({library(packages[pack], character.only = TRUE)}, error = function(e) {
      print(""); print(""); print(paste("Function", packages[pack], "did not properly install"))
      print(""); print(""); break
    })                 
  }
  
  ##################
  ## BIOCONDUCTOR ##
  ##################
  
  # Create Bioconductor installer from github 
  bioURL <- function(packVer) {
    pack <- unlist(strsplit(packVer, "_"))[1]
    file.path("https://github.com/Bioconductor", pack, "tree/RELEASE_3_10")
  }
  
  # Test new packages 
  biocInstall <- c("mzR", "MSnbase", "rhdf5", "BRAIN", "Rdisop")
  
  for (pack in 1:length(biocInstall)) {
    print(""); print(""); print(paste("Installing", biocInstall[pack])); print(""); print("")
    BiocManager::install(biocInstall[pack], update = FALSE, ask = FALSE)
    tryCatch({library(biocInstall[pack], character.only = TRUE)}, error = function(e) {
      print(""); print(""); print(paste("Function", biocInstall[pack], "did not properly install"))
      print(""); print(""); break
    })                 
  }
  
  ############
  ## GITHUB ##
  ############
  
  # Install rawDiag
  # Moved outside script because rawDiag sometimes doesn't properly install 
  
}

installPSpecteR()