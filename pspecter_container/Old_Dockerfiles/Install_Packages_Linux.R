#!/usr/bin/env Rscript

installPSpecteR <- function() {
  
  ## Install CRAN packages 
    
  packages <- c("devtools", "DT", "plotly", "data.table", "shiny", "shinycssloaders", "shinyWidgets",
                "shinyBS", "shinyjs", "shinyjqui", "shinyFiles", "xlsx")
  
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