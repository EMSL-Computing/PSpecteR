## David Degnan, Pacific Northwest National Labs
## Last Updated: 2020_09_24

# DESCRIPTION: This contains the modal for exporting the FRAG csv.

list(

# Get all figures 
getAllVisPTMFigs <- function(VisPTM) {
  
  # Initiate List to hold all Vis PTM Figures
  AllVisPTMFigs <- list()
  
  # Start progress bar for each generated spectra
  withProgress({
    
    revals$PTMmarkdown <- T
    
    incProgress(amount = 0.5, "Generating Spectra")
    
    for (row in 1:nrow(VisPTM)) {
      
      # Add one to the PTMread for getModFrag
      revals$PTMread <- revals$PTMread + 1
      
      incProgress(0, paste("Exporting Markdown: ", round(row/nrow(VisPTM), 4) * 100, "%", sep = ""))
      
      # Get the peak data and scan number
      peak <- getSSPeak()
      scan <- getScan()
      scanNum <- scan[getScanClick(), "Scan.Num"]
      
      # Now get the fragment data and the original sequence DF
      frag <- getModFrag() 
      oriSDF <- getModOriSDF()
      
      # Get modification name
      modName <- VisPTM[as.character(row), "Name"]
    
      ##############################################
      ## Step 1: Plot spectra without annotations ##
      ##############################################
      
      # Add 0's to peak data and sort 
      len <- nrow(peak)
      spectra <- data.frame("mzExp" = c(peak$mz - 1e-9, peak$mz, peak$mz + 1e-9),
                            "intensityExp" = c(rep(0, len), peak$intensity, rep(0, len)))
      spectra <- spectra[order(spectra$mzExp),]
      
      # Add missing data 
      spectra$ion <- spectra$type <- spectra$z <- spectra$isoPeak <- NA
      
      # Plot spectra
      p <- plot_ly(x = spectra$mzExp, y = spectra$intensityExp, type = "scatter",
                   mode = "lines+markers", line = list(color = "black"), 
                   name = "Spec", marker = list(opacity = 0, color = "black"), hoverinfo = "text", 
                   hovertext = paste(paste("MZ:", round(spectra$mzExp, 3), 
                                           "<br>Int:", round(spectra$intensityExp, 0))))
      
      ##############################
      ## Step 2: Plot annotations ##
      ##############################
      
      # This step only occurs if they are fragments
      if (is.null(frag) == F) {
        
        # Set colors list
        colors <- c("a" = "forestgreen", "b" = "steelblue", "c" = "darkviolet",
                    "x" = "rgb(172, 122, 122)", "y" = "red", "z" = "darkorange")
        
        # Subset out fragment data 
        frag <- frag[,c("mzExp", "intensityExp", "ion", "type", "z", "isoPeak")]
        
        for (type in c("a", "b", "c", "x", "y", "z")) {
          fragSub <- frag[frag$type == type,]
          len <- nrow(fragSub)
          spectraAnno <- data.frame("mzExp" = c(fragSub$mzExp - 1e-9, fragSub$mzExp, fragSub$mzExp + 1e-9),
                                    "intensityExp" = c(rep(0, len), fragSub$intensityExp, rep(0, len)),
                                    "ion" = rep(fragSub$ion, 3), "z" = rep(fragSub$z, 3), 
                                    "isoPeak" = rep(fragSub$isoPeak, 3))
          spectraAnno <- spectraAnno[order(spectraAnno$mzExp),]
          p <- add_trace(p, x = spectraAnno$mzExp, y = spectraAnno$intensity, type = "scatter",
                         mode = "lines+markers", line = list(color = colors[type]), 
                         name = type, marker = list(opacity = 0), 
                         hoverinfo = "text", hovertext = paste(paste("Ion: ", spectraAnno$ion, 
                                                                     "<sup>", spectraAnno$z, "</sup> ", spectraAnno$isoPeak, sep = ""), "<br>MZ:",
                                                               round(spectraAnno$mzExp, 3), "<br>Int:", round(spectraAnno$intensity, 0)))
          
          # Add labels if enabled
          if (is.null(input$ssLetter) == F && input$ssLetter == T & is.null(getFrag()) == F
              && nrow(getFrag()) > 0 & getScan()[getScanClick(), "MS.Level"] != 1 & 
              nrow(spectraAnno) > 0) {
            
            # Remove 0 from labeling
            toLabel <- spectraAnno[spectraAnno$intensityExp > 0,]
            
            # Add spacing
            dist <- input$ssLabDist
            if (dist == "Very Close") {dist <- 2e5} else if (dist == "Close") {dist <- 2e4} else
              if (dist == "Near") {dist <- 2e3} else if (dist == "Far") {dist <- 2e2} else {dist <- 2e1}
            adjValX <- max(spectra$mzExp, na.rm = T) / dist
            adjValY <- max(spectra$intensityExp, na.rm = T) / dist
            
            # Get and plot labels
            for (i in 1:nrow(toLabel)) {
              text <- list(
                x = toLabel$mzExp[i] + adjValX, y = toLabel$intensityExp[i] + adjValY,
                text = HTML(paste('<span style="color: ', colors[type], '; font-size: ', 
                                  input$ssAnnoSize, 'pt;"> ', toLabel$ion[i], "<sup>", 
                                  toLabel$z[i], "</sup>, ", toLabel$isoPeak[i], "</span>", sep = "")),
                xref = "x", yref = "y", showarrow = FALSE)
              p <- p %>% layout(annotations = text)
            }}
        }}
    
      # Add plot name
      plotName <- paste("Name: ", VisPTM[as.character(row), "Name"], ", Average.PPM.Error: ",
              round(VisPTM[as.character(row), "Average.PPM.Error"], 4), ", Number.of.Ions: ",
              VisPTM[as.character(row), "Number.of.Ions"], ", Coverage: ", 
              VisPTM[as.character(row), "Coverage"], sep = "")
      p <- p %>% layout(p, xaxis = list(title = "M/Z (Mass to Charge)"),
                 yaxis = list(title = "Intensity"), 
                 title = list(text = plotName, font = list(size = 10)))
        
      AllVisPTMFigs[[plotName]] <- p
      
    }
    
    incProgress(amount = 0.5, "Done!")
  })
  
  revals$PTMmarkdown <- F
  revals$PTMread <- 0
  
  return(AllVisPTMFigs)

},

output$VPmarkdown <- downloadHandler(
  filename = function() {
    basename <- "PSpecteR"
    if (is.null(msPath()) == F) {
      basename <- unlist(strsplit(unlist(strsplit(msPath(), "/"))[-1], ".", fixed = T))[1]
    }
    paste(basename, "_VisPTM_", format(Sys.time(), "%y%m%d_%H%M%S"), ".html", sep = "")
  },
  content = function(file) {
    
    withProgress({
      
      incProgress(0.1, "Starting HTML Export")
      
      # Get R Markdown file
      RmdFile <- file.path("Server", "Export", "VisPTM_Export.Rmd")
      
      incProgress(0.3, "Collecing Parameter Data")
      
      # Declare ion group information
      ionGroups <- input$ionGroups
      if (is.null(ionGroups)) {ionGroups <- c("a", "b", "c", "x", "y", "z", "Spec")}
      
      # Put all the parameters to be passed to the markdown file into a list
      params <- list(scanNum = getScan()[getScanClick(), "Scan.Num"], 
                     seq = getNewVSeq(), 
                     ions = ionGroups, 
                     isoPer = input$ssIsoPerMin,
                     fragTol = input$ssTolerance, 
                     intMin = input$ssIntenMin,
                     corrScore = input$ssCorrScoreFilter,
                     VPMetFigs = getAllVisPTMFigs(getVPMetrics()[order(-getVPMetrics()$Number.of.Ions),]))
      
      # Insert directory information for pandoc
      if (Sys.info()["sysname"] == "Windows") {
        Sys.setenv(RSTUDIO_PANDOC=file.path("C:", "Program Files", "Rstudio", "bin", "pandoc"))} else 
      if (Sys.info()["sysname"] == "Darwin") {
        Sys.setenv(RSTUDIO_PANDOC=file.path("", "Applications", "RStudio.app", "Contents", "MacOS", "pandoc"))
      }
      
      incProgress(0.3, "Writing HTML file")
      
      # Print Markdown
      rmarkdown::render(RmdFile, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
      incProgress(0.2, "Finished HTML file")
      
    })
  })
  
)


# Export markdown
#observeEvent(input$VPmarkdown, {
#  
#  if (is.null(getScan())) {
#    sendSweetAlert(session, "No MS Data Uploaded", "Upload MS Data", type = "error")
#    return(NULL)
#  }
#  
#  if (is.null(getID())) {
#    sendSweetAlert(session, "No ID Data Upload", "Upload ID Data", type = "error")
#    return(NULL)
#  }
  
  # Get VP Metrics
#  if (is.null(getVPMetrics())) {
#    sendSweetAlert(session, "The Vis PTM Table is Empty.", 
#                   "Let David know if you ever see this error.", type = "error")
#    return(NULL)
#  }
  
  # Get R Markdown file
#  RmdFile <- file.path("Server", "Export", "VisPTM.Rmd")
  
  # Determine if figures folder exists and if not make it
#  out <- file.path(outputPath(), "Figures")
#  if (dir.exists(out) == F) {dir.create(out)}
  
  # Name the markdown file and copy to the proper folder
#  plots$MDindex <- plots$MDindex + 1
#  outRmd <- file.path(out, paste(plots$MDindex, "_VisPTM.Rmd", sep = ""))
#  file.copy(RmdFile, outRmd, overwrite = T)
  
  # Set the name of the outputted HTML file
#  outHTML <- file.path(out, paste(plots$MDindex, "_VisPTM.html", sep = ""))
  
  # PDeclare ion group information
#  ionGroups <- input$ionGroups
#  if (is.null(ionGroups)) {ionGroups <- c("a", "b", "c", "x", "y", "z", "Spec")}
  
  # Put all the parameters to be passed to the markdown file into a list
#  params <- list(scanNum = getScan()[getScanClick(), "Scan.Num"], 
#                 seq = getNewVSeq(), 
#                 ions = ionGroups, 
#                 isoPer = input$ssIsoPerMin,
#                 fragTol = input$ssTolerance, 
#                 intMin = input$ssIntenMin,
#                 corrScore = input$ssCorrScoreFilter,
#                 VPMetFigs = getAllVisPTMFigs(getVPMetrics()[order(-getVPMetrics()$Number.of.Ions),]))
  
  # Insert directory information for pandoc
#  if (Sys.info()["sysname"] == "Windows") {
#    Sys.setenv(RSTUDIO_PANDOC=file.path("C:", "Program Files", "Rstudio", "bin", "pandoc"))} else {
#    Sys.setenv(RSTUDIO_PANDOC=file.path("", "Applications", "RStudio.app", "Contents", "MacOS", "pandoc"))}
  
  # Print Markdown
#  rmarkdown::render(outRmd, output_file = outHTML,
#                    params = params,
#                    envir = new.env(parent = globalenv()))
  
  # Write to the PSpecteR Session Info
#  write(paste("  Vis PTM Markdown exported at ", format(Sys.time(), "%R:%S"), sep = ""),
#        file = file.path(outputPath(), "PSpecteR_Session_Info.txt"), append = T)
  
  # Open an HTML in R
#  system2("open", outHTML)
  
#})