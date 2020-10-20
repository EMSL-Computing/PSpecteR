## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_06_29

# DESCRIPTION: Contains other VisPTM plots beside spectra (error map, sequence, precursors)

list(
  
  ##################
  ## RENDER PLOTS ## 
  ##################
  
  # Error Heat Map
  output$VPehm <- renderPlotly({
  
    req(getVPErrorHM())
    EHM <- getVPErrorHM()
      
    # Define specific red to white to blue colorscale (How American!)
    RWB <- list(list(0, 'rgb(179,0,17)'),
                list(0.05, 'rgb(179,0,17)'), list(0.05, 'rgb(189,23,31)'),
                list(0.10, 'rgb(189,23,31)'), list(0.10, 'rgb(199,51,45'), 
                list(0.15, 'rgb(199,51,45'), list(0.15, 'rgb(213,83,61)'), 
                list(0.20, 'rgb(213,83,61)'), list(0.20, 'rgb(225,111,75)'), 
                list(0.25, 'rgb(225,111,75)'), list(0.25, 'rgb(238,140,89)'), 
                list(0.30, 'rgb(238,140,89)'), list(0.30, 'rgb(246,167,112)'), 
                list(0.35, 'rgb(246,167,112)'), list(0.35, 'rgb(246,184,139)'), 
                list(0.40, 'rgb(246,184,139)'), list(0.40, 'rgb(238,201,175)'), 
                list(0.45, 'rgb(238,201,175)'), list(0.45, 'rgb(220,220,220)'), 
                list(0.50, 'rgb(220,220,220)'), list(0.50, 'rgb(255,255,255)'), 
                list(0.55, 'rgb(255,255,255)'), list(0.55, 'rgb(203,207,230)'), 
                list(0.60, 'rgb(203,207,230)'), list(0.60, 'rgb(185,194,239)'), 
                list(0.65, 'rgb(185,194,239)'), list(0.65, 'rgb(137,161,241)'), 
                list(0.70, 'rgb(137,161,241)'), list(0.70, 'rgb(104,138,250)'), 
                list(0.75, 'rgb(104,138,250)'), list(0.75, 'rgb(87,121,248)'), 
                list(0.80, 'rgb(87,121,248)'), list(0.80, 'rgb(63,95,238)'), 
                list(0.85, 'rgb(63,95,238)'), list(0.85, 'rgb(41,66,197)'), 
                list(0.90, 'rgb(41,66,197)'), list(0.90, 'rgb(29,50,188)'), 
                list(0.95, 'rgb(29,50,188)'), list(0.95, 'rgb(17,36,183)'), 
                list(1, 'rgb(17,36,183)'))
      
    HM <- plot_ly(EHM, x = EHM$Pos, y = EHM$Type, z = EHM$MatchScore, type = "heatmap", 
            colorscale = RWB, reversescale = T, hoverinfo = "text", xgap = 3, ygap = 3,
            hovertext = paste("Amino Acid:", EHM$Let, "<br>Ion Type:", EHM$Type, 
            "<br>PPM Error:", round(as.numeric(EHM$MatchScore), 4))) %>% 
          layout(xaxis = list(title = "Amino Acid", tickvals = EHM$Pos,
            ticktext = EHM$Let), yaxis = list(title = "Ion Type"), 
            plot_bgcolor = "#000000")
    
    plots$currVPHM <- HM
    
    HM 
  
  }), 
  
  # Render Sequence with Flags
  output$VPseqflags <- renderPlotly({
    
    # Get flag dataframe
    flagData <- getVPFlagDF()
    if (is.null(flagData)) {return(NULL)}
    
    # Separate into flag dataframe and modifications dataframe
    modData <- flagData[flagData$Peptide == "*",]
    flagData <- flagData[flagData$Peptide != "*",]
    
    # Plot the letters
    p <- plot_ly(flagData, x = flagData$X, y = flagData$Y, mode = "text", name = "",
                 text = ~flagData$Peptide, type = "scatter", hoverinfo = "none",
                 textfont = list(size = 15, color = "black")) 
    
    # Remove modification data
    flagData <- flagData[flagData$Peptide != "*",]
    
    # Determine all the lines that will be needed, ones for sides, and ones 
    # for both the top and the bottom.
    lines <- c()
    
    # Top/bottom lines
    for (i in 1:nrow(flagData)) {
      line <- list(type = "line", line = list(color = flagData$Color[i]), xref = "x", yref = "y")
      if (flagData$Type[i] %in% c("a", "b", "c")) {
        line[["x0"]] <- flagData$X[i] + 0.45
        line[c("y0","y1")] <- flagData$Y[i] - 0.45
        line[["x1"]] <- flagData$X[i] - 0.45} else{
          line[["x0"]] <- flagData$X[i] - 0.45
          line[c("y0", "y1")] <- flagData$Y[i] + 0.45
          line[["x1"]] <- flagData$X[i] + 0.45}
      lines <- c(lines, list(line))}
    
    # Side lines
    for (i in 1:nrow(flagData)) {
      line <- list(type = "line", line = list(color = flagData$Color[i]), xref = "x", yref = "y")
      if (flagData$Type[i] %in% c("a", "b", "c")) {
        line[c("x0", "x1")] <- flagData$X[i] + 0.45
        line[["y0"]] <- flagData$Y[i] - 0.45
        line[["y1"]] <- flagData$Y[i] - 0.1} else{
          line[c("x0", "x1")] <- flagData$X[i] - 0.45
          line[["y0"]] <- flagData$Y[i] + 0.45 
          line[["y1"]] <- flagData$Y[i] + 0.1}
      lines <- c(lines, list(line))}
    
    # Now determine what the plotted values are going to be for each of the ions
    texts <- c()
    for (i in 1:nrow(flagData)) {
      z <- as.character(flagData$z[i])
      if (is.na(z) | z == "1+") {z <- ""}
      text <- HTML(paste('<span style="color: ', flagData$Color[i], '; font-size: 10pt;"> ', 
                         flagData$Type[i], "<sub>", flagData$Ion[i], "</sub><sup>", z, 
                         "</sup></span>", sep = ""))
      texts <- c(texts, text)}
    
    # Now determine the change in y position depending on whether the ion is 
    # abc or xyz. 
    ypos <- c()
    for (i in 1:nrow(flagData)) {
      if (flagData$Type[i] %in% c("a", "b", "c")) {ypos[i] <- flagData$Y[i] - 0.3} else {
        ypos[i] <- flagData$Y[i] + 0.3}}
    
    # Update x and y values to fit the data
    xax <- list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE,
                showgrid = FALSE, range = c(0.5, 15.5))
    yax <- list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE,
                showgrid = FALSE, range = c(-4.5, -0.5))
    
    # Now add all ofthese components together to generate the plot, with how
    # far off each experimental value is from the theoretical value. 
    p <- layout(p, xaxis = xax, yaxis = yax, shapes = lines, showlegend = F) %>%
      add_trace(x = flagData$X, y = ypos, mode = "text", text = texts, name = "",
                hovertext = paste("PPM Error:", round(flagData$MatchScore, 6)), 
                hoverinfo = "text")
    
    # Add modifications if they exist
    if (nrow(modData) > 0) {
      p <- add_trace(p, x = modData$X, y = modData$Y, mode = "text", name = "",
                     text = ~modData$Peptide, hovertext = ~modData$Mod, hoverinfo = "text")
    }
    
    plots$currVPFLAG <- p
      
    p
    
  }),
  
  # Generates the matched precursor graphic for the previous MS scan
  output$VPmatchpre <- renderPlotly({
    
    MatchedPre <- getVPMatchedPrevious()
    if (is.null(MatchedPre)) {return(NULL)}
    
    # Apply percent difference filter
    perc <- input$MPpercdiff
    MatchedPre$Isotopes <- unlist(lapply(1:nrow(MatchedPre), function(row) {
      Perc.Diff <- MatchedPre[row, "Perc.Diff"]
      if (is.na(Perc.Diff) == F && abs(Perc.Diff) > abs(perc/100)) {
        return(NA)
      } else {return(as.character(MatchedPre[row, "Isotopes"]))}
    }))
    
    # Make two traces, and remove isotope intensity data for MatchedPre, and the opposite for MatchedPreIso
    MatchedPreIso <- MatchedPre
    MatchedPre[is.na(MatchedPre$Isotopes) == F, "int"] <- 0
    MatchedPreIso[is.na(MatchedPre$Isotopes), "int"] <- 0
    
    # Get precursor scan number and mz value 
    PreScanNum <- getScan()[getScanClick(), "Pre.Scan"]
    Xpre <- getScan()[getScanClick(), "Pre.MZ"]
    Ypre <- max(MatchedPre$int) * 1.25
    
    # Add the precursor MZ gray line
    RepNum <- nrow(MatchedPre)
    GrayPrecursor <- data.frame(X = rep(Xpre, RepNum), Y = c(rep(0, RepNum - 1), rep(Ypre, 1)))
    
    # Make plot
    P <- plot_ly(MatchedPreIso, x = MatchedPreIso$mz, y = MatchedPreIso$int, type = "scatter",
                 mode = "lines+markers", name = "Theoretical", line = list(color = "rgb(255,0,0)"),
                 marker = list(color = "rgb(255,0,0)", opacity = 0),
                 hoverinfo = "text", hovertext = paste("MZ:", round(MatchedPreIso$mz, 3), "<br>Int:",
                                                       round(MatchedPreIso$int, 0), "<br>Isotope:", MatchedPreIso$Isotope, 
                                                       "<br>Reference Int:", round(MatchedPreIso$Ref.Int, 4), "<br>Percent Diff:", 
                                                       paste(round(MatchedPreIso$Perc.Diff, 4) * 100, "%", sep = ""))) %>%
      add_trace(MatchedPre, x = MatchedPre$mz, y = MatchedPre$int, type = "scatter",
                name = "Experimental", mode = "lines+markers", line = list(color = "rgb(0,0,0)"),
                marker = list("rgb(0,0,0)", opacity = 0),
                hovertext = paste("MZ:", round(MatchedPre$mz, 3), "<br>Int:",
                                  round(MatchedPre$int, 0))) %>%
      add_trace(GrayPrecursor, x = GrayPrecursor$X, y = GrayPrecursor$Y, type = "scatter",
                mode = "lines+markers", name = "Precursor MZ", hoverinfo = "text", 
                hovertext = paste("Precursor MZ:", Xpre),
                line = list(color = "gray", dash = "dash", opacity = 0.5)) %>% 
      layout(title = paste("Previous MS Scan: ", PreScanNum, sep = ""), 
             legend = list(orientation = "h"), yaxis = list(title = "Intensity"))
    
    plots$currVPPRE <- P %>% layout(xaxis = list(title = "M/Z"))
    
    P
    
  }),
  
  # Generates the matched precursor graphic for the next MS1 scan
  output$VPmatchnext <- renderPlotly({
      
    MatchedPre <- getVPMatchedNext()
    if (is.null(MatchedPre)) {return(NULL)}
    
    # Apply percent difference filter
    perc <- input$MPpercdiff
    MatchedPre$Isotopes <- unlist(lapply(1:nrow(MatchedPre), function(row) {
      Perc.Diff <- MatchedPre[row, "Perc.Diff"]
      if (is.na(Perc.Diff) == F && abs(Perc.Diff) > abs(perc/100)) {
        return(NA)
      } else {return(as.character(MatchedPre[row, "Isotopes"]))}
    }))
    
    # Make two traces, and remove isotope intensity data for MatchedPre, and the opposite for MatchedPreIso
    MatchedPreIso <- MatchedPre
    MatchedPre[is.na(MatchedPre$Isotopes) == F, "int"] <- 0
    MatchedPreIso[is.na(MatchedPre$Isotopes), "int"] <- 0
    
    # Get precursor scan number and mz value 
    NextScanNumPos <- match(getScan()[getScanClick(), "Pre.Scan"], getAllMs1()) + 1
    NextScanNum <- getAllMs1()[NextScanNumPos]
    Xpre <- getScan()[getScanClick(), "Pre.MZ"]
    Ypre <- max(MatchedPre$int) * 1.25
    
    # Add the precursor MZ gray line
    RepNum <- nrow(MatchedPre)
    GrayPrecursor <- data.frame(X = rep(Xpre, RepNum), Y = c(rep(0, RepNum - 1), rep(Ypre, 1)))
    
    # Make plot
    P <- plot_ly(MatchedPreIso, x = MatchedPreIso$mz, y = MatchedPreIso$int, type = "scatter",
                 mode = "lines+markers", name = "Theoretical", line = list(color = "rgb(255,0,0)"),
                 marker = list(color = "rgb(255,0,0)", opacity = 0),
                 hoverinfo = "text", hovertext = paste("MZ:", round(MatchedPreIso$mz, 3), "<br>Int:",
                                                       round(MatchedPreIso$int, 0), "<br>Isotope:", MatchedPreIso$Isotope, 
                                                       "<br>Reference Int:", round(MatchedPreIso$Ref.Int, 4), "<br>Percent Diff:", 
                                                       paste(round(MatchedPreIso$Perc.Diff, 4) * 100, "%", sep = ""))) %>%
      add_trace(MatchedPre, x = MatchedPre$mz, y = MatchedPre$int, type = "scatter",
                name = "Experimental", mode = "lines+markers", line = list(color = "rgb(0,0,0)"),
                marker = list("rgb(0,0,0)", opacity = 0),
                hovertext = paste("MZ:", round(MatchedPre$mz, 3), "<br>Int:", round(MatchedPre$int, 0))) %>%
      add_trace(GrayPrecursor, x = GrayPrecursor$X, y = GrayPrecursor$Y, type = "scatter",
                mode = "lines+markers", name = "Precursor MZ", hoverinfo = "text", 
                hovertext = paste("Precursor MZ:", Xpre),
                line = list(color = "gray", dash = "dash", opacity = 0.5)) %>% 
      layout(title = paste("Next MS Scan: ", NextScanNum, sep = ""), 
             legend = list(orientation = "h"), yaxis = list(title = "Intensity"))
    
    plots$currVPNEXT <- P %>% layout(xaxis = list(title = "M/Z"))
    
    P
       
  })
  
)