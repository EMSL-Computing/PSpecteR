## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains code to generate precursor plots in 2. Scan & Seq

list(
  
  ###############
  ## OBSERVERS ##
  ###############
  
  # Make Previous Precursor Large
  observeEvent(input$MPlargePre, {
    showModal(modalDialog(fluidPage(
      plotlyOutput("bigMPLP", width = "100%", height = "700px") %>% 
        withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
    title = HTML('<p style="text-align: center;">View Previous Precursor Full Screen</p>'),
    footer = modalButton("Exit"), size = "l", easyClose = T))
  }),
  
  # Make Next Precursor Large
  observeEvent(input$MPlargeNext, {
    showModal(modalDialog(fluidPage(
      plotlyOutput("bigMPNP", width = "100%", height = "700px") %>% 
        withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
    title = HTML('<p style="text-align: center;">View Next Precursor Full Screen</p>'),
    footer = modalButton("Exit"), size = "l", easyClose = T))
  }),
  
  ##################
  ## RENDER PLOTS ##
  ##################
  
  # Generates the matched precursor graphic for the previous MS scan
  output$ssMatPre <- renderPlotly({
    
    NULL

  }),
  
  # Generates the matched precursor graphic for the next MS1 scan
  output$ssMatNext <- renderPlotly({
    
    # Get next matched precursor data 
    MatchedPre <- getMatchedNext()
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
                 "<br>Reference Int:", round(MatchedPreIso$Ref.Int, 4), "<br>Percent Err:", 
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
    
    plots$currMPNEXT <- P %>% layout(xaxis = list(title = "M/Z"))
    
    plotly::toWebGL(P)

  }),
   
  # Plot new previous precursor
  output$bigMPLP <- renderPlotly({plotly::toWebGL(plots$currMPPRE)}),
  
  # Plot new previous precursor
  output$bigMPNP <- renderPlotly({plotly::toWebGL(plots$currMPNEXT)})
  
)