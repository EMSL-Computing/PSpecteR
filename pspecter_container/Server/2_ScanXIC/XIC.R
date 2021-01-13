## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_12_29

# DESCRIPTION: Contains all XIC functions

list(
  
  ###############
  ## OBSERVERS ##
  ###############
  
  # Open XIC panel when XIC tab is clicked
  observe({
    if (is.null(input$SStabs)) {return(NULL)} 
    if (input$SStabs == "XIC") {
      updateCollapse(session, "ssCollapse", open = "6. XIC Settings", close = 
       c("1. Filter Settings", "2. Spectra Settings", "3. Sequence Settings",
         "4. Graphics Settings", "5. Table Column Settings", "7. Export Images & Data"))
    }
  }),
  
  # Change X (M/Z) Spectra Values
  observeEvent(input$XICnewRT, {
    updateSliderInput(session, "XICx", value = c(input$XICminRT, input$XICmaxRT))
  }),
  
  # Change Y (intensity) Spectra Values
  observeEvent(input$XICnewINT, {
    updateSliderInput(session, "XICy", value = c(input$XICminINT, input$XICmaxINT))
  }),
  
  ####################
  ## RENDER WIDGETS ##
  ####################
  
  # Set XIC Mass
  output$massXIC <- renderUI({
    mass <- 0
    if (is.null(getScan()) == F) {
      scan <- getScan()
      clicked <- getScanClick()
      mass <- scan[clicked, "Pre.MZ"]
    }
    PM <- numericInput("premzXIC", "MZ", mass)
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(PM, Desc[Desc$Name == "premzXIC", "Title"], Desc[Desc$Name == "premzXIC", "Description"])
    } else {PM}
  }),
  
  # Set XIC Charge
  output$chargeXIC <- renderUI({
    charge <- 1
    if (is.null(getScan()) == F) {
      scan <- getScan()
      clicked <- getScanClick()
      charge <- scan[clicked, "Pre.Charge"]
    }
    PC <- numericInput("prechXIC", "Charge", charge)
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(PC, Desc[Desc$Name == "prechXIC", "Title"], Desc[Desc$Name == "prechXIC", "Description"])
    } else {PC}
  }),
  
    # Determine XIC X range (RT) 
  output$XICxRange <- renderUI({
    XIC <- getXIC()
    if (is.null(XIC)) {return(NULL)}
    highest <- max(XIC$rt)
    sliderInput("XICx", "Retention Time Range", 0, highest, c(0, highest), 0.1, width = "150%")
  }),
  
  # Determine XIC Y range (INT)
  output$XICyRange <- renderUI({
    XIC <- getXIC()
    if (is.null(XIC)) {return(NULL)}
    highest <- max(XIC$int) + 1
    sliderInput("XICy", "Intensity Range", 0, highest, c(0, highest), 10, width = "150%")
  }),
  
  ##################
  ## RENDER PLOTS ##
  ##################
  
  # Generate XIC warnings
  output$warnXIC <- renderText({
    
    # Test for raw file
    if (is.null(getScan()) || getFileType() == "mzms") {
      HTML('<span style="color: rgb(184, 49, 47);"><strong>
                    <span style="font-size: 18px;">PLEASE UPLOAD A RAW FILE</span>
                    </strong></span>')
    } else if (getScan()[getScanClick(), "MS.Level"] == 1) {
      HTML('<span style="color: rgb(184, 49, 47); font-size: 24px;"><strong>
           PLEASE SELECT AN MS2 SPECTRA</strong></span>')
    }
  
  }), 

  # Plot the XIC
  output$XIC <- renderPlotly({
    
    # Get XIC Data
    XIC <- getXIC()
    if (is.null(XIC)) {return(NULL)}
    
    # Initiate plotly
    p <- plot_ly()
      
    # With the calculations, plot each trace
    for (el in 1:length(unique(XIC$lab))) {
      
      # Use the label to extract out the trace for plotting
      lab <- unique(XIC$lab)[el]
      trace <- XIC[XIC$lab == lab,]
      
      p <- add_trace(p, x = trace$rt, y = trace$int, type = "scatter",
            mode = "lines+markers", line = list(color = trace$color),
            marker = list(color = trace$color), name = trace$lab,
            connectgaps = T, hoverinfo = "text", hovertext = paste(trace$lab, 
            "<br>RT:", round(trace$rt, 3), "min", "<br>Int:", round(trace$int)))
    }

    # Get retention time and max intensity
    lastRT <- getScan()[getScanClick(), "RT"]
    maxInt <- max(XIC$int, na.rm = T)
    preMZ <- input$premzXIC
    preCh <- input$prechXIC
    
    # Define start and stop coordinates
    start <- lastRT - 10
    if (start < 0) {start = 0}
    xRAN <- c(start, lastRT + 5)
    
    # Determine plot title 
    title <- paste("XIC for Mass:", preMZ, "and Charge:", preCh)
    
    # Add plot features
    p <- p %>% layout(xaxis = list(title = "Retention Time (min)", range = xRAN), 
                 yaxis = list(title = "Intensity"), title = title,
                 shapes = list(
                   list(type = "line", line = list(color = "black", dash = "dash"), size = 4, 
                        opacity = 0.3, 
                        x0 = lastRT, x1 = lastRT, xref = "x",
                        y0 = 0, y1 = maxInt + (maxInt / 10), yref = "y")))
    
    plots$currXIC <- p
    
    p
  })
  
)