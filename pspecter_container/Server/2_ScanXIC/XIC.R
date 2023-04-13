## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

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
    if (is.null(GET_scan_metadata()) == F) {
      scan <- GET_scan_metadata()
      clicked <- GET_scan_click()
      mass <- scan[clicked, "Precursor M/Z"]
    }
    PM <- numericInput("premzXIC", HTML("<p><em>m/z</em></p>"), mass)
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(PM, Desc[Desc$Name == "premzXIC", "Title"], Desc[Desc$Name == "premzXIC", "Description"])
    } else {PM}
  }),
  
    # Determine XIC X range (RT) 
  output$XICxRange <- renderUI({
    XIC <- GET_XIC()
    if (is.null(XIC)) {return(NULL)}
    highest <- max(XIC$RT)
    sliderInput("XICx", "Retention Time Range", 0, highest, c(0, highest), 0.1, width = "150%")
  }),
  
  # Determine XIC Y range (INT)
  output$XICyRange <- renderUI({
    XIC <- GET_XIC()
    if (is.null(XIC)) {return(NULL)}
    highest <- max(XIC$Intensity) + 1
    sliderInput("XICy", "Intensity Range", 0, highest, c(0, highest), 10, width = "150%")
  }),
  
  # Smooth switch
  output$XICsmoothSWITCH <- renderUI({
    XS <- materialSwitch("XICsmooth", HTML("<strong>Smooth XIC Lines?</strong>"), value = F, status = "success")
    XS
  }),
  
  ##################
  ## RENDER PLOTS ##
  ##################

  # Plot the XIC
  output$XIC <- renderPlotly({
    
    # Get XIC Data
    if (is.null(GET_XIC())) {return(NULL)}
    
    p <- xic_plot(
      XICobj = GET_XIC(),
      Smooth = ifelse(is.null(input$XICsmooth), FALSE, input$XICsmooth),
      Interactive = TRUE
    )
 
    plots$currXIC <- p
    
    p
  })
  
)