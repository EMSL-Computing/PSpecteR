## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains code to generate precursor plots in 2. Scan & Seq

list(
  
  ##################
  ## RENDER PLOTS ##
  ##################
  
  # Generates the matched precursor graphic for the previous MS scan
  output$ssMatPre <- renderPlotly({
    
    # If no ScanMetadata, return NULL
    if (is.null(GET_scan_metadata())) {return(NULL)}
    if (is.null(input$ssNewSeq)) {return(NULL)}
    
    Pre <- ms1_plots(
      ScanMetadata = GET_scan_metadata(),
      ScanNumber = GET_scan_number(),
      Window = ifelse(is.null(input$MPwinsize), 3, input$MPwinsize),
      Sequence = input$ssNewSeq,
      IsotopicPercentageFilter = ifelse(is.null(input$MPpercdiff), 25, input$MPpercdiff),
      Interactive = TRUE
    )[[1]] 
    
    plots$currMPPRE <- Pre
    
    return(Pre)
    
  }),
  
  # Generates the matched precursor graphic for the next MS1 scan
  output$ssMatNext <- renderPlotly({
    
    # If no ScanMetadata, return NULL
    if (is.null(GET_scan_metadata())) {return(NULL)}
    if (is.null(input$ssNewSeq)) {return(NULL)}
    
    Next <- ms1_plots(
      ScanMetadata = GET_scan_metadata(),
      ScanNumber = GET_scan_number(),
      Window = ifelse(is.null(input$MPwinsize), 3, input$MPwinsize),
      Sequence = input$ssNewSeq,
      IsotopicPercentageFilter = ifelse(is.null(input$MPpercdiff), 25, input$MPpercdiff),
      Interactive = TRUE
    )[[2]]
    
    plots$currMPNEXT <- Next
    
    return(Next)

  }),
   
  # Plot new previous precursor
  output$bigMPLP <- renderPlotly({plotly::toWebGL(plots$currMPPRE)}),
  
  # Plot new previous precursor
  output$bigMPNP <- renderPlotly({plotly::toWebGL(plots$currMPNEXT)})
  
)