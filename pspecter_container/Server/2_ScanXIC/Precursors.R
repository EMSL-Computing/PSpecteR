## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains code to generate precursor plots in 2. Scan & Seq

list(
  
  ##################
  ## RENDER PLOTS ##
  ##################
  
  # Generates the matched precursor graphic for the previous MS scan
  output$ssMatPre <- renderPlotly({
    
    # If no ScanMetadata or Sequence, return NULL
    if (is.null(GET_scan_metadata())) {return(NULL)}
    if (is.null(GET_sequence())) {return(NULL)}
    
    # Return NULL if the scan number is an MS1 scan 
    if (GET_scan_number() %in% attributes(GET_scan_metadata())$pspecter$MS1Scans) {return(NULL)}
    
    # Otherwise, make plot
    Pre <- ms1_plots(
      ScanMetadata = GET_scan_metadata(),
      ScanNumber = GET_scan_number(),
      Window = ifelse(is.null(input$MPwinsize), 3, input$MPwinsize),
      Sequence = GET_sequence(),
      IsotopicPercentageFilter = ifelse(is.null(input$MPpercdiff), 25, input$MPpercdiff),
      Interactive = TRUE
    )[[1]] 
    
    plots$currMPPRE <- Pre
    
    return(Pre)
    
  }),
  
  # Generates the matched precursor graphic for the next MS1 scan
  output$ssMatNext <- renderPlotly({
    
    # If no ScanMetadata or Sequence, return NULL
    if (is.null(GET_scan_metadata())) {return(NULL)}
    if (is.null(GET_sequence())) {return(NULL)}
    
    # Return NULL if the scan number is an MS1 scan 
    if (GET_scan_number() %in% attributes(GET_scan_metadata())$pspecter$MS1Scans) {return(NULL)}
    
    Next <- ms1_plots(
      ScanMetadata = GET_scan_metadata(),
      ScanNumber = GET_scan_number(),
      Window = ifelse(is.null(input$MPwinsize), 3, input$MPwinsize),
      Sequence = GET_sequence(),
      IsotopicPercentageFilter = ifelse(is.null(input$MPpercdiff), 25, input$MPpercdiff),
      Interactive = TRUE
    )[[2]]
    
    plots$currMPNEXT <- Next
    
    return(Next)

  })
  
)