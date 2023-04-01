## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains all the protein coverage tables and figures

list(
  
  ###############
  ## OBSERVERS ##
  ###############
  
  # Change PT X (Amino Acid) Values
  observeEvent(input$PTposNew, {
    updateSliderInput(session, "PTx", value = c(input$PTposMin, input$PTposMax))
  }),
  
  # Change XIC Y (intensity) Spectra Values
  observeEvent(input$PTscanNew, {
    updateSliderInput(session, "PTy", value = c(input$PTscanMin, input$PTscanMax))
  }),
  
  ####################
  ## RENDER WIDGETS ##
  ####################
  
  # Render "Remove Contaminants" switch
  output$PTContSWITCH <- renderUI({
    PC <- materialSwitch("PTCont", HTML("<strong>Remove Contaminants?</strong>"), value = T, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
    popify(PC, Desc[Desc$Name == "PTCont", "Title"], Desc[Desc$Name == "PTCont", "Description"],
           options = list(selector = '.material-switch'), placement = 'right')
    } else {PC}
  }),
  
    # Set the PT Amino Acid Position (X variable) Range
  output$PTxRange <- renderUI({
    
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    
    browser()
    
    
    PT <- getProteinTree()
    if (is.null(PT)) {return(NULL)}
    highest <- nchar(PT[PT$Scan == 1,]$Sequence) + 1
    sliderInput("PTx", "Amino Acid Position Range", 0, highest, c(0, highest), 0.01,
                width = "150%")
  }),
  
  # Set the PT Scan Number (Y variable) Range
  output$PTyRange <- renderUI({
    
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    
    browser()
    
    PT <- getProteinTree()
    if (is.null(PT)) {return(NULL)}
    highest <- max(PT$Scan, na.rm = T) + 1
    sliderInput("PTy", "Scan Number Range", 0, highest, c(0, highest), 1, width = "150%")
  }), 
  
  ##########################
  ## RENDER PLOTS / TABLE ##
  ##########################
  
  # Check to see if a FASTA file is loaded, since I keep forgetting to do this
  output$PTnoFAwarn <- renderText({
    if (is.null(msPath()) | is.null(idPath()) | is.null(fastaPath())) {
      HTML('<p><span style="font-size: 14pt; color: #46a716;">
           Remember to upload an MS, ID, & FASTA file to see Protein Coverage Graphs.</span></p')}
  }),

  # Creates table with identified protein data 
  output$PTTable <- DT::renderDataTable({
    
    # If no protein table, return NULL 
    if (is.null(GET_protein_table())) {return(NULL)}
    
    datatable(GET_protein_table(), rownames = F, filter = 'top', options = list(pageLength = 4),
              selection = list(mode = 'single', selected = 1))
  }),
  
  # Reveal protein sequence
  output$LSeq <- renderPlot({
    
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    # Make plot
    p <- coverage_lit_seq_plot(GET_peptide_coverage())
    
    plots$currLSEQ <- p
    
    p
    
  }),
  
  # Make chart of protein tree matches
  output$PTMatch <- renderPlotly({
    
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    Score <- NULL
    
    if (input$PTPlotScore != "None") {
      Score <- input$PTPlotScore
    }
    
    # Make the plotly
    p <- coverage_plot(
      PeptideCoverage = GET_peptide_coverage(),
      Interactive = T,
      ColorByScore = Score
    )
    
    plots$currMATCH <- p
    
    plotly::toWebGL(p)
    
  }),
  
  # Makes barchart of protein tree data 
  output$PTBar <- renderPlotly({
  
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    p <- coverage_bar_plot(PeptideCoverage = GET_peptide_coverage(), Interactive = T)
    
    plots$currPTBAR <- p
    
    p
    
  }),
  
  # Print Q Value Warning
  output$QValWarn <- renderText({
    
    scan <- GET_scan_metadata()
    if (is.null(scan)) {return(NULL)}
    
    # If all the Q-Values are NA give this warning
    if ((FALSE %in% unique(is.na(scan$`Q Value`))) == F) {
      HTML('<strong><span style="color: rgb(184, 49, 47); background-color: rgb(247, 218, 100);">
           No Q-Values provided. Filtering disabled. </span></strong>')
    } else {return(NULL)}
    
  })

)