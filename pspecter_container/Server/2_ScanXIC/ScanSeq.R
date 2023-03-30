## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Generates Scan & Seq plots not isolated in the other scripts

list(
  
  ###############
  ## FUNCTIONS ##
  ###############
  
  # Create a trunc2 function to truncate at two decimal places
  trunc2 <- function(val) {trunc(val * 100) / 100},
  
  ###############
  ## OBSERVERS ##
  ###############
  
  # Add an ion if the button is clicked
  observeEvent(input$AddIonSubmit, {
    
    # Send a warning if the mass spectra difference is more than 1000 and do nothing
    if (abs(input$AddIonWeight) > 1000) {
      sendSweetAlert(session, "Add Ion Warning", 
        "PSpecteR does not currently accept modified weights of > 1000 or < 1000.", "warning")
      return(NULL)
    }
    
    # Update data table created by iterating through all ions and making a table
    revals$AddedIons <- rbind(revals$AddedIons, 
      data.table("Ion" = input$AddIonSelect, "Annotation" = input$AddIonAnnotation, 
        "AMU Change" = input$AddIonWeight)
    )
    
  }),
  
  # Remove an ion if the button is clicked
  observeEvent(input$RemoveIonSubmit, {
    
    # Get selected row
    row <- input$AddIonTable_row_last_clicked
    
    # If null, send a sweet alert 
    if (is.null(row)) {
      sendSweetAlert(session, "Modified Ion Removal Error", 
        "Please select a row in the table to remove an ion. If there is no table, there are no modified ions.",
      "error")
      return(NULL)
    }
    
    # Remove ion
    revals$AddedIons <- revals$AddedIons[-row,]
    
  }),
  
  # Restore sequence for 2. Scan & Seq when this button is clicked
  observeEvent(input$ssRSeq, {
    
    # Re-initiate sequence
    seq <- ""
    
    # Change sequence to actual seq if it exists
    scan <- getScan()
    if (is.null(scan) == F) {seq <- scan[getScanClick(), "Sequence"]}
    
    # Update text input
    updateTextInput(session, "ssNewSeq", value = seq)
    
  }), 
  
  # Change X (M/Z) Spectra Values
  observeEvent(input$ssSpecNewRange, {
    updateSliderInput(session, "ssSpecX", value = c(input$ssSpecMin, 
     input$ssSpecMax))
  }),
  
  # Change Y (intensity) Spectra Values
  observeEvent(input$ssSpecNewINT, {
    updateSliderInput(session, "ssSpecY", value = c(input$ssIntMin,
     input$ssIntMax))
  }),
  
  ######################
  ## OBSERVER POP UPS ##
  ######################
  
  # Trigger "Add Ions" modal  
  observeEvent(input$ManageIons, {
    showModal(modalDialog(fluidPage(
      column(4, selectInput("AddIonSelect", "Select Ion to Modify", c("a", "b", "c", "x", "y", "z"), "z")),
      column(4, selectInput("AddIonAnnotation", "Choose Annotation Symbol", 
        c("'", "''", "*", "**", "(-1)", "(+1)", "(-2)", "(+2)"), "'")),
      column(4, numericInput("AddIonWeight", "Add Weight Change in AMU", 1.007825)),
      column(12, hr()),
      column(12, DT::dataTableOutput("AddIonTable"))),
      title = HTML('<p style="text-align: center;">Manage Modified Ions</p>'),
      footer = list(actionButton("AddIonSubmit", "Add Ion", icon = icon("plus")), 
        actionButton("RemoveIonSubmit", "Remove Ion", icon = icon("minus")),            
        modalButton("Exit")),
      size = "m", easyClose = T
    ))
  }),
  
  # Make spectra full screen
  observeEvent(input$ssLargeSpec, {
    showModal(modalDialog(fluidPage(
      plotlyOutput("bigSpec", width = "100%", height = "700px") %>% 
        withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
    title = HTML('<p style="text-align: center;">View Spectra Full Screen</p>'),
    footer = modalButton("Exit"), size = "l", easyClose = T))
  }),
  
  ####################
  ## RENDER WIDGETS ##
  ####################
  
  # Render the Annotate Spectra Widget
  output$ssLetterSWITCH <- renderUI({
    SL <- materialSwitch("ssLetter", label = HTML("<strong>Annotate Spectra</strong>"), value = T, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(SL, Desc[Desc$Name == "ssLetter", "Title"], Desc[Desc$Name == "ssLetter", "Description"],
             options = list(selector = '.material-switch'), placement = 'right')
    } else {SL}
  }),
  
  # Render the Bar: Count Per Fragment Widget
  output$ssBarCTFragSWITCH <- renderUI({
    BCTF <- materialSwitch("ssBarCTFrag", HTML("<strong>Barplot: Count Ions per Fragment</strong>"), value = T, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(BCTF, Desc[Desc$Name == "ssBarCTFrag", "Title"], Desc[Desc$Name == "ssBarCTFrag", "Description"],
             options = list(selector = '.material-switch'), placement = 'right')
    } else {BCTF}
  }),
  
  # Render the Seq: Annotate PTMs Widget
  output$ssAnoPTMSWITCH <- renderUI({
    SAP <- materialSwitch("ssAnoPTM", HTML("<strong>Annotate Modifications in Plots</strong>"), value = T, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(SAP, Desc[Desc$Name == "ssAnoPTM", "Title"], Desc[Desc$Name == "ssAnoPTM", "Description"],
             options = list(selector = '.material-switch'), placement = 'right')
    } else {SAP}
  }),
  
  # Render the "Remove Isotopes: Spectra?" Slider
  output$ssISOspectraSWITCH <- renderUI({
    SIS <- materialSwitch("ssISOspectra", HTML('<strong>Include Isotopes</strong>'), value = T, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(SIS, Desc[Desc$Name == "ssISOspectra", "Title"], Desc[Desc$Name == "ssISOspectra", "Description"],
             options = list(selector = '.material-switch'), placement = 'right')
    } else {SIS}
  }),
  
  # Determine which ions to check based on activation method
  output$SelectedIons <- renderUI({
    
    # Set variable to contain the checkbox information 
    selectIonUI <- selectInput("ionGroups", "Ion Type", c("a", "b", "c", "x", "y", "z",
      paste0(revals$AddedIons$Ion, revals$AddedIons$Annotation)), selected = GET_activation_method_ions(), multiple = T)
    
    # Show pop up box if pop-ups are enabled
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(selectIonUI, Desc[Desc$Name == "ionGroups", "Title"], Desc[Desc$Name == "ionGroups", "Description"])
    } else {selectIonUI}
  }),
  
  # Allow user to test a different sequence
  output$ssRenderNS <- renderUI({
    
    # Initiate Sequence
    seq <- ""
    
    # Change sequence to actual seq if it exists
    scan <- GET_scan_metadata()
    if (is.null(scan) == F) {seq <- scan[GET_scan_click(), "Sequence"]}
    
    SSseqUI <- textInput("ssNewSeq", "Test Different Sequence", value = seq, placeholder = "Enter Amino Acid Sequence")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(SSseqUI, Desc[Desc$Name == "ssNewSeq", "Title"], Desc[Desc$Name == "ssNewSeq", "Description"])
    } else {SSseqUI}
    
  }),
  
  # Determine X range for spectra
  output$ssSpecXRange <- renderUI({
    peak <- GET_peak_data()
    if (is.null(peak)) {return(NULL)}
    highest <- round(max(peak$mz), 3)
    sliderInput("ssSpecX", "M/Z Range", 0, highest, c(0, highest), 0.01, width = "150%")
  }),
  
  # Determine Y range for spectra
  output$ssSpecYRange <- renderUI({
    peak <- GET_peak_data()
    if (is.null(peak)) {return(NULL)}
    highest <- round(max(peak$intensity))
    sliderInput("ssSpecY", "Intensity Range", 0, highest, c(0, highest), 1, width = "150%")
  }),
  
  ##############################
  ## RENDER TABLES, AND PLOTS ##
  ##############################
  
  # Generates the scan view (table)
  output$ssScan <- DT::renderDataTable({

   # Scan data is required  
   req(GET_scan_metadata())

   # Pull scan data and convert class
   scan <- GET_scan_metadata()
   class(scan) <- c("data.table", "data.frame")
   
   # Make datatable
   datatable(scan %>% dplyr::select(colnames(scan)[as.numeric(input$ssScanMetadataCol)]),
             selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
             options = list(pageLength = 5, scrollX = T))
  }), 
  
  observeEvent(input$debug, {browser()}),
  
  # This will generate the spectrum view
  output$ssSpectrum <- renderPlotly({

    # Get the peak data, scan number, fragments, and rows from "Select Ions" table
    if (is.null(GET_peak_data()) || nrow(GET_peak_data()) == 0) {return(NULL)}
    
    # If there are more than 50,000 peaks in the spectra, let user know. 
    if (nrow(GET_peak_data()) > 5e4) {
      sendSweetAlert(session, "Plotting more than 50,000 peaks", 
        paste("This may take a while to generate and the resulting spectra",
          "will be very busy. Consider setting filters in 1. Filter Settings."))
    }
    
    # Make plot
    thePlot <- annotated_spectrum_plot(
      PeakData = GET_peak_data(),
      MatchedPeaks = GET_matched_peaks(),
      IncludeIsotopes = ifelse(is.null(input$ssISOspectra), TRUE, input$ssISOspectra),
      IncludeLabels = ifelse(is.null(input$ssLetter), TRUE, input$ssLetter),
      LabelSize = ifelse(is.null(input$ssAnnoSize), 8, abs(input$ssAnnoSize)),
      LabelDistance = ifelse(is.null(input$ssLabDist), 0.5, abs(input$ssLabDist)),
      Interactive = TRUE
    )
    
    # Save plot
    plots$currSPEC <- thePlot
    
    return(thePlot)
      
  }),
  
  # Generates a tables of fragment data such as its charge, name, modifications, etc.
  output$ssSeqTable <- DT::renderDataTable({
    
    # Get Matched Peaks 
    if (is.null(GET_matched_peaks())) {return(NULL)}
    
    browser()
    
    # Round MZ so that it fits in the table, as well as correlation score
    frag$mzExp <- round(frag$mzExp, 1)
    frag$corrScore <- round(frag$corrScore, 2)
    
    # Frag peptide
    frag$Peptide <- substr(rep(revals$testSeq, nrow(frag)), frag$npos, frag$npos)
    
    # Rename selected columns for table
    frag <- frag[, c("type", "pos", "Peptide", "z", "isoPeak", "mz", "corrScore"), drop = F]
    colnames(frag) <- c("Type", "Pos", "Pep", "Z", "Iso", "MZ", "CorrScore")
    
    # Display data table 
    datatable(frag[, c(as.integer(input$ssFragColumns)), drop = FALSE], rownames = F, 
              filter = "none", selection = list(mode = "multiple"),
              options = list(pageLength = 3, sDom = '<"top">lt<"bottom">p', `scrollX` = T))
  }),
  
  # Makes a bar chart of the frequency of fragments
  output$ssSeqBar <- renderPlotly({
    if (is.null(GET_matched_peaks())) {return(NULL)}
    SEQBAR <- ion_bar_plot(MatchedPeaks = GET_matched_peaks())
    plots$currSSBAR <- SEQBAR
    SEQBAR
  }), 
  
  # Creates the "flag" figure with the best matching ion per sequence
  output$ssSeqFlag <- renderPlot({
    
   # Get matched peaks 
   if (is.null(GET_matched_peaks())) {return(NULL)}
    
    # Make plot
    p <- sequence_plot(
      MatchedPeaks = GET_matched_peaks(),
      IncludeIsotopes = ifelse(is.null(input$ssISOspectra), TRUE, input$ssISOspectra),
      RemoveChargeAnnotation = TRUE,
      WrapLength = 15
    )
    
    plots$currFLAG <- p
      
    p
  }),
  
  # Render the get all fragment datatable
  output$ssAllFrag <- DT::renderDataTable({
    
    # Get all fragment data
    if (is.null(GET_matched_peaks())) {return(NULL)}
    
    # Count anntations
    CA <- count_ion_annotations(
      MatchedPeaks = GET_matched_peaks(),
      IncludeIsotopes = ifelse(is.null(input$ssISOspectra), TRUE, input$ssISOspectra)
    )
    
    # Render datatable
    datatable(CA, selection = list(mode = 'single'), rownames = F, filter = 'top', 
              options = list(pageLength = 3), escape = F)
  }),
  
  # Output full screen spectra
  output$bigSpec <- renderPlotly({
    if (is.null(getSSPeak())) {return(NULL)}
    if (nrow(getSSPeak()) > 50000) {
      sendSweetAlert(session, "Full Screen Warning", 
                     "Full screen disabled for spectra with more than 50,000 peaks.",
                     type = "warning")
      return(NULL)
    } else {
      p <- plots$currSPEC
      plotly::toWebGL(p)
    }
  }),
  
  # Output New Seq Warnings
  output$ssNSWarn <- renderText({
    Nseq <- input$ssNewSeq
    if (is.null(Nseq)) {paste("No empty strings or whitespace")} else
    if (grepl("\\[", Nseq) | grepl("\\]", Nseq)) {paste("Brackets not supported.", 
        "Use Vis PTM to test modifications.")} else
    if (grepl("[[:space:]]", Nseq)) {paste("No empty strings or whitespace")} else
    if (grepl("[^aA-zZ]", Nseq)) {paste("Letters only")} else
    if (nchar(Nseq) < 2) {paste("Sequence must have > 1 amino acid")} else
    if (grepl("[BbJjOoUuXxZz]", Nseq)) {paste("B, J, O, U, X, Z are incorrect options")} else
      {paste("Acceptable sequence")}
  }), 
  
  
  # Output add ion render datatable
  output$AddIonTable <- DT::renderDataTable({
    
    # Return NULL if no added ions
    if (is.null(revals$AddedIons)) {return(NULL)}
  
    # Render datatable
    datatable(revals$AddedIons, selection = list(mode = 'single'), rownames = F, filter = 'none', 
              options = list(pageLength = 10), escape = F)
    
    
  }),
  
  # Add heat error map 
  output$ErrorMap <- renderPlotly({
    
    # Return NULL if no matched peaks 
    if (is.null(GET_matched_peaks())) {return(NULL)}
      
    # Make plot   
    HM <- error_heatmap_plot(
      MatchedPeaks = GET_matched_peaks(),
      IncludeIsotopes = ifelse(is.null(input$ssISOspectra), TRUE, input$ssISOspectra)
    )
      
    plots$currHM <- HM
    
    return(HM) 
    
  }),
  
  # Add coverage 
  output$coverage <- renderText({
    
    # These are the variables needed to render at the bottom of the sidebar
    coverage <- NULL
    numPeaks <- NULL
    
    # Get Scan Data
    scan <- GET_scan_metadata()
    if (is.null(scan) == F) {
      clicked <- GET_scan_click()
      peak <- GET_peak_data()
      peak <- peak[peak$intensity > 0,]
      numPeaks <- nrow(peak)
      
      # Get Fragment Data to Calculate Coverage 
      if (is.null(GET_matched_peaks()) == F) { 
        
        # Get the sequence length, removing one as the first amino acid is not countedin the coverage calculation
        seqLen <- nchar(scan[clicked, "Sequence"]) - 1
        
        # If a new sequence has been inputted, test that new sequence
        if (is.na(seqLen)) {seqLen <- nchar(revals$testSeq) - 1}
        
        # Get all nposition data of identified fragments, remove npos 1, and take the length
        covLen <- length(unique(GET_matched_peaks()$npos[GET_matched_peaks()$npos != 1]))
        
        coverage <- paste(round((covLen / seqLen * 100), 1), "%", sep = "")
        
      }} 
    
    # Paste coverage output
    paste("Number of Peaks: ", numPeaks, ", Coverage: ", coverage, sep = "")
    
  })
  
)