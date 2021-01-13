## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2021_01_07

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
    SL <- materialSwitch("ssLetter", label = HTML("<strong>Annotate Spectra</strong>"), value = F, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(SL, Desc[Desc$Name == "ssLetter", "Title"], Desc[Desc$Name == "ssLetter", "Description"],
             options = list(selector = '.material-switch'), placement = 'right')
    } else {SL}
  }),
  
  # Render the Real Time Editing Widget
  output$RealTimeSWITCH <- renderUI({
    RTE <- materialSwitch("RealTime", HTML("<strong>Freeze Interactive Spectra Editing</strong>"), value = T, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(RTE, Desc[Desc$Name == "RealTime", "Title"], Desc[Desc$Name == "RealTime", "Description"],
             options = list(selector = '.material-switch'), placement = 'right')
    } else {RTE}
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
  
  # Render the "Remove Isotopes: Graphics?" Slider
  output$ssISOgraphsSWITCH <- renderUI({
    SIG <- materialSwitch("ssISOgraphs", HTML('<strong>Remove Isotopes in Plots</strong>'), value = T, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(SIG, Desc[Desc$Name == "ssISOgraphs", "Title"], Desc[Desc$Name == "ssISOgraphs", "Description"],
             options = list(selector = '.material-switch'), placement = 'right')
    } else {SIG}
  }),
  
  # Render the "Remove Isotopes: Spectra?" Slider
  output$ssISOspectraSWITCH <- renderUI({
    SIS <- materialSwitch("ssISOspectra", HTML('<strong>Remove Isotopes from Spectra</strong>'), value = F, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(SIS, Desc[Desc$Name == "ssISOspectra", "Title"], Desc[Desc$Name == "ssISOspectra", "Description"],
             options = list(selector = '.material-switch'), placement = 'right')
    } else {SIS}
  }),
  
  # Determine which ions to check based on activation method
  output$SelectedIons <- renderUI({
    
    # Set variable to contain the checkbox information 
    selectIonUI <- selectInput("ionGroups", "Fragment Type", c("a" = "a", "b" = "b", 
      "c" = "c", "x" = "x", "y" = "y", "z"= "z"), selected = getActMetIon(), multiple = T)
    
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
    scan <- getScan()
    if (is.null(scan) == F) {seq <- scan[getScanClick(), "Sequence"]}
    
    SSseqUI <- textInput("ssNewSeq", "Test Different Sequence", value = seq, placeholder = "Enter Amino Acid Sequence")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(SSseqUI, Desc[Desc$Name == "ssNewSeq", "Title"], Desc[Desc$Name == "ssNewSeq", "Description"])
    } else {SSseqUI}
    
  }),
  
  # Determine X range for spectra
  output$ssSpecXRange <- renderUI({
    peak <- getSSPeak()
    if (is.null(peak)) {return(NULL)}
    highest <- round(max(peak$mz), 3)
    sliderInput("ssSpecX", "M/Z Range", 0, highest, c(0, highest), 0.01, width = "150%")
  }),
  
  # Determine Y range for spectra
  output$ssSpecYRange <- renderUI({
    peak <- getSSPeak()
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
   req(getScan())
   scan <- getScan()
   
   # Add modification annotations if they exist
   if (is.null(getScanMod()) == F) {scan <- getScanMod()}
   
   datatable(scan[, c(as.integer(input$ssCheckboxes)), drop = FALSE], 
             selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
             options = list(pageLength = 5, scrollX = T))
  }), 
  
  # This will generate the spectrum view
  output$ssSpectrum <- renderPlotly({
    
    # Have switch enable Real-Time Editing
    if (is.null(input$RealTime) == F && input$RealTime == F) {plots$currSPEC} else {
        
    # Get the peak data, scan number, fragments, and rows from "Select Ions" table
    if (is.null(getSSPeak()) || nrow(getSSPeak()) == 0) {return(NULL)}
    peak <- getSSPeak()
    scanNum <- getScan()[getScanClick(), "Scan.Num"]
    frag <- getFrag()
    rows <- input$ssSeqTable_rows_selected
    if (is.null(rows) == F) {
      frag <- frag[rows,]
      peak <- peak[trunc2(peak$mz) %in% trunc2(frag$mzExp),]
    }
    
    # If there are more than 50,000 peaks in the spectra, let user know. 
    if (nrow(peak) > 5e4) {
      sendSweetAlert(session, "Plotting more than 50,000 peaks", 
        paste("This may take a while to generate and the resulting spectra",
          "will be very busy. Consider setting filters in 1. Filter Settings."))
    }
   
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
          
          # Remove 0 from labelling
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
       }
    }
    
   # Declare X and Y range
   xrange <- c(0, max(spectra$mzExp) + 1)
   yrange <- c(0, max(spectra$intensity) + 1)
  
   # Adjust window as necessary (specific to when ions are selected)
   if (is.null(rows) == F) {
      xrange <- c(min(spectra$mzExp) - 1, max(spectra$mzExp) + 1)
      yrange <- c(0, max(spectra$intensityExp))
   }
      
    p <- p %>% layout(xaxis = list(title = '<i>m/z</i> (Mass to Charge)', range = xrange),
                 yaxis = list(title = "Intensity", range = yrange), 
                 title = paste("Scan:", scanNum), legend = list(orientation = "h"))
  
    plots$currSPEC <- p
  
    plotly::toWebGL(p)
    }
    
  }),
  
  # Generates a tables of fragment data such as its charge, name, modifications, etc.
  output$ssSeqTable <- DT::renderDataTable({
    
    # Get fragment
    frag <- getFrag()
    if (is.null(frag)) {return(NULL)}
    
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
    
    # Get fragment
    frag <- getFrag()
    if (is.null(frag)) {return(NULL)}
    
    # These variables are used to make the title 
    fragment <- "of all fragments"
    isotopes <- "including"
    
    # Remove isotopes from frag if removing isotopes feature is enabled
    if (is.null(input$ssISOgraphs) == F && input$ssISOgraphs == T) {
      frag <- frag[frag$isoPeak == "M+0",]; isotopes <- "excluding"
    }
    
    # Reduce count to a per fragment basis (remove counts by charge and isotope)
    if (is.null(input$ssBarCTFrag) == F && input$ssBarCTFrag == T) {
      frag <- frag[match(unique(frag$ion), frag$ion),]; fragment <- "per fragment"
    }
    
    # Get original sequence dataframe
    oriSDF <- getOriSDF()
    if (is.null(oriSDF)) {return(NULL)}
    
    # Merge XY dataframe and fragment dataframe, removing modification annotations
    SDF <- merge(oriSDF, frag, by = c("npos", "npos"))
    SDF <- SDF[SDF$Peptide != "*",]
    
    # Subset SDF based on ion groups
    SDF <- subset(SDF, SDF$type %in% input$ionGroups)
    
    names <- c("a", "b", "c", "x", "y", "z", "all")
    values <- c(nrow(subset(SDF, SDF$type == "a")), nrow(subset(SDF, SDF$type == "b")),
                nrow(subset(SDF, SDF$type == "c")), nrow(subset(SDF, SDF$type == "x")),
                nrow(subset(SDF, SDF$type == "y")), nrow(subset(SDF, SDF$type == "z")),
                nrow(SDF))
    barVals <- data.frame(names, values)
    barVals$names <- factor(barVals$names, levels = barVals[["names"]])
    
    # Set the title and a y-range so the plot fits nicely in the window in PSpecteR
    title <- paste("Counts ", fragment, ", ", isotopes, " isotopes", sep = "")
    max <- barVals[barVals$names == "all", "values"]
    max <- max + (max * 0.2)
     
    # Make bar plot
    SEQBAR <- plot_ly(x = barVals$names, y = barVals$values, type = "bar", text = barVals$values, 
            textposition = "outside", 
            marker = list(color = c("forestgreen","steelblue", "darkviolet", "rgb(172, 122, 122)",
            "red", "darkorange", "lightgray")), hoverinfo = "text", hovertext =
            paste(barVals$names, " fragments: ", barVals$values, sep = "")) %>%
      layout(yaxis = list(title = "Frequency", range = c(0, max)), 
             title = list(text = title, font = list(size = 14)))
    
    plots$currSSBAR <- SEQBAR
    
    SEQBAR
  }), 
  
  # Creates the "flag" figure with the best matching ion per sequence
  output$ssSeqFlag <- renderPlotly({
    
    # Get flag dataframe
    flagData <- getFlagDF()
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
    
    plots$currFLAG <- p
      
    p
  }),
  
  # Render the get all fragment datatable
  output$ssAllFrag <- DT::renderDataTable({
    
    # Get all fragment data
    AllFrag <- getAllFrag()
    
    # Render datatable
    datatable(AllFrag, selection = list(mode = 'single'), rownames = F, filter = 'top', 
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
  })
  
)