## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_08_03

# DESCRIPTION: Contains the table and spectra for Vis PTM

list(
  
  # Source Get Frag
  source(file.path("Server", "Get", "Get_Frag.R"), local = T)$value,
  
  ##############
  ## FUNCTION ##
  ##############
  
  # Select Modification: Function to build to PTM
  buildPTM <- function(posMod) {
    posMod <- unlist(posMod)
    seq <- getNewVSeq()
    pos <- as.numeric(unlist(lapply(posMod, function(el) {unlist(strsplit(el, " "))[1]})))
    aa <- unlist(lapply(pos, function(el) {substr(seq, el, el)}))
    mod <- unlist(lapply(posMod, function(el) {
      modName <- unlist(strsplit(el, " "))[2]
      if (modName == "Added_Mass") {
        mass <- unlist(strsplit(el, " "))[3]
        modName <- paste("Added_Mass=", mass, sep = "")
      }
      return(modName)
    }))
    
    PTM <- do.call(paste, lapply(1:length(pos), function(el) {
      paste(aa[el], pos[el], ": ", mod[el], ",", sep = "")}))
    return(substr(PTM, 1, nchar(PTM) - 1))
  },
  
  ###############
  ## OBSERVERS ##
  ###############
  
  # Restore VP Seq when this button is clicked
  observeEvent(input$VPrseq, {
    
    # Re-initiate sequence
    seq <- ""
    
    # Change sequence to actual seq if it exists
    scan <- getScan()
    if (is.null(scan) == F) {seq <- scan[getScanClick(), "Sequence"]}
    
    # Update text input
    updateTextInput(session, "VPSequence", value = seq)
    
  }),
  
  # If VP clear is clicked, remove all selections
  observeEvent(input$VPclear, {
    sendSweetAlert(session, "All Selected PTMs Cleared!", type = "success")
    updatePickerInput(session, inputId = "VPpick", selected = "")
  }),
  
  # If VP commons is clicked, selected all common options
  observeEvent(input$VPcommon, {
    sendSweetAlert(session, "Selected Most Common PTMs", type = "success")
    updatePickerInput(session, inputId = "VPpick", selected = c("Carbamidomethyl; 57.021464",
      "Label:2H(3)+Oxidation; 19.013745", "Deamidation; 0.984016", "Methyl; 14.01565", 
      "Acetyl; 42.010565", "Phospho; 79.966331", "Pyro-glu; -17.026549", 
      "Pyro_glu; -18.010565"))
  }),
  
  # Delete selected rows from VPTable
  observeEvent(input$VPdelrow, {
    clicked <- input$VPTable_rows_selected
    if (is.null(clicked)) {return(NULL)} else {
      revals$PTMs <- revals$PTMs[-clicked]}
  }),
  
  # Keep only selected rows from VPTable
  observeEvent(input$VPkeeprow, {
    clicked <- input$VPTable_rows_selected
    if (is.null(clicked)) {return(NULL)} else {
      revals$PTMs <- revals$PTMs[clicked]} 
  }),
  
  # Dynamic search calculations
  observeEvent(input$VPcalc, {
    
    if (is.null(getScan())) {
      sendSweetAlert(session, "VisPTM Error!", "Upload an MS File First.",
                     "error")
      return(NULL)
    }
    
    if (is.null(getNewVSeq())) {
      sendSweetAlert(session, "VisPTM Error!", "Please input a valid sequence.",
                     "error")
      return(NULL)
    }
    
    if (length(revals$PTMs) < 1) {
      sendSweetAlert(session, "VisPTM Error!", "No modifications have been specified.",
                     "error")
      return(NULL)
    }
    
    withProgress({ 
  
      # Set incremental progress bar
      incProgress(amount = 0.1, "Calculating Theoretical Spectrum")
        
      # Get PTM values
      PTM <- revals$PTMs
      
      # Shorten glossary to selected PTMs
      Glossary <- getGlossary()      
      Selected <- lapply(strsplit(input$VPpick, ";"), function(el) {el[1]}) %>% unlist()
      Glossary <- Glossary[Glossary$Interim.Name %in% Selected,]
      
      # Get the peak data and scan number
      peak <- getSSPeak()
      if (is.null(peak)) {return(NULL)}
      seq <- getNewVSeq()
      scan <- getScan()
      scanNum <- scan[getScanClick(), "Scan.Num"]
      
      # Get theoretical fragment data
      frag <- getCalcFrag(seq, input$ ionGroups, 1:scan[scan$Scan.Num == scanNum, "Pre.Charge"])
      
      incProgress(amount = 0.5, "Acquiring Modification Data")
      
      # Start for loop to get all possibilities
      VisPTM <- lapply(1:length(PTM), function(row) {
        
        incProgress(amount = 0, paste("Calculating Metrics: ",
                                     round(row/length(PTM), 2) * 100, "% Row: ", row, sep = ""))

        
        if (PTM[row] != "No Modifications") {
        
          # 1. Build modifications data frame
          mod <- buildModDF(scanNum, seq, PTM[row], Glossary)
          
          # 2. Add Mod Mass
          modMass <- addModMass(mod, frag, scanNum, seq, Glossary)
          
          # 3. Apply Mod
          frag <- applyMod(modMass, frag)
          
          # 4. Get Molecular Formula
          frag <- getMolecularFormula(frag)
          
          # 5. Get Calculated Isotope Values
          frag <- getCalcIso(frag, 0.1)
          
          # 6. Get Annotated Peaks
          frag <- getAnnotatedPeaks(peak, frag, 10)
          frag$error <- (frag$mzExp - frag$mz) / frag$mz * 1e6
          
          # If no lines of data, manually input NA for mean
          if (nrow(frag) == 0) { 
            return(cbind(Name = PTM[row], Average.PPM.Error = NA, Number.of.Ions = 0, Coverage = 0))
          } else {
            seqLen <- nchar(seq) - 1
            covLen <- length(unique(frag$npos[frag$npos != 1]))
            return(cbind(Name = PTM[row], Average.PPM.Error = mean(abs(frag$error)), Number.of.Ions = nrow(frag),
                         Coverage = round((covLen / seqLen * 100), 1)))
          }
        } else {
          
          # If no modifications, then get the data from page 2
          frag <- getNoModFrag()
          if (is.null(frag)) {
            return(cbind(Name = "No Modifications", Average.PPM.Error = NA, Number.of.Ions = 0,
                         Coverage = 0))
          } else {
            seqLen <- nchar(seq) - 1
            covLen <- length(unique(frag$npos[frag$npos != 1]))
            return(cbind(Name = "No Modifications", Average.PPM.Error = mean(abs(frag$error)),
                         Number.of.Ions = nrow(frag), 
                         Coverage = round((covLen / seqLen * 100), 1)))}}})
    
      VisPTM <- do.call(rbind, VisPTM)
    })
    
    removeModal()
    VisPTM[,1] <- as.character(VisPTM[,1])
    VisPTM[,2] <- as.numeric(VisPTM[,2])
    VisPTM[,3] <- as.numeric(VisPTM[,3])
    revals$PTMdf <- rbind(revals$PTMdf, VisPTM)
    
  }),
  
  # Modal pop up box for specific annotations
  observeEvent(input$VPspecific, {
    
    # Get Sequence Data
    seq <- getNewVSeq()
    
    # Send alert if sequence data is non-existant
    if (is.null(seq) || is.na(seq)) {
      sendSweetAlert(session, "No sequence error!", "Please type in a valid sequence.",
                     type = "error")
      return(NULL)
    }
    
    # Send alert if no MS data exists
    if (is.null(getScan())) {
      sendSweetAlert(session, "No MS Data error!", "Please upload MS data", type = "error")
      return(NULL)
    }
    
    scan <- getScan()
    
    # Send alert if MS scan is 1
    if (scan[getScanClick(), "MS.Level"] == 1) {
      sendSweetAlert(session, "No MS1 Scans for VisPTM", "Select an MS2 scan.", type = "error")
      return(NULL)
    }
   
   # Open the select modifications pop up box
   showModal(modalDialog(fluidPage(
      plotlyOutput("ssAnnoSeq", width = "100%", height = "400px")
      %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c")),
      hr(), htmlOutput("ssAnnoText"), hr(),
      column(6, uiOutput("ssAnnoPicker")), 
      column(6, numericInput("ssAnnoMassAdded", "...or Choose a Mass", 0))),      
    title = HTML('<p style="text-align: center;">Specific Modification Search: Click an amino acid to begin.</p>'),
    footer = list(actionButton("ssAnnoAdd", "Add Modification"),
                  actionButton("ssAnnoMass", "Add Mass"),
                  actionButton("ssAnnoCalc", "Calculate"),
                  actionButton("ssAnnoExit", "Exit")), size = "m", easyClose = F))
  }),
  
  # Select Modifications Modal: Add Modification
  observeEvent(input$ssAnnoAdd, {
    
    # Send warning if no modifications have been selected
    if (is.null(input$VPpickSelect)) {
      sendSweetAlert(session, "Select Modifications Error!", 
                     "Choose amodification.", type = "error")
      return(NULL)}
    
    # Get the ModSeq Dataframe
    ModSeq <- getModSeq()
    if (is.na(ModSeq) || is.null(ModSeq)) {return(NULL)}
    
    # Get the plotly click information 
    selected <- event_data("plotly_click", "ssAnnoSeq")
    if (length(selected) == 0) {
      sendSweetAlert(session, "Selected Modifications Error!", 
                     "Select an amino acid by clicking.", type = "error")
      return(NULL)}
    
    # Get the amino acid position 
    aaPos <- ModSeq[ModSeq$x == selected$x & ModSeq$y == selected$y,]
    if (nrow(aaPos) == 0) {return(NULL)}
    
    # Get Glossary Data and Get Modification's entry
    Glossary <- getGlossary()
    mod <- unlist(strsplit(input$VPpickSelect, "; "))[1]
    mod <- Glossary[Glossary$Interim.Name == mod,]
    if (aaPos$aa %in% unlist(strsplit(as.character(mod$Modified.Sites), " ")) == F) {
      message <- paste("The modification ", mod$Interim.Name, " is only applied to ",
                       mod$Modified.Sites, ". You selected: ", aaPos$aa, ".", sep = "")
      sendSweetAlert(session, "Modification Application Error!", message, type = "error")
    } else {
      posMod <- paste(aaPos$pos, mod$Interim.Name, mod$Monoisotopic.Mass)
      search$posMod <- append(search$posMod, posMod)
    }
    
  }),
  
  # Select Modification: Add Mass
  observeEvent(input$ssAnnoMass, {
    
    # Get the ModSeq Dataframe
    ModSeq <- getModSeq()
    if (is.na(ModSeq) || is.null(ModSeq)) {return(NULL)}
    
    # Get the plotly click information 
    selected <- event_data("plotly_click", "ssAnnoSeq")
    if (length(selected) == 0) {
      sendSweetAlert(session, "Selected Modifications Error!", 
                     "Select an amino acid by clicking.", type = "error")
      return(NULL)}
    
    # Get the amino acid position 
    aaPos <- ModSeq[ModSeq$x == selected$x & ModSeq$y == selected$y,]
    if (nrow(aaPos) == 0) {return(NULL)}
    
    posMod <- paste(aaPos$pos, "Added_Mass", input$ssAnnoMassAdded)
    search$posMod <- append(search$posMod, posMod)
    
  }), 
  
   # Select Modification: Calculate 
  observeEvent(input$ssAnnoCalc, {
    
    # Get scan number, sequence, and the glossary
    scan <- getScan()
    scanNum <- scan[getScanClick(), "Scan.Num"]
    seq <- getNewVSeq()
    Glossary <- getGlossary()
    peak <- getSSPeak()
    
    # Get the theoretical spectra
    frag <- getCalcFrag(seq, input$ ionGroups, 1:scan[scan$Scan.Num == scanNum, "Pre.Charge"])
    
    # Remove modal box
    removeModal()
    
    if (length(search$posMod) > 0) {
      
      # 1. Build modifications data frame
      mod <- buildModDF(scanNum, seq, buildPTM(search$posMod), Glossary)
      
      # Follow getFrag algorithmic steps
      modMass <- addModMass(mod, frag, scanNum, seq, Glossary)
      frag <- applyMod(modMass, frag)
      frag <- getMolecularFormula(frag)
      frag <- getCalcIso(frag, 0.1)
      frag <- getAnnotatedPeaks(peak, frag, 10)
      frag$error <- (frag$mzExp - frag$mz) / frag$mz * 1e6
      
      # Get sequence and coverage length
      seqLen <- nchar(seq) - 1
      covLen <- length(unique(frag$npos[frag$npos != 1]))
      
    } else {
      sendSweetAlert(session, "Vis PTM Error", "No modifications selected", "error")
      return(NULL)
    }
    
    VisPTM <- data.frame(Name = buildPTM(search$posMod), 
                         Average.PPM.Error = mean(abs(frag$error)),
                         Number.of.Ions = nrow(frag), 
                         Coverage = round((covLen / seqLen * 100), 1),
                         stringsAsFactors = F)
    
    # Change data types 
    PTMdf <- data.frame(revals$PTMdf, stringsAsFactors = F)
    PTMdf$Average.PPM.Error <- as.numeric(PTMdf$Average.PPM.Error)
    PTMdf$Number.of.Ions <- as.numeric(PTMdf$Number.of.Ions)
    PTMdf$Coverage <- as.numeric(PTMdf$Coverage)
    
    revals$PTMdf <- data.frame(rbind(PTMdf, VisPTM), stringsAsFactors = F)
    
    # Clear search position modifications
    search$posMod <- list()
    
  }),
  
  # Select Modification: Exit
  observeEvent(input$ssAnnoExit, {
    removeModal()
    search$posMod <- list()
  }), 
  
  ####################
  ## RENDER WIDGETS ##
  ####################
  
  # Autofill with sequence
  output$VPseq <- renderUI({
    
    # Initiate Sequence
    seq <- ""
    
    # Change sequence to actual seq if it exists
    scan <- getScan()
    if (is.null(scan) == F) {seq <- scan[getScanClick(), "Sequence"]}
    VPseqUI <- textInput("VPSequence", "Sequence", seq, placeholder = "Enter Amino Acid Sequence")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(VPseqUI, Desc[Desc$Name == "VPSequence", "Title"], Desc[Desc$Name == "VPSequence", "Description"])
    } else {VPseqUI}
  }),
  
    # Auto fill picker with the glossary
  output$VPselect <- renderUI({

    # Get the glossary and sort it by the monoisotopic mass
    Glossary <- getGlossary()
    Glossary <- Glossary[order(Glossary$Monoisotopic.Mass),]
    
    # Make PTM Tag and higlight the most common, removing those from PTM
    PTM <- paste(Glossary$Interim.Name, "; ", Glossary$Monoisotopic.Mass, sep = "")
    MostCommon <- c("Carbamidomethyl; 57.021464", "Label:2H(3)+Oxidation; 19.013745",
                    "Deamidation; 0.984016", "Methyl; 14.01565", "Acetyl; 42.010565",
                    "Phospho; 79.966331", "Pyro-glu; -17.026549", "Pyro_glu; -18.010565")
    PTM <- PTM[PTM %in% MostCommon == F]
    
    pickerInput("VPpick", label = "Modifications", choices = list(`Most Common` = MostCommon,
                Others = PTM), selected = "Carbamidomethyl; 57.021464", multiple = T, 
                options = list(`live-search` = T, `virtual-Scroll` = 10, 
                `max-Options-Text` = HTML('<span style="font-size: 20pt;"><strong>
                <span style="color: #ff0000;">No more than 15 modifications
                </span></strong></span>'), `max-Options` = 15))
  }), 
  
  # Select Modifications Modal: Allow user to select the data of interest
  output$ssAnnoPicker <- renderUI({
    
    # Get the glossary and sort it by the monoisotopic mass
    Glossary <- getGlossary()
    Glossary <- Glossary[order(Glossary$Monoisotopic.Mass),]
    
    # Make PTM Tag and higlight the most common, removing those from PTM
    PTM <- paste(Glossary$Interim.Name, "; ", Glossary$Monoisotopic.Mass, sep = "")
    MostCommon <- c("Carbamidomethyl; 57.021464", "Label:2H(3)+Oxidation; 19.013745",
                    "Deamidation; 0.984016", "Methyl; 14.01565", "Acetyl; 42.010565",
                    "Phospho; 79.966331", "Pyro-glu; -17.026549", "Pyro_glu; -18.010565")
    PTM <- PTM[PTM %in% MostCommon == F]
    
    pickerInput("VPpickSelect", label = "Modifications", choices = list(`Most Common` = MostCommon,
                Others = PTM), multiple = F, 
                options = list(`live-search` = T, `virtual-Scroll` = 10))
  }), 
  
  ##########################
  ## RENDER TABLE / PLOTS ##
  ##########################
  
  # Inform user of set parameters
  output$VPsetparams <- renderText({
    
    # Set the ion groups to standard if none have been selected
    ionGroups <- input$ionGroups
    if (is.null(ionGroups)) {ionGroups <- c("a", "b", "c", "x", "y", "z", "Spec")}
    
    HTML(paste("<p>The following parameters have been set:</p><ul>
      <li><strong>Ion Groups:</strong>", paste(ionGroups, collapse = ", "), "</li>
      <li><strong>Min Intensity:</strong>", input$ssIntenMin, "</li>
      <li><strong>PPM Tolerance:</strong>", input$ssTolerance, "</li>
      <li><strong>Min Isotopic Percentage:</strong>", input$ssIsoPerMin, "</li>
      <li><strong>Min Correlation Score:</strong>", input$ssCorrScoreFilter, "</li>
      <li><strong>Min Precursor Percentage:</strong>", input$MPpercdiff, "</li></ul>
      <p>These parameters can be adjusted in 2. MS &amp; XIC.</p>"))
  }),

  # Select Modifications Modal: Show sequence plot
  output$ssAnnoSeq <- renderPlotly({
    
    # Get Sequence dataframe
    ModSeq <- getModSeq()
    if (is.na(ModSeq) || is.null(ModSeq)) {return(NULL)}
    
    # Set axis parameters
    ModSeq$x <- as.numeric(ModSeq$x)
    ModSeq$y <- as.numeric(ModSeq$y)
    xax <- list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE,
                showgrid = FALSE, range(0.5, 8.5))
    yax <- list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE,
                showgrid = FALSE)
    
    # Create hovertext information, changing for N and C term data
    hovertext <- lapply(1:nrow(ModSeq), function(row) {
      aa <- ModSeq$aa[row]
      pos <- ModSeq$pos[row]
      mod <- ModSeq$mod[row]
      if (grepl("-term", aa) == F) {
        return(paste(paste(aa, pos, sep = ""), "Modifications:", mod))
      } else {return(paste(aa, "Modifications:", mod))}
    }) %>% unlist()
    
    plot_ly(ModSeq, x = ModSeq$x, y = ModSeq$y, mode = "text", name = "",
           text = ~ModSeq$aa, type = "scatter",
           textfont = list(size = 28, color = ModSeq$color),
           hoverinfo = "text", hovertext = hovertext, source = "ssAnnoSeq") %>%
      layout(p, xaxis = xax, yaxis = yax, shapes = lines, showlegend = F)
    
  }),
  
  # Select Modifications Modal: Text box
  output$ssAnnoText <- renderText({
    
    # Get the ModSeq Dataframe
    ModSeq <- getModSeq()
    if (is.na(ModSeq) || is.null(ModSeq)) {return(NULL)}
    
    # Get the plotly click information 
    selected <- event_data("plotly_click", "ssAnnoSeq")
    
    # If nothing or something is clicked, inform user
    if (length(selected) == 0) {
      "Click an amino acid to add a modification."
    } else {
    aaPos <- ModSeq[ModSeq$x == selected$x & ModSeq$y == selected$y,]
    if (nrow(aaPos) == 0) {"Click an amino acid to add a modification."} else {
      if (grepl("-term", aaPos[1,]$aa)) {paste("Selected: ", aaPos[1,]$aa)} else { 
        paste("Selected: ", aaPos[1,]$aa, aaPos[1,]$pos, sep = "")}}}
  }),
  
  # Generate DT for the possible sequence combinations (modal results)
  output$VPTable <- DT::renderDataTable({
    VPTable <- getModTable()
    datatable(VPTable, rownames = F, filter = 'top', options = list(pageLength = 10),
              selection = list(mode = 'multiple'))
  }),
  
  # Generate DT with all possibilities and statistics
  output$VPmetrics <- DT::renderDataTable({
    
    VisPTM <- getVPMetrics()
    if (is.null(VisPTM)) {return(NULL)}
    
    # Check scan if MS1 level
    scan <- getScan()
    if (is.null(scan) | scan[getScanClick(), "Scan.Num"] == 1) {return(NULL)}
    
    # Get the C-term position number
    Cpos <- paste(" ", nchar(getNewVSeq()) + 1, ":", sep = "")
    
    # Change VisPTM 0 and nchar(seq) + 1 values to N- and C-term
    VisPTM$Name <- unlist(lapply(VisPTM$Name, function(mod) {
      mod <- paste("", mod)
      mod <- gsub(" 0:", " N-term:", mod)
      mod <- gsub(Cpos, " C-term", mod)
      mod <- trimws(mod, which = "left")
    }))
    
    datatable(VisPTM, rownames = F, filter = 'top', options = list(pageLength = 5),
              selection = list(mode = 'single', selected = 1))
  }),
  
  # Plot spectra with modifications
  output$VPSpec <- renderPlotly({

    # Get the peak data and scan number
    peak <- getSSPeak()
    scan <- getScan()
    if (is.null(peak) | is.null(scan) || nrow(peak) == 0) {return(NULL)}
    scanNum <- scan[getScanClick(), "Scan.Num"]
    
    # Return NULL if MS Scan is 1
    if (scan[getScanClick(), "MS.Level"] == 1) {return(NULL)}
    
    # Now get the fragment data and the original sequence DF
    frag <- getModFrag()
    oriSDF <- getModOriSDF()
    
    # Get modification name
    modName <- as.character(getVPMetrics()[getVPClick(), "Name"])
    if (grepl("0:", modName)) {modName <- gsub("0:", "N-term:", modName)}
    if (is.null(getNewVSeq()) == F) {
      endPos <- paste(nchar(getNewVSeq()) + 1, ":", sep = "")
      if (grepl(endPos, modName)) {modName <- gsub(endPos, "C-term:", modName)}
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
        }}
        
        # Declare X and Y range
        xrange <- c(0, max(spectra$mzExp) + 1)
        yrange <- c(0, max(spectra$intensity) + 1)
        
        p <- p %>% layout(xaxis = list(title = "M/Z (Mass to Charge)", range = xrange),
                          yaxis = list(title = "Intensity", range = yrange), 
                          title = paste("Scan:", scanNum), legend = list(orientation = "h"))
        
        plots$currVPSPEC <- p
        
        # Show plots
        p
      
  }),
  
  # Output Scan Warnings
  output$VPscanWarn <- renderText({
    scan <- getScan()
    if (is.null(scan)) {paste("No MS File Loaded")} else {
      paste("Selected Scan:", scan[getScanClick(), "Scan.Num"])
    } 
  }),
  
  # Output New Seq Warnings
  output$VPnsWarn <- renderText({
    Vseq <- input$VPSequence
    if (length(Vseq) == 0) {paste("No empty strings or whitespace")} else
    if (grepl("[[:space:]]", Vseq)) {paste("No empty strings or whitespace")} else
    if (grepl("\\[", Vseq) | grepl("\\]", Vseq)) {paste("Bracket notation not supported.",  
       "Try '4. Specific Mod Search'.")} else
    if (grepl("[^aA-zZ]", Vseq)) {paste("Letters only")} else
    if (nchar(Vseq) < 2) {paste("Sequence must have > 1 amino acid")} else
    if (grepl("[BbJjOoUuXxZz]", Vseq)) {paste("B, J, O, U, X, Z are incorrect options")} else
          {paste("Acceptable sequence")}
  }), 
  
  # Output warning that VisPTM will not be conducted on MS1 spectra
  output$VPMS1scanwarn <- renderText({
    scan <- getScan()
    if (is.null(scan)) {return(NULL)}
    MSLevel <- scan[getScanClick(), "MS.Level"]
    if (MSLevel == 1) {
      HTML('<p><strong><span style="font-size: 24px; color: rgb(184, 49, 47);">
           VisPTM does not accept modification visualization for MS1 scans. </span></strong></p>')
    }
  })
  
)