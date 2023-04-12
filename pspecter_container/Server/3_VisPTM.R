## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains the table and spectra for Vis PTM

list(
  
  ##############
  ## FUNCTION ##
  ##############
  
  stitch_model_mods <- function(SeqSplit, theMod1, number, theMod2 = NULL) {
    
    # Get glossary
    Glossary <- GET_glossary()
    
    # Extract residues 
    Residue1 <- Glossary[Glossary$Modification == theMod1, "Residues"]
    
    # Extract residues to search 
    ResidueSearch1 <- Residue1 %>% 
      unlist() %>% 
      strsplit(" ") %>% 
      unlist() %>% 
      .[. %in% c("C-term", "N-term") == FALSE]
    
    if (length(ResidueSearch1) == 0) {
      sendSweetAlert(title = "Dynamic Modification Search Warning", text = paste("The modification", theMod1, "has no applicable residues in this sequence"), type = "warning")
      return(NULL)
    }
    
    if (is.null(theMod2)) {
      return(
        paste0(theMod1, ",X(", paste0(which(SeqSplit %in% ResidueSearch1), collapse = ","), ")[", number, "]")
      )
    } else {
      
      Residue2 <- Glossary[Glossary$Modification == theMod2, "Residues"]
      
      ResidueSearch2 <- Residue2 %>% 
        unlist() %>% 
        strsplit(" ") %>% 
        unlist() %>% 
        .[. %in% c("C-term", "N-term") == FALSE]
      
      if (length(ResidueSearch2) == 0) {
        sendSweetAlert(title = "Dynamic Modification Search Warning", text = paste("The modification", theMod2, "has no applicable residues in this sequence"), type = "warning")
        return(NULL)
      }
      
      return(
        c(
          paste0(theMod1, ",X(", paste0(which(SeqSplit %in% ResidueSearch1), collapse = ","), ")[1]"),
          paste0(theMod1, ",X(", paste0(which(SeqSplit %in% ResidueSearch1), collapse = ","), ")[2]"),
          paste0(theMod2, ",X(", paste0(which(SeqSplit %in% ResidueSearch2), collapse = ","), ")[1]"),
          paste0(theMod2, ",X(", paste0(which(SeqSplit %in% ResidueSearch2), collapse = ","), ")[2]"),
          paste0(theMod1, ",X(", paste0(which(SeqSplit %in% ResidueSearch1), collapse = ","), ")[1];", 
                 theMod2, ",X(", paste0(which(SeqSplit %in% ResidueSearch2), collapse = ","), ")[1]")
        )
      )
      
    }
    
  }, 
  
  ###############
  ## RENDER UI ##
  ###############
  
  # Auto fill picker with the glossary
  output$VPselect <- renderUI({
    
    # Get the glossary and sort it by the monoisotopic mass
    Glossary <- GET_glossary()
    
    # Make PTM Tag and higlight the most common, removing those from PTM
    PTM <- paste(Glossary$Modification, "; ", Glossary$`Mass Change`, sep = "")
    MostCommon <- c("Carbamidomethyl; 57.021464", "Label:2H(3)+Oxidation; 19.013745",
                    "Deamidation; 0.984016", "Methyl; 14.01565", "Acetyl; 42.010565",
                    "Phospho; 79.966331", "Pyro-glu; -17.026549", "Pyro_glu; -18.010565")
    PTM <- PTM[PTM %in% MostCommon == F]
    
    pickerInput("VPpick", label = "Modifications", choices = list(`Most Common` = MostCommon,
                                                                  Others = PTM), selected = "Carbamidomethyl; 57.021464", multiple = T, 
                options = list(`live-search` = T, `virtual-Scroll` = 10, 
                               `max-Options-Text` = HTML('<span style="font-size: 20pt;"><strong>
                <span style="color: #ff0000;">No more than 2 modifications
                </span></strong></span>'), `max-Options` = 2))
  }), 
  
  output$VPspecificUI <- renderUI({
    textInput("VPspecific", "Add a sequence to test in ProForma format", GET_sequence(), placeholder = "Enter Amino Acid Sequence in ProForma format")
  }),
  
  output$VPWarn <- renderText({
    
    Nseq <- input$VPspecific
    if (is.null(Nseq) || is.na(Nseq) || Nseq == "") {return("No empty strings or whitespace")} else 
    
    # Test that the square brackets are added
    if (grepl("\\[", Nseq) & !grepl("\\]", Nseq) |
        !grepl("\\[", Nseq) & grepl("\\]", Nseq)) {return("Complete the brackets to test a modification")} else
          
    # No spaces 
    if (grepl("[[:space:]]", Nseq)) {return("No empty strings or whitespace")} else
            
    # Extract modifications 
    if (grepl("\\[", Nseq)) {
      Split <- Nseq %>% strsplit("\\[|\\]") %>% unlist()
      Modifications <- Split[c(FALSE, TRUE)]
      
      # Iterate through modifications
      Responses <- lapply(Modifications, function(mod) {
        
        # Test if numeric 
        if (is.na(as.numeric(mod))) {
          if (!(mod %in% GET_glossary()$Modification)) {paste0(mod, " is not in the modifications glossary")}
        }
        
      }) %>% unlist() %>% paste(collapse = " ")
      
      # Combine the sequence
      Nseq <- Split[c(TRUE, FALSE)] %>% paste0(collapse = "")
      
      if (Responses != "") {return(Responses)}
    }
  
    # Test amino acids   
    if (nchar(Nseq) < 2) {return("Sequence must have > 1 amino acid")} else
      if (grepl("[BbJjOoUuXxZz]", Nseq)) {return("B, J, O, U, X, Z are incorrect options for amino acids")} else
      {return("Acceptable sequence")}
  
  }),
  
  output$VPGeneralMessage <- renderText({
    ifelse(is.null(revals$PTMs), "Use the sidebar to test modifications", "")
  }),

  ################
  ### OBSERVERS ##
  ################
  
  # If calculate is clicked, then start calculating the modifications table
  observeEvent(input$VPposs, {
    
    # Scan and sequence required
    if (is.null(GET_scan_metadata())) {
      sendSweetAlert(title = "Dynamic Modification Search Error", text = "Please upload MS data", type = "error")
      return(NULL)
    }
    if (is.null(GET_sequence())) {
      sendSweetAlert(title = "Dynamic Modification Search Error", text = "Please enter a test sequence in Visualize MS & XIC", type = "error")
      return(NULL)
    }
    
    # Get required values
    mods <- input$VPpick
    
    if (is.null(mods)) {
      sendSweetAlert(session, "No Modifications Selected", 
                     "Select some modifications", type = "error")
      return(NULL)
    }
    
    modPerSeq <- as.numeric(input$VPmaxmod)
    seq <- GET_sequence()
    Glossary <- GET_glossary()
    
    # Fix seq 
    convert <- convert_proforma(seq)
    if (!is.character(convert)) {
      seq <- attributes(convert)$pspecter$cleaned_sequence
    } else {seq <- convert}
    
    # If there is a sequence longer than 0 and modifications have been selected,
    # then permute all possibilities
    if (is.null(seq) == F & length(seq) > 0 & is.null(mods) == F & is.null(GET_scan_metadata()) == F) {
      
      SeqSplit <- seq %>% strsplit("") %>% unlist()
      
      # Get all the PTM names and select them from the glossary
      PTMs <- lapply(strsplit(mods, ";"), function(el) {el[1]}) %>% unlist()
      
      # Pull test 
      if (length(PTMs) == 1) {
        
        # Pull all possible positions
        MultipleModFormat <- stitch_model_mods(SeqSplit, PTMs, 1)
        if (is.null(MultipleModFormat)) {return(NULL)}
        Proteoforms <- multiple_modifications(seq, MultipleModFormat)
        
        # Add 2's if possible
        if (modPerSeq == 2) {
          MultipleModFormat <- stitch_model_mods(SeqSplit, PTMs, 2)
          if (is.null(MultipleModFormat)) {return(NULL)}
          Proteoforms2 <- multiple_modifications(seq, MultipleModFormat)
          Proteoforms <- c(Proteoforms, Proteoforms2)
        }
        
        
      } else {
        
        PTM1 <- PTMs[1]
        PTM2 <- PTMs[2]
        
        if (modPerSeq == 1) {
          checkList <- c(stitch_model_mods(SeqSplit, PTM1, 1), stitch_model_mods(SeqSplit, PTM2, 1))
        } else {
          checkList <- stitch_model_mods(SeqSplit, PTM1, 2, PTM2)
        }
        
        if (is.null(checkList)) {return(NULL)}
   
        Proteoforms <- do.call(c, lapply(checkList, function(x) {multiple_modifications(seq, x)})) %>% unique()
        
      
      }
      
      # If no modifications is not already in revals$PTMs
      if ("No Modifications" %in% revals$PTMs == FALSE) {
        revals$PTMs <- c("No Modifications", Proteoforms)
      } else {
        revals$PTMs <- c(revals$PTMs, Proteoforms)
      }
      
      revals$PTMs <- unique(revals$PTMs)
      
    }
    
    
  }),
  
  # Modal pop up box for specific annotations
  observeEvent(input$VPspecificConfirm, {
    
    # Send alert if no MS data exists
    if (is.null(GET_scan_metadata())) {
      sendSweetAlert(session, "No MS Data error!", "Please upload MS data", type = "error")
      return(NULL)
    }
    
    # Check sequence 
    if (!is_sequence(input$VPspecific)) {
      sendSweetAlert(session, "Apply Single Modification Error", "Unacceptable ProForma string", type = "error")
      return(NULL)
    }
    
    if (is.character(convert_proforma(input$VPspecific))) {
      if ("No Modifications" %in% revals$PTMs == FALSE) {revals$PTMs <- "No Modifications"}
    } else {
      revals$PTMs <- c(revals$PTMs, input$VPspecific)
    }
    
    revals$PTMs <- unique(revals$PTMs)
    
  }),
  
  # Remove values
  observeEvent(input$VPReset, {
    revals$PTMs <- NULL
  }),

  # Dynamic search calculations
  observeEvent(input$VPCalc, {
    
    if (is.null(GET_scan())) {
      sendSweetAlert(session, "VisPTM Error!", "Upload an MS File First.",
                     "error")
      return(NULL)
    }
   
    if (is.null(GET_sequence())) {
      sendSweetAlert(session, "VisPTM Error!", "Please input a valid sequence.",
                     "error")
      return(NULL)
    }
    
    if (length(revals$PTMs) < 1) {
      sendSweetAlert(session, "VisPTM Error!", "No modifications have been specified.",
                     "error")
      return(NULL)
    }
  #  
  #  withProgress({ 
  #
  #    # Set incremental progress bar
  #    incProgress(amount = 0.1, "Calculating Theoretical Spectrum")
  #      
  #    # Get PTM values
  #    PTM <- revals$PTMs
  #    
  #    # Shorten glossary to selected PTMs
  #    Glossary <- getGlossary()      
  #    Selected <- lapply(strsplit(input$VPpick, ";"), function(el) {el[1]}) %>% unlist()
  #    Glossary <- Glossary[Glossary$Interim.Name %in% Selected,]
  #    
  #    # Get the peak data and scan number
  #    peak <- getSSPeak()
  #    if (is.null(peak)) {return(NULL)}
  #    seq <- getNewVSeq()
  #    scan <- getScan()
  #    scanNum <- scan[getScanClick(), "Scan.Num"]
  #    
  #    # Get theoretical fragment data
  #    frag <- getCalcFrag(seq, input$ionGroups, 1:scan[scan$Scan.Num == scanNum, "Pre.Charge"], revals$AddedIons)
  #    
  #    incProgress(amount = 0.5, "Acquiring Modification Data")
  #    
  #    # Start for loop to get all possibilities
  #    VisPTM <- lapply(1:length(PTM), function(row) {
  #      
  #      incProgress(amount = 0, paste("Calculating Metrics: ",
  #                                   round(row/length(PTM), 2) * 100, "% Row: ", row, sep = ""))
#
  #      
  #      if (PTM[row] != "No Modifications") {
  #      
  #        # 1. Build modifications data frame
  #        mod <- buildModDF(scanNum, seq, PTM[row], Glossary)
  #        
  #        # 2. Add Mod Mass
  #        modMass <- addModMass(mod, frag, scanNum, seq, Glossary)
  #        
  #        # 3. Apply Mod
  #        frag <- applyMod(modMass, frag)
  #        
  #        # 4. Get Molecular Formula
  #        frag <- getMolecularFormula(frag)
  #        
  #        # 5. Get Calculated Isotope Values
  #        frag <- getCalcIso(frag, 0.1)
  #        
  #        # 6. Get Annotated Peaks
  #        frag <- getAnnotatedPeaks(peak, frag, 10)
  #        frag$error <- (frag$mzExp - frag$mz) / frag$mz * 1e6
  #        
  #        # If no lines of data, manually input NA for mean
  #        if (nrow(frag) == 0) { 
  #          return(cbind(Name = PTM[row], Average.PPM.Error = NA, Number.of.Ions = 0, Coverage = 0))
  #        } else {
  #          seqLen <- nchar(seq) - 1
  #          covLen <- length(unique(frag$npos[frag$npos != 1]))
  #          return(cbind(Name = PTM[row], Average.PPM.Error = mean(abs(frag$error)), Number.of.Ions = nrow(frag),
  #                       Coverage = round((covLen / seqLen * 100), 1)))
  #        }
  #      } else {
  #        
  #        # If no modifications, then get the data from page 2
  #        frag <- getNoModFrag()
  #        if (is.null(frag)) {
  #          return(cbind(Name = "No Modifications", Average.PPM.Error = NA, Number.of.Ions = 0,
  #                       Coverage = 0))
  #        } else {
  #          seqLen <- nchar(seq) - 1
  #          covLen <- length(unique(frag$npos[frag$npos != 1]))
  #          return(cbind(Name = "No Modifications", Average.PPM.Error = mean(abs(frag$error)),
  #                       Number.of.Ions = nrow(frag), 
  #                       Coverage = round((covLen / seqLen * 100), 1)))}}})
  #  
  #    VisPTM <- do.call(rbind, VisPTM)
  #  })
  #  
  #  removeModal()
  #  VisPTM[,1] <- as.character(VisPTM[,1])
  #  VisPTM[,2] <- as.numeric(VisPTM[,2])
  #  VisPTM[,3] <- as.numeric(VisPTM[,3])
  #  revals$PTMdf <- rbind(revals$PTMdf, VisPTM)
  #  
  }),


  

  
  
  #####################
  ### RENDER WIDGETS ##
  #####################
  #
  ## Autofill with sequence
  #output$VPseq <- renderUI({
  #  
  #  # Initiate Sequence
  #  seq <- ""
  #  
  #  # Change sequence to actual seq if it exists
  #  scan <- getScan()
  #  if (is.null(scan) == F) {seq <- scan[getScanClick(), "Sequence"]}
  #  VPseqUI <- textInput("VPSequence", "Sequence", seq, placeholder = "Enter Amino Acid Sequence")
  #  if (is.null(input$infoMode) == F && input$infoMode == T) {
  #    popify(VPseqUI, Desc[Desc$Name == "VPSequence", "Title"], Desc[Desc$Name == "VPSequence", "Description"])
  #  } else {VPseqUI}
  #}),
  #

  
  ## Select Modifications Modal: Allow user to select the data of interest
  #output$ssAnnoPicker <- renderUI({
  #  
  #  # Get the glossary and sort it by the monoisotopic mass
  #  Glossary <- getGlossary()
  #  Glossary <- Glossary[order(Glossary$Monoisotopic.Mass),]
  #  
  #  # Make PTM Tag and higlight the most common, removing those from PTM
  #  PTM <- paste(Glossary$Interim.Name, "; ", Glossary$Monoisotopic.Mass, sep = "")
  #  MostCommon <- c("Carbamidomethyl; 57.021464", "Label:2H(3)+Oxidation; 19.013745",
  #                  "Deamidation; 0.984016", "Methyl; 14.01565", "Acetyl; 42.010565",
  #                  "Phospho; 79.966331", "Pyro-glu; -17.026549", "Pyro_glu; -18.010565")
  #  PTM <- PTM[PTM %in% MostCommon == F]
  #  
  #  pickerInput("VPpickSelect", label = "Modifications", choices = list(`Most Common` = MostCommon,
  #              Others = PTM), multiple = F, 
  #              options = list(`live-search` = T, `virtual-Scroll` = 10))
  #}), 
  #
  ###########################
  ### RENDER TABLE / PLOTS ##
  ###########################
  #
  ## Inform user of set parameters
  output$VPsetparams <- renderText({
    
    # Pull scan number
    ScanNumber <- ""
    if (!is.null(GET_scan_metadata())) {
      ScanNumber <- GET_scan_metadata()[GET_scan_click(), "Scan Number"]
    }
    
    # Clean sequence
    Sequence <- ""
    if (!is.null(GET_sequence())) {
      
      # Remove modificaitons if appropriate
      convert <- GET_sequence() %>% convert_proforma()
      
      if (!is.character(convert)) {
        Sequence <- attributes(convert)$pspecter$cleaned_sequence
      } else {
        Sequence <- convert
      }
      
      # Wrap every 15 characters
      Sequence <- paste0('<p style="margin:0;">', gsub("(.{25})", "\\1 ", Sequence) %>% strsplit(" ") %>% unlist() %>% paste0(collapse = '</p><p style="margin:0;">'), "</p>")
      
    }
    
    # Set the ion groups to standard if none have been selected
    ionGroups <- input$ionGroups
    if (is.null(ionGroups)) {ionGroups <- c("a", "b", "c", "x", "y", "z", "Spec")}
    
    HTML(paste("<p>The following parameters have been set:</p><ul>
      <li><strong>Scan Number:</strong>", ScanNumber, "</li>
      <li><strong>Unmodified Sequence:</strong>", Sequence, "</li>
      <li><strong>PPM Tolerance:</strong>", input$ssTolerance, "</li>
      <li><strong>Min Intensity:</strong>", input$ssIntenMin, "</li>
      <li><strong>Min Correlation Score:</strong>", input$ssCorrScoreFilter, "</li>
      <li><strong>Ion Groups:</strong>", paste(ionGroups, collapse = ", "), "</li>
      <li><strong>Include Isotopes:</strong>", input$ssISOspectra, "</li>
      <p>These parameters can be adjusted in Visualize MS &amp; XIC.</p>"))
  }),

  # Generate DT with all possibilities and statistics
  output$VPmetrics <- DT::renderDataTable({
    
    if (is.null(revals$PTMs)) {return(NULL)}
      
    PTM_DF <- data.frame(Row = 1:length(revals$PTMs), Modification = revals$PTMs)
  
    datatable(PTM_DF, rownames = F, filter = 'top', options = list(pageLength = 5),
              selection = list(mode = 'single', selected = 1))
  }),
  
  # Plot spectra with modifications
  output$VPSpec <- renderPlotly({
    
    return(NULL)

  }),

  # Plot spectra with modifications
  output$VPseqflags <- renderPlot({
    
    return(NULL)
    
  })

  
)