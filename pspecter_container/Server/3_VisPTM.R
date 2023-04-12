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
    
    if (length(revals$PTMList) > 0) {
      sendSweetAlert(session, "Dynamic Modification Search Error", 'Please click "Clear Proteoform Options"', type = "warning")
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
    
    if (length(revals$PTMList) > 0) {
      sendSweetAlert(session, "Apply Single Modification Error", 'Please click "Clear Proteoform Options"', type = "warning")
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
    revals$PTMList <- list()
  }),

  # Dynamic search calculations
  observeEvent(input$VPCalc, {
    
    if (is.null(GET_scan_metadata())) {
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
    
    # Add ions 
    if (is.null(input$ionGroups)) {IonGroups <- c("a", "b", "c", "x", "y", "z")} else {IonGroups <- input$ionGroups}
    RegIons <- c("a", "b", "c", "x", "y", "z")[which(c("a", "b", "c", "x", "y", "z") %in% IonGroups)]
    ModIons <- IonGroups[which(IonGroups %in% c("a", "b", "c", "x", "y", "z") == FALSE)]
    if (length(ModIons) == 0) {ModIons <- NULL}
    
    # Filter to selected modified ions
    if (!is.null(ModIons)) {
      
      NewIons <- make_mass_modified_ion(
        Ion = revals$AddedIons$Ion,
        Symbol = revals$AddedIons$Annotation,
        AMU_Change = revals$AddedIons$`AMU Change`
      )
      class(NewIons) <- c("data.table", "data.frame")
      NewIons <- NewIons %>% dplyr::filter(Modified_Ion %in% ModIons)
      ModIons <- NewIons
      class(ModIons) <- c(class(ModIons), "modified_ion")
      
    }
    
    withProgress({
      
      for (pos in 1:length(revals$PTMs)) {
        
        incProgress(1/length(revals$PTMs), paste("Calculating proteoform for row number:", pos))
        
        PTM <- revals$PTMs[pos]
        
        if (PTM == "No Modifications") {
          PTM <- GET_sequence()
          if (!is.character(convert_proforma(PTM))) {
            PTM <- attributes(convert_proforma(PTM))$pspecter$cleaned_sequence
          }
        }
        
       revals$PTMList[[pos]] <- get_matched_peaks(
          ScanMetadata = GET_scan_metadata(),
          PeakData = GET_peak_data(),
          PPMThreshold = input$ssTolerance,
          IonGroups = RegIons,
          CalculateIsotopes = ifelse(is.null(input$ssISOspectra), TRUE, input$ssISOspectra),
          MinimumAbundance = 0.1,
          CorrelationScore = input$ssCorrScoreFilter,
          MatchingAlgorithm = "closest peak", 
          AlternativeIonGroups = ModIons,
          AlternativeSequence = PTM
       )
        
        
      }
      
    })
    
  }),
    
  ##########################
  ## RENDER TABLE / PLOTS ##
  ##########################
  
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
    
    if (length(revals$PTMList) == 0) {
      
      PTM_DF <- data.frame(Row = 1:length(revals$PTMs), Modification = revals$PTMs)
    
    } else {
      
      # Extract statistics 
      theStats <- do.call(rbind, lapply(revals$PTMList, function(x) {
        if (is.null(x)) {return(c("0%", NA, 0))} else {
          return(c(attributes(x)$pspecter$Coverage, abs(attributes(x)$pspecter$MedianPPMError), nrow(x)))
        }
      })) %>% data.table()
      
      # Name columns
      colnames(theStats) <- c("Percent Coverage", "Median Absolute PPM Error", "Number of Matched Peaks")
      theStats$`Percent Coverage` <- gsub("%", "", theStats$`Percent Coverage`, fixed = T) %>% as.numeric()
      theStats$`Median Absolute PPM Error` <- as.numeric(theStats$`Median Absolute PPM Error`)
      theStats$`Number of Matched Peaks` <- as.numeric(theStats$`Number of Matched Peaks`)
      theStats$Row <- 1:nrow(theStats)
      theStats$Modification <- revals$PTMs
      theStats <- theStats %>% dplyr::select(Row, Modification, `Percent Coverage`, `Median Absolute PPM Error`, `Number of Matched Peaks`)
      
      revals$PTMdf <- theStats
      PTM_DF <- theStats
      
    } 
    
    datatable(PTM_DF, rownames = F, filter = 'top', options = list(pageLength = 5, scrollX = T),
              selection = list(mode = 'single', selected = 1))
  }),
  
  # Plot spectra with modifications
  output$VPSpec <- renderPlotly({
    
    if (length(revals$PTMList) == 0) {return(NULL)}
    
    annotated_spectrum_plot(
      PeakData = GET_peak_data(),
      MatchedPeaks = revals$PTMList[[GET_vis_click()]],
      LabelSize = 6,
      Interactive = TRUE
    )
    

  }),

  # Plot spectra with modifications
  output$VPseqflags <- renderPlot({
    
    if (length(revals$PTMList) == 0) {return(NULL)}
    
    sequence_plot(
      MatchedPeaks = revals$PTMList[[GET_vis_click()]],
      RemoveChargeAnnotation = TRUE,
      WrapLength = 10
    )
    
  })

  
)