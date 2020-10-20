## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_06_15

# DESCRIPTION: This contains the Visualize Modifications Getters: 
# getNewVSeq, getModTable, getModSeq, getVPClick, getVPMetrics, buildModDF,
# getModOriSDF, getNoModFrag, getModFrag, getVPErrorHM, getVPFlagDF, getVPPreIsoDist,
# getVPMatchedPrevious, getVPMatchedNext

list(
  
  # Check whether inputted sequence meets requirements
  getNewVSeq <- reactive({
    Vseq <- input$VPSequence
    if (length(Vseq) == 0) {return(NULL)} else
    if (grepl("\\[", Vseq) | grepl("\\]", Vseq)) {return(NULL)} else
    if (nchar(Vseq) < 1) {return(NULL)} else
    if (grepl("[[:space:]]", Vseq)) {return(NULL)} else
    if (grepl("[^aA-zZ]", Vseq)) {return(NULL)} else
    if (nchar(Vseq) < 2) {return(NULL)} else
    if (grepl("[BJOUXZ]", Vseq)) {return(NULL)} else
       {return(toupper(Vseq))}
  }),
  
  # If calculate is clicked, then start calculating the modifications table
  observeEvent(input$VPposs, {
    
    # Get required values
    mods <- input$VPpick
    
    if (is.null(mods)) {
      sendSweetAlert(session, "No Modifications Selected", 
                     "Select some modifications", type = "error")
      return(NULL)
    }
    
    modPerSeq <- as.numeric(input$VPmaxmod)
    modPerPos <- as.numeric(input$VPmaxpep)
    seq <- getNewVSeq()
    Glossary <- getGlossary()
    
    # If there is a sequence longer than 0 and modifications have been selected,
    # then permute all possibilities
    if (is.null(seq) == F & length(seq) > 0 & is.null(mods) == F & is.null(getScan()) == F) {
      
      withProgress({
      
      # Get all the PTM names and select them from the glossary
      PTMs <- lapply(strsplit(mods, ";"), function(el) {el[1]}) %>% unlist()
      Glossary <- Glossary[Glossary$Interim.Name %in% PTMs,]
      
      # Split sequence string by AA
      seqSplit <- c("N-term", unlist(strsplit(seq, "")), "C-term")
      
      incProgress(0, "Generating Position Matches: 0%")
      
      # Get all position matches (as a number relating to position) along with name 
      modCodes <- unlist(lapply(1:nrow(Glossary), function(row) {
        
        # Include Position Matches Progress
        incProgress(0, paste("Generating Position Matches: ", 
                             round(row/nrow(Glossary), 2) * 100, "%", sep = ""))
        
        # Split modified glossary data
        modSites <- unlist(strsplit(as.character(Glossary$Modified.Sites[row]), " "))
        
        # Get positions of matches between the Glossary's site and the actual sequence
        match <- which(seqSplit %in% modSites)
        seqSplit <- unlist(strsplit(seq, ""))
        seqSplit <- c("N-term", paste(seqSplit, 1:length(seqSplit), sep = ""), "C-term")
        if (length(match) > 0) {
          paste(seqSplit[match], ": ", Glossary$Interim.Name[row], sep = "")
        }
    
      }))  
      
      incProgress(0.2, "Combining All Possible Matches: 0%")
      
      # Limit modification by sequence. Generate all combinations of possible modifications
      PTM <- lapply(1:modPerSeq, function(targetSize) {
        incProgress(0, paste("Combining All Position Matches: ", 
                             round(targetSize/modPerSeq, 2) * 100, "%", sep = ""))
        comb <- NULL
        if (targetSize <= length(modCodes)) {
          comb <- combinations(n = length(modCodes), r = targetSize, v = modCodes, repeats.allowed = F)
          comb <- lapply(1:nrow(comb), function(row) {
            paste(comb[row,], collapse = ", ")}) %>% unlist()
        }
       return(comb)}) %>% unlist()
      
      incProgress(0.2, "Removing Duplicates: 0%")
       
      # Limit modifications by peptide position
      PTM <- do.call(rbind, lapply(PTM, function(mod) {
        incProgress(0, paste("Combining All Position Matches: ", 
                             round(match(mod, PTM) / length(PTM), 2) * 100, "%", sep = ""))
        counts <- max(as.vector(table(unlist(strsplit(unlist(strsplit(mod, ", ")), ": "))[c(TRUE, FALSE)])))
        if (counts > modPerPos) {return(NULL)} else{return(mod)}
      }))
      
    })
      
      # Save generate PTMs and add "No Modifications" option
      revals$PTMs <- PTM
      revals$PTMs[length(revals$PTMs) + 1] <- "No Modifications"
    }
    
    showModal(modalDialog(fluidPage(
      DT::dataTableOutput("VPTable", width = "100%", height = "250px")),
      title = HTML('<p style="text-align: center;">Remove Any Options?</p>'),
      footer = list(actionButton("VPdelrow", "Delete Selected"),
                    actionButton("VPkeeprow", "Keep Selected"),
                    actionButton("VPcalc", "Calculate"),
                    modalButton("Exit")), size = "l", easyClose = T))

  }),
  
  # Generate the modifications table information
  getModTable <- reactive({
    
    # Get all the PTM combinations after they've been determined
    VPTable <- revals$PTMs
    VPTable <- data.frame(`Modified Sites` = unlist(VPTable))
    if (nrow(VPTable) < 1) {return(NULL)}
    
    # Mass Flag (add whether the added PTM falls within the mass)
    Glossary <- getGlossary()
    Glossary <- Glossary[Glossary$Interim.Name %in% lapply(strsplit(input$VPpick, ";"), 
                         function(el) {el[1]}) %>% unlist(),]
    
    return(VPTable)
  }),
  
  # Put sequence in a dataframe for easy plotting
  getModSeq <- reactive({
    
    seq <- getNewVSeq()
    if (is.null(seq) || is.na(seq)) {return(NULL)}
    
    # Get the amino acid, position, x value, y value, and modifications
    ModSeq <- data.frame(do.call(rbind, lapply(1:nchar(seq), function(pos) {
      aa <- unlist(strsplit(seq, ""))[pos]
      x <- pos %% 8
      if (x == 0) {x <- 8}
      y <- -((pos + 7) %/% 8)
      mod <- "None"
      color <- "rgb(0,0,0)"
      return(cbind(aa, pos, x, y, mod, color))
    })), stringsAsFactors = F)
    
    # Add N and C-term
    ModSeq <- data.frame(rbind(ModSeq, data.frame(aa = c("N-term", "C-term"), 
                pos = c(0, nrow(ModSeq) + 1), x = c(1.5, 1.5), y = c(0, min(as.numeric(ModSeq$y)) - 1), 
                mod = c("None", "None"), color = c("rgb(0,0,0)", "rgb(0,0,0)"))))
    
    # Add position and modifications (Select Modification Only)
    if (length(search$posMod) > 0) {
      # Unlist all posMods into a vector
      posMods <- unlist(search$posMod)
      for (posMod in posMods) {
        posMod <- unlist(strsplit(as.character(posMod), " "))
        pos <- as.numeric(posMod[1])
        mod <- paste(posMod[2], ": ", posMod[3], sep = "")
        if (ModSeq[ModSeq$pos == pos, "mod"] == "None") {
          ModSeq[ModSeq$pos == pos, "mod"] <- mod
        } else {ModSeq[ModSeq$pos == pos, "mod"] <- paste(ModSeq[ModSeq$pos == pos, "mod"], mod)}
        ModSeq[ModSeq$pos == pos, "color"] <- "rgb(255,40,0)"
      }
    }
    return(ModSeq)
  }), 
  
  # Get VP Table Click
  getVPClick <- reactive({
    clicked <- input$VPmetrics_row_last_clicked
    if (is.null(clicked)) {clicked <- 1}
    return(clicked)
  }),
  
  # Get VP Metrics 
  getVPMetrics <- reactive({
    
    # Return NULL is no scan
    if (is.null(getScan())) {return(NULL)}
    
    # Convert VisPTM to data.frame if it's not null
    VisPTM <- revals$PTMdf
    if (is.null(VisPTM) == F) {
      VisPTM <- data.frame(VisPTM)
      VisPTM[,1] <- as.character(VisPTM[,1])
      VisPTM[,2] <- as.numeric(as.character(VisPTM[,2]))
      VisPTM[,3] <- as.numeric(as.character(VisPTM[,3]))
      VisPTM[,4] <- as.numeric(as.character(VisPTM[,4]))
    }
    
    # Add the no modifications data and peptide database search tool identified modifications
    NM <- NULL
    if ("No Modifications" %in% VisPTM[,1] == F) {
      
      # Check if there are any peptide database search tool identified modifications
      scanNum <- getScan()[getScanClick(), "Scan.Num"]
      seq <- getNewVSeq()
      if (is.null(seq) || is.na(seq)) {seq <- getScan()[getScanClick(), "Sequence"]}
      mod <- getModDF()
      modPerSeq <- mod[mod$spectrumID == scanNum & mod$sequence == seq,]
      
      # If so, add the identified modifications
      if (is.null(modPerSeq) == F && nrow(modPerSeq) > 0) {

        # Calculate Modification
        peak <- getSSPeak()
        Glossary <- getGlossary()
        ionGroups <- input$ionGroups
        if (is.null(ionGroups)) {ionGroups <- c("a", "b", "c", "x", "y", "z", "Spec")}
        mztolppm <- input$ssTolerance
        intFilter <- input$ssIsoPerMin / 100
        frag <- getCalcFrag(seq, ionGroups, 1:getScan()[getScanClick(), "Pre.Charge"])
        if (is.null(frag)) {return(NULL)}
        modMass <- addModMass(mod, frag, scanNum, seq, Glossary)
        frag <- applyMod(modMass, frag)
        frag <- getMolecularFormula(frag)
        frag <- getCalcIso(frag, intFilter)
        frag <- getAnnotatedPeaks(peak, frag, mztolppm)
        frag$error <- (frag$mzExp - frag$mz) / frag$mz * 1e6
        if (is.null(frag) == F) {if (nrow(frag) == 0) {frag <- NULL}}
        M <- frag
        modName <- paste(lapply(1:nrow(modPerSeq), function(row) {
          pos <- modPerSeq[row, "location"]
          name <- modPerSeq[row, "name"]
          aa <- substr(seq, pos, pos)
          paste(aa, pos, ": ", name, sep = "")
        }), collapse = ", ")
        
        # Get the modification data and name
        NM <- getNoModFrag()
        
        if (is.null(NM) & is.null(M)) {
          NM <- data.frame(Name = c("No Modifications", modName), 
                           Average.PPM.Error = c(NA, NA),
                           Number.of.Ions = c(0, 0), Coverage = c(0, 0))
        } else if (is.null(NM) & is.null(M) == F) {
          cov <- round((length(unique(frag$npos[frag$npos != 1])) / nchar(seq) - 1) * 100, 1)
          NM <- data.frame(Name = c("No Modifications", modName), 
                           Average.PPM.Error = c(NA, mean(abs(M$error))),
                           Number.of.Ions = c(0, nrow(M)), 
                           Coverage = c(0, cov))
        } else if (is.null(NM) == F & is.null(M)) {
          cov <- round((length(unique(frag$npos[frag$npos != 1])) / nchar(seq) - 1) * 100, 1)
          NM <- data.frame(Name = c("No Modifications", modName), 
                           Average.PPM.Error = c(mean(abs(NM$error)), NA),
                           Number.of.Ions = c(nrow(NM), 0), Coverage = c(cov, 0))} else {
          cov <- round((length(unique(frag$npos[frag$npos != 1])) / nchar(seq) - 1) * 100, 1)
          NM <- data.frame(Name = c("No Modifications", modName), 
                           Average.PPM.Error = c(mean(abs(NM$error)), mean(abs(M$error))),
                           Number.of.Ions = c(nrow(NM), nrow(M), Coverage = c(cov, cov)))}
      } else {
      
        frag <- getNoModFrag()
        if (is.null(frag)) { 
          NM <- data.frame(Name = "No Modifications", Average.PPM.Error = NA,
                           Number.of.Ions = 0, Coverage = 0)} else {
          seqLen <- nchar(seq) - 1
          covLen <- length(unique(frag$npos[frag$npos != 1]))
          NM <- data.frame(Name = "No Modifications", 
                           Average.PPM.Error = mean(abs(frag$error)),
                           Number.of.Ions = nrow(frag), 
                           Coverage = round((covLen / seqLen * 100), 1))}
    }}
    
    VisPTM <- unique(rbind(NM, VisPTM))
  
    return(VisPTM)
    
  }),
  
  # This function allows for the creation of a modification dataframe
  buildModDF <- function(scanNum, seq, singlePTM, Glossary) {
    
    splitPTM <- unlist(strsplit(unlist(strsplit(singlePTM, ", ")), ": "))
    name <- splitPTM[c(FALSE, TRUE)]
    pos <- splitPTM[c(TRUE, FALSE)]
    
    # Correct positional information
    pos <- unlist(lapply(pos, function(el) {
                  if (el == "N-term") {return(0)} else
                  if (el == "C-term") {return(nchar(seq))} else {
                  return(as.numeric(gsub("[aA-zZ]", "", el)))}}))
    
    # Get mass data
    mass <- unlist(lapply(1:length(name), function(el) {
                   if (grepl("Added_Mass", name[el])) {
                   return(as.numeric(unlist(strsplit(name[el], "="))[2]))} else {
                   return(Glossary[Glossary$Interim.Name == name[el],]$Monoisotopic.Mass)}}))
    
    mod <- data.frame(spectrumID = scanNum, sequence = seq, name = name, mass = mass,
                      location = pos)
    
    # C position
    mod$cpos <- lapply(1:nrow(mod), function(row) {
      nchar(as.character(mod$sequence[row])) - mod$location[row] + 1}) %>% unlist()
    
    # Add sequence length
    mod$length <- nchar(as.character(mod$sequence))
    
    # If location is larger than length, then adjust
    mod$location[mod$location > mod$length] <- mod[mod$location > mod$length,]$length
    mod$cpos[mod$cpos > mod$length] <- mod[mod$cpos > mod$length,]$length
    
    return(mod)
  },
  
  # Get Modifications Original Sequence Data Frame
  getModOriSDF <- reactive({
    
    # Get clicked sequence
    clicked <- getVPClick()
    scan <- getScan()
    if (is.null(scan)) {return(NULL)}
    
    seq <- input$VPseq
    if (is.null(seq)) {seq <- scan[getScanClick(), "Sequence"]}
    
    if (length(seq) > 0) { if (is.na(seq) == F) {
      
      # Give x and y coordinates to each sequence letter for graphing
      x <- c()
      y <- c()
      for (i in 1:nchar(seq)) {
        xtest <- i %% 6
        if (xtest != 0) {x[i] <- xtest} else {x[i] <- 6}
        y[i] <- -((i + 5) %/% 6)}
      
      # Generate xy coordinate dataframe, and give it a position for merging
      oriSDF <- data.frame(strsplit(seq, ""), x, y, 1:nchar(seq), "None")
      colnames(oriSDF) <- c("Peptide", "X", "Y", "npos", "Mod")
      
      return(oriSDF)
    }}
    
    return(NULL)
  
  }),
  
  # Get No Modifications Frag
  getNoModFrag <- reactive({
    
    # Source the getFrag functions
    source(file.path("Server", "Get", "Get_Frag.R"), local = T)$value
    
    # Get all the required data: Scan (seq, charge, scanNum), ionGroups,
    # modification, Glossary, peak data, mztolppm, % intensity filter
    scan <- getScan()
    peak <- getSSPeak()
    if (is.null(scan) | is.null(peak)) {return(NULL)}
    
    # Scan data 
    clicked <- getScanClick()
    scanNum <- scan[clicked, "Scan.Num"]
    charge <- 1:scan[clicked, "Pre.Charge"]
    seq <- getNewVSeq()
    if (is.null(seq)) {seq <- scan[clicked, "Sequence"]}
    
    # Return nothing if no seq or scan number
    if (is.null(seq) || is.na(seq) || is.null(scanNum)) {return(NULL)}
    
    # Get other dataframe (modifications and glossary)
    mod <- NULL
    Glossary <- getGlossary()
    
    # Get user inputs
    ionGroups <- input$ionGroups
    if (is.null(ionGroups)) {ionGroups <- c("a", "b", "c", "x", "y", "z", "Spec")}
    mztolppm <- input$ssTolerance
    intFilter <- input$ssIsoPerMin / 100
  
    # Run get frag pipeline and return results
    frag <- getCalcFrag(seq, ionGroups, charge)
    if (is.null(frag)) {return(NULL)}
    modMass <- addModMass(mod, frag, scanNum, seq, Glossary)
    frag <- applyMod(modMass, frag)
    frag <- getMolecularFormula(frag)
    frag <- getCalcIso(frag, intFilter)
    frag <- getAnnotatedPeaks(peak, frag, mztolppm)
    frag$error <- (frag$mzExp - frag$mz) / frag$mz * 1e6
    frag <- getCorrelationScore(frag)
    if (input$ssCorrScoreFilter != "ALL") {
      frag$corrScore <- as.numeric(frag$corrScore)
      frag <- frag[!(is.na(frag$corrScore)),]
      frag <- frag[frag$corrScore > as.numeric(input$ssCorrScoreFilter),]
    }
    
    # Convert frag to NULL if it's an empty data.frame
    if (is.null(frag) == F) {if (nrow(frag) == 0) {frag <- NULL}}
    
    return(frag)
  
  }),
  
  # Get Modification Fragment
  getModFrag <- reactive({
    
    # Get clicked value on modifications table 
    VPclick <- getVPClick()
    
    if (revals$PTMmarkdown) {
      VPclick <- revals$PTMread
    }
    
    # Get VP Metric Table
    VisPTM <- getVPMetrics()
    if (is.null(VisPTM)) {return(NULL)}
    
    # Modification name
    mod <- as.character(VisPTM[VPclick, 1])
    
    # No modifications
    if (is.na(mod) || mod == "No Modifications") {
      return(getNoModFrag())
    } else {
      
      # Get scan, peak, Glossary data
      scan <- getScan()
      peak <- getSSPeak()
      Glossary <- getGlossary()
      if (is.null(scan) || is.null(peak)) {return(NULL)}
      
      # Get clicked value from scan table
      Sclick <- getScanClick()
      scanNum <- scan[Sclick, "Scan.Num"]
      charge <- 1:scan[Sclick, "Pre.Charge"]
      seq <- getNewVSeq()
      if (is.null(seq) || is.na(seq)) {seq <- scan[Sclick, "Sequence"]}
      
      # Get Modification Data Frame
      mod <- buildModDF(scanNum, seq, mod, Glossary)
      
      # Get user inputs
      ionGroups <- input$ionGroups
      if (is.null(ionGroups)) {ionGroups <- c("a", "b", "c", "x", "y", "z", "Spec")}
      mztolppm <- input$ssTolerance
      intFilter <- input$ssIsoPerMin / 100
      
      # Run get frag pipeline and return results
      frag <- getCalcFrag(seq, ionGroups, charge)
      if (is.null(frag)) {return(NULL)}
      modMass <- addModMass(mod, frag, scanNum, seq, Glossary)
      frag <- applyMod(modMass, frag)
      frag <- getMolecularFormula(frag)
      frag <- getCalcIso(frag, intFilter)
      frag <- getAnnotatedPeaks(peak, frag, mztolppm)
      frag$error <- (frag$mzExp - frag$mz) / frag$mz * 1e6
      frag <- getCorrelationScore(frag)
      if (input$ssCorrScoreFilter != "ALL") {
        frag$corrScore <- as.numeric(frag$corrScore)
        frag <- frag[!(is.na(frag$corrScore)),]
        frag <- frag[frag$corrScore > as.numeric(input$ssCorrScoreFilter),]
      }
      
      # Convert frag to NULL if it's an empty data.frame
      if (is.null(frag) == F) {if (nrow(frag) == 0) {frag <- NULL}}

    }
    return(frag)
  }),
  
  # Get data to generate Visualize PTM error heat map
  getVPErrorHM <- reactive({
    
    # Get required data, returning NULL for missing data
    if (is.null(getModFrag()) | is.null(getModOriSDF())) {return(NULL)}
    frag <- getModFrag()
    oriSDF <- getModOriSDF()
    
    # Remove isotopes from frag if removing isotopes feature is enabled
    if (is.null(input$ssISOgraphs) == T || input$ssISOgraphs == T) {
      frag <- frag[frag$isoPeak == "M+0",]
    }
    
    # Generate heatmap-ready datatable
    SDF <- merge(oriSDF, frag, by = c("npos", "npos"), all.x = T)
    EHM <- data.frame(AA = paste(SDF$Peptide, SDF$npos, sep = ""), Type = SDF$type, 
                      IsoPeak = SDF$isoPeak, Z = SDF$z, MatchScore = SDF$error, 
                      Pos = as.numeric(SDF$npos), Let = as.character(SDF$Peptide),
                      stringsAsFactors = F)
    
    # Change type information to include ion and isotope data. Remove NA. Make factor.  
    EHM$Type <- paste(EHM$Type, "(", lapply(strsplit(as.character(EHM$Z), "[^[:digit:]]"), 
                                            function(el) el[1]) %>% unlist(), "+) ", EHM$IsoPeak, sep = "")
    EHM$Type[EHM$Type == "NA(NA+) NA"] <- NA
    EHM$Type <- as.factor(EHM$Type)
    
    return(EHM)
    
  }),
  
  # Get Visualize PTM seq flag dataframe
  getVPFlagDF <- reactive({
    
    # Get fragment data
    frag <- getModFrag()
    if (is.null(frag)) {return(NULL)}
    
    # Get ID data
    scan <- getScan()
    if (is.null(scan)) {return(NULL)}
    
    # Get user clicks on table, returning 1 if nothing is selected
    clicked <- getScanClick()
    
    # Get sequence
    seq <- revals$testSeq
    
    # Make fragment type a fragment for later analysis, and gather all information
    # to make plotting the sequence with flags easy. 
    if (length(frag$ion) == 0) {return(NULL)}
    
    # Generate Flag Dataframe
    flagData <- data.frame(as.numeric(gsub("\\D", "", frag$ion)), frag$type, frag$npos, 
                           paste(lapply(strsplit(as.character(frag$z), " "), 
                                        function(el) el[1]) %>% unlist(), "+", sep = ""), 
                           frag$error, frag$type, frag$type)
    colnames(flagData) <- c("Ion", "Type", "Position", "z", "MatchScore", "Color", "Rank")
    
    # Change frag types to colors for easy plotting later
    levels(flagData$Color)[levels(flagData$Color) == "a"] <- "forestgreen"
    levels(flagData$Color)[levels(flagData$Color) == "b"] <- "steelblue"
    levels(flagData$Color)[levels(flagData$Color) == "c"] <- "darkviolet"
    levels(flagData$Color)[levels(flagData$Color) == "x"] <- "rgb(172,122,122)"
    levels(flagData$Color)[levels(flagData$Color) == "y"] <- "red"
    levels(flagData$Color)[levels(flagData$Color) == "z"] <- "darkorange"
    
    # Add rank to flagData (1 for a,b,c and 2 for x,y,z)
    levels(flagData$Rank)[levels(flagData$Rank) %in% c("a", "b", "c")] <- 1
    levels(flagData$Rank)[levels(flagData$Rank) %in% c("x", "y", "z")] <- 2
    
    # Sort them by position
    flagData <- flagData[order(flagData$Position),]
    flagData <- data.table(flagData, key = "Position")
    
    # Split data by rank
    flagData1 <- flagData[flagData$Rank == 1,]
    flagData2 <- flagData[flagData$Rank == 2,]
    
    # Sort by best match per position, separated by rank
    flagData1 <- flagData1[, .SD[which.min(abs(MatchScore))], by = Position]
    flagData2 <- flagData2[, .SD[which.min(abs(MatchScore))], by = Position]
    
    # Put dataframes back together
    flagData <- rbind(flagData1, flagData2)
    
    # Generate coordinates for plotting amino acids
    x <- c()
    y <- c()
    seq <- as.character(seq)
    for (i in 1:nchar(seq)) {
      xtest <- i %% 15
      if (xtest != 0) {x[i] <- xtest}
      else {x[i] <- 15}
      y[i] <- -((i + 14) %/% 15)}
    allPepSDF <- data.frame(strsplit(seq, ""), x, y, 1:nchar(seq), stringsAsFactors = F)
    colnames(allPepSDF) <- c("Peptide", "X", "Y", "Position")
    allPepSDF$Mod <- ""
    
    # Add Modification Annotations (peptide database search tool identified only)
    # This step is user specified. 
    if (is.null(input$ssAnoPTM) || input$ssAnoPTM == T) {
      
      # Get scan number and use the sequence to determine if any modifications were identified
      scanNum <- scan[clicked, "Scan.Num"]
      mod <- getVPMetrics()[getVPClick(),]

      # Do not add modification annotations if there isn't any
      if (as.character(mod[,1]) != "No Modifications") {
        
        # Get modification dataframe
        mod <- buildModDF(scanNum, seq, as.character(mod[,1]), getGlossary())
        
        # If yes, apply these new modifications to the dataframe for seq flags view
        if (is.null(mod) == F) { if (nrow(mod) > 0) {
          modData <- data.frame(do.call(rbind, lapply(1:nrow(mod), function(row) {
            pos <- mod[row, "location"]
            if (pos != 0) {
              X <- allPepSDF[allPepSDF$Position == pos, "X"] + 0.2
              Y <- allPepSDF[allPepSDF$Position == pos, "Y"] 
              return(c(Peptide = "*", X = X, Y = Y, Position = pos, 
                       Mod = as.character(mod[row, "name"])))
            } else {
              return(c(Peptide = "*", X = 0.7, Y = -1, Position = 0,
                       Mod = as.character(mod[row, "name"])))
            }
          })), stringsAsFactors = F)}}
          
          # Ensure correct data types for the modifications and then bind dataframes
          modData$X <- as.numeric(modData$X)
          modData$Y <- as.numeric(modData$Y)
          modData$Position <- as.numeric(modData$Position)
          allPepSDF <- rbind(allPepSDF, modData)
          
      }
    }
    
    # Combine flag data and all the peptides xy coordinates
    flagData <- merge(allPepSDF, flagData, by = c("Position", "Position"), all = T)
    levels(flagData$Color) <- c(levels(flagData$Color), "white")
    
    # Remove fragment data for modification positions
    flagData[flagData$Peptide == "*", c(6:11)] <- NA
    
    # Make all NA data white
    flagData$Color[is.na(flagData$Color)] <- "white"
    
    # Return flag data
    return(flagData)
  }),
  
  # Get precursor isotopic distribution
  getVPPreIsoDist <- reactive({
    
    # Return null if no scan data or ID data
    if (is.null(getScan()) || is.null(getID())) {return(NULL)}
    
    # Get charge
    Pre.Charge <- getScan()[getScanClick(), "Pre.Charge"]
    if (Pre.Charge == 0) {return(NULL)}
    
    # Get sequence
    seq <- getNewVSeq()
    if (is.null(seq)) {return(NULL)}
    
    # Get scan number
    scanNum <- getScan()[getScanClick(), "Scan.Num"]
    
    # Get atoms from sequence
    if (is.na(seq) == F) { 
      seqForm <- data.frame(getAtomsFromSeq(seq))
      
      # Add modifications
      mods <- getVPMetrics()[getVPClick(), 1]
      modForm <- NULL
      if (mods != "No Modifications") {
        
        # Pull modification names
        mods <- unlist(lapply(unlist(strsplit(as.character(mods), ",")), 
           function(el) {
             unlist(strsplit(trimws(el), " "))[c(FALSE, TRUE)]
           }
        ))
        
        # Pull modification formula
        modForm <- do.call(bind_rows, lapply(mods, function(mod) {
          data.frame(stringToList(paste(getGlossary()[getGlossary()$Interim.Name == mod, "Molecular.Formula"])))
        }))
      }  
      
      # Modify modification formula if it has a "." in its name for the modifications were elements are removed
      colnames(modForm) <- unlist(lapply(colnames(modForm), function(element) {
        if (grepl(".", element, fixed = T)) {gsub(".", "", element, fixed = T)} else {element}
      }))
      
      # Add formulas together and converted to a string for RDisop
      completeForm <- bind_rows(seqForm, modForm) 
      completeForm <- paste(lapply(1:ncol(completeForm), function(col) {
        if (sum(completeForm[,col], na.rm = T) != 0) {
          paste(names(completeForm)[col], sum(completeForm[,col], na.rm = T), sep = "")
        } else {""}
      }), collapse = "")
      
      # Get isotopic distribution
      isoDist <- data.frame(t(getMolecule(completeForm)$isotopes[[1]]))
      colnames(isoDist) <- c("mz", "Ref.Int")
      isoDist$mz <- (isoDist$mz + (Pre.Charge * 1.003)) / Pre.Charge
    } else {isoDist <- data.frame("mz" = NA, "Ref.Int" = NA)}
    return(isoDist)
    
  }), 
  
  # Get Matched Precursor Data for Previous
  getVPMatchedPrevious <- reactive({
    
    # Return null if no scan data
    if (is.null(getScan())) {return(NULL)}
    
    # If scan is a MS1, return null
    MS.Level <- getScan()[getScanClick(), "MS.Level"]
    if (MS.Level == 1) {return(NULL)}
    
    # If no window, return null
    window <- getPrecursorWindow()
    if (is.null(window)) {return(NULL)}
    
    # Get scan number, peak information based on window size, and pre mz
    scanNum <- getScan()[getScanClick(), "Scan.Num"]
    PreScanNum <- getScan()[getScanClick(), "Pre.Scan"]
    if (is.na(PreScanNum) || PreScanNum == 0) {return(NULL)}
    Pre.MZ <- getScan()[getScanClick(), "Pre.MZ"]
    
    # Get range
    MatchedPre <- peakDataRange(PreScanNum, Pre.MZ, window)
    if (is.null(MatchedPre) || nrow(MatchedPre) == 0) {return(NULL)}
    
    # Get precursor isotope distribution
    isoDist <- getVPPreIsoDist()
    
    # If precursor isotope distribution is null, fill the new columns with NA values
    if (is.null(isoDist)) {
      MatchedPre <- data.frame("mz" = MatchedPre$mz, "int" = MatchedPre$int, "Isotopes" = NA,
                               "Ref.Int" = NA, "Exp.Int" = NA, "Perc.Diff" = NA)
    } else {
      
      # Create trunc function for third decimal place
      trunc2 <- function(x) {trunc(x * 100)/100}
      
      # Fill out MatchedPre with isotope data
      newCols <- c("Isotopes", "Ref.Int")
      MatchedPre[newCols] <- NA
      MatchedPre[newCols] <- data.frame(do.call(rbind, lapply(1:nrow(MatchedPre), function(row) {
        if (trunc2(MatchedPre[row, "mz"]) %in% trunc2(isoDist$mz)) {
          return(c(paste("M+", which(trunc2(MatchedPre[row, "mz"]) == trunc2(isoDist$mz)) - 1, sep = ""), 
                   round(isoDist[which(trunc2(MatchedPre[row, "mz"]) == trunc2(isoDist$mz)), "Ref.Int"], 4)))
        } else {return(c(NA, 0))}
      })))
      MatchedPre$Ref.Int <- as.numeric(as.character(MatchedPre$Ref.Int))
      
      # Calculate Expected Intentisties and Percent Differences 
      IntTotalDF <- MatchedPre[MatchedPre$int != 0 & MatchedPre$Ref.Int != 0,]
      MaxInt <- IntTotalDF[which.max(IntTotalDF$Ref.Int),]
      IntTotal <- MaxInt$int / MaxInt$Ref.Int
      if (length(IntTotal) == 0) {IntTotal <- 1}
      MatchedPre$Exp.Int <- MatchedPre$Ref.Int * IntTotal
      MatchedPre$Perc.Diff <- (MatchedPre$int - MatchedPre$Exp.Int) / MatchedPre$Exp.Int
      
      # Remove metadata for placeholder values
      MatchedPre[MatchedPre$int == 0, "Ref.Int"] <- 0
      MatchedPre[MatchedPre$Ref.Int == 0, "Perc.Diff"] <- NA
    }
    
    return(MatchedPre)
  }),
  
  # Get Matched Precursor Data for Next
  getVPMatchedNext <- reactive({
    
    # If no scan data, return null
    if (is.null(getScan())) {return(NULL)}
    
    # If scan is a MS1, return null
    MS.Level <- getScan()[getScanClick(), "MS.Level"]
    if (MS.Level == 1) {return(NULL)}
    
    # If no window, return null
    window <- getPrecursorWindow()
    if (is.null(window)) {return(NULL)}
    
    # Get all Ms1 scans and if none, return null
    AllMs1 <- getAllMs1()
    if (is.null(AllMs1)) {return(NULL)}
    
    # Get scan number, peak information based on window size, and next mz
    scanNum <- getScan()[getScanClick(), "Scan.Num"]  
    NextScanNumPos <- match(getScan()[getScanClick(), "Pre.Scan"], getAllMs1()) + 1
    NextScanNum <- getAllMs1()[NextScanNumPos]
    if (is.na(NextScanNum) || NextScanNum == 0) {return(NULL)}
    Pre.MZ <- getScan()[getScanClick(), "Pre.MZ"]
    
    # Get range
    MatchedPre <- peakDataRange(NextScanNum, Pre.MZ, window)
    if (is.null(MatchedPre) || nrow(MatchedPre) == 0) {return(NULL)}
    
    # Get precursor isotope distribution
    isoDist <- getVPPreIsoDist()
    
    # If precursor isotope distribution is null, fill the new columns with NA values
    if (is.null(isoDist)) {
      MatchedPre <- data.frame("mz" = MatchedPre$mz, "int" = MatchedPre$int, "Isotopes" = NA,
                               "Ref.Int" = NA, "Exp.Int" = NA, "Perc.Diff" = NA)
    } else {
      
      # Create trunc function for third decimal place
      trunc2 <- function(x) {trunc(x * 100)/100}
      
      # Fill out MatchedPre with isotope data
      newCols <- c("Isotopes", "Ref.Int")
      MatchedPre[newCols] <- NA
      MatchedPre[newCols] <- data.frame(do.call(rbind, lapply(1:nrow(MatchedPre), function(row) {
        if (trunc2(MatchedPre[row, "mz"]) %in% trunc2(isoDist$mz)) {
          return(c(paste("M+", which(trunc2(MatchedPre[row, "mz"]) == trunc2(isoDist$mz)) - 1, sep = ""), 
                   round(isoDist[which(trunc2(MatchedPre[row, "mz"]) == trunc2(isoDist$mz)), "Ref.Int"], 4)))
        } else {return(c(NA, 0))}
      })))
      MatchedPre$Ref.Int <- as.numeric(as.character(MatchedPre$Ref.Int))
      
      # Calculate Expected Intentisties and Percent Differences 
      IntTotalDF <- MatchedPre[MatchedPre$int != 0 & MatchedPre$Ref.Int != 0,]
      MaxInt <- IntTotalDF[which.max(IntTotalDF$Ref.Int),]
      IntTotal <- MaxInt$int / MaxInt$Ref.Int
      if (length(IntTotal) == 0) {IntTotal <- 1}
      MatchedPre$Exp.Int <- MatchedPre$Ref.Int * IntTotal
      MatchedPre$Perc.Diff <- (MatchedPre$int - MatchedPre$Exp.Int) / MatchedPre$Exp.Int
      
      # Remove metadata for placeholder values
      MatchedPre[MatchedPre$int == 0, "Ref.Int"] <- 0
      MatchedPre[MatchedPre$Ref.Int == 0, "Perc.Diff"] <- NA
    }
    
    return(MatchedPre)
    
  }),
  
  # Clear the output of the Vis PTM metric table if sequence, ionGroups, tolerance,
  # isotopic percentage, and/or including isotopes are changed
  observeEvent(input$VPSequence, {revals$PTMdf <- NULL}),
  observeEvent(input$ionGroups, {revals$PTMdf <- NULL}),
  observeEvent(input$ssIntenMind, {revals$PTMdf <- NULL}),
  observeEvent(input$ssTolerance, {revals$PTMdf <- NULL}),
  observeEvent(input$ssIsoPerMin, {revals$PTMdf <- NULL}),
  observeEvent(input$ssCorrScoreFilter, {revals$PTMdf <- NULL})
  
)