## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_12_29

# DESCRIPTION: This contains the get functions which are directly related to the ID file:
# getIsoPerMin, getIsoPeaks, getID, getModDF, getModPerSeq, getFrag, getOriSDF,
# getFlagDF, getAllFrag, getErrorHM, getPTscan, getPTID

list(
  
  # Set observer to initializing testSeq
  observe({
    if (is.null(msPath()) == F && is.null(idPath()) == F) {
      revals$testSeq <- getScan()[getScanClick(), "Sequence"]
    }
  }),
  
  # Set testSeq to whatever the current sequence is
  observeEvent(input$ssScan_row_last_clicked, {
    revals$testSeq <- getScan()[getScanClick(), "Sequence"]
  }), 
  
  # Get New Sequence if it has been inputted correctly
  observeEvent(input$ssASeq, {
    Nseq <- input$ssNewSeq
    if (is.null(Nseq)) {revals$testSeq <- NULL}
    if (grepl("[[:space:]]", Nseq)) {revals$testSeq <- NULL} else
    if (grepl("\\[", Nseq) | grepl("\\]", Nseq)) {revals$testSeq <- NULL} else
    if (grepl("[^aA-zZ]", Nseq)) {revals$testSeq <- NULL} else
    if (nchar(Nseq) < 2) {revals$testSeq <- NULL} else
    if (grepl("[BbJjOoUuXxZz]", Nseq)) {revals$testSeq <- NULL} else
          {revals$testSeq <- toupper(Nseq)}
  }),
  
  # Adjust Isotopic Percentage Minimum if Bad Input
  getIsoPerMin <- reactive({
    IPM <- input$ssIsoPerMin
    if (is.null(IPM)) {IPM <- 10}
    if (IPM < 0) {IPM <- 0}
    if (IPM > 100) {IPM <- 100}
    return(IPM / 100)
  }),
  
  # Get the number of isotopic peaks to search - DELETE THIS
  getIsoPeaks <- function(IsoDist, MatchPos, isolate) {
    First <- Second <- Third <- Fourth <- Fifth <- NA
    if (length(MatchPos) > 0) {
      First <- isolate[MatchPos,]
      if (IsoDist > 1) {Second <- isolate[MatchPos + 1,]}
      if (IsoDist > 2 & is.na(Second)[1] == F) {
        Lower <- ((Second$mz - First$mz) * 0.5) + Second$mz
        Upper <- ((Second$mz - First$mz) * 1.5) + Second$mz
        Third <- isolate[isolate$mz >= Lower & isolate$mz <= Upper,]
        Third <- isolate[which.max(isolate$intensity),]}
      if (IsoDist > 3 & is.na(Third)[1] == F) {
        Lower <- ((Third$mz - Second$mz) * 0.5) + Third$mz
        Upper <- ((Third$mz - Second$mz) * 1.5) + Third$mz
        Fourth <- isolate[isolate$mz >= Lower & isolate$mz <= Upper,]
        Fourth <- isolate[which.max(isolate$intensity),]}
      if (IsoDist > 4 & is.na(Fourth)[1] == F) {
        Lower <- ((Fourth$mz - Third$mz) * 0.5) + Fourth$mz
        Upper <- ((Fourth$mz - Third$mz) * 1.5) + Fourth$mz
        Fifth <- isolate[isolate$mz >= Lower & isolate$mz <= Upper,]
        Fifth <- isolate[which.max(isolate$intensity),]}
    }
    return(list(First = First, Second = Second, Third = Third, Fourth = Fourth, 
                Fifth = Fifth))
  },
  
  # Get data from identified protein file (ID from mzid)
  getID <- reactive({
    
    # Return null if no ID data nor file type
    if (is.null(idPath()) | is.null(getFileType())) {return(NULL)}
    
    # If MS Path Finder is used, these file types will be needed
    PS <- gsub(".mzid", "_MSPathFinder_PSMS.mzid", idPath(), fixed = T)
    SC <- gsub(".mzid", "_MSPathFinder_Score.csv", idPath(), fixed = T)
    
    # MS Path Finder has accession numbers in its ID file that can't be read by mzR 
    tryCatch({
      
      id <- openIDfile(idPath())}, 
      
      error = function(e) {
        
        # Get the CV Number of the error
        cvNum <- as.numeric(gsub("\\D", "", as.character(e[1])))
        
        # If the error is due to MS Path Finder, generate ID file
        if (is.na(cvNum) == F && cvNum >= 1002720 & cvNum <= 1002725) {
          
          # Check if the required files exist, and if not, make them
          if (file.exists(PS) == F | file.exists(SC) == F) {
          
            # Read the file as a textID file
            suppressWarnings({textID <- readLines(idPath())})
            
            # Make PS: Find rows with inappropriate accession numbers and remove them
            psFile <- textID[-grep("1002720|1002725|1002721|1002722|1002723|1002724", textID)]
            writeLines(psFile, PS)
            
            # Make SC: Get scan number, specEValue, and QValue from file
            SN <- EV <- QV <- c()
            
            # Make SC: Go through the file and collect the SpecEValue and the regular Q Value
            for (line in textID) {
              if (grepl("scan=", line)) {
                splitLine <- unlist(strsplit(line, " |=|\""))
                SN <- c(SN, as.numeric(splitLine[grep("scan", splitLine) + 1]))
              } else if (grepl("SpecEValue", line)) {
                splitLine <- unlist(strsplit(line, " |=|\""))
                EV <- c(EV, as.numeric(splitLine[grep("value", splitLine) + 2]))
              } else if (grepl(":QValue", line)) {
                splitLine <- unlist(strsplit(line, " |=|\""))
                QV <- c(QV, as.numeric(splitLine[grep("value", splitLine) + 2]))
              }
            }
            
            # Make SC: Write the file
            score <- data.frame(cbind(SN, EV, QV))
            if (is.null(EV) & is.null(QV)) {colnames(scan) <- c("Scan")} else
            if (is.null(QV)) {colnames(score) <- c("Scan", "MS.GF.SpecEValue")} else
            if (is.null(EV)) {colnames(score) <- c("Scan", "MS.GF.QValue")} else 
              {colnames(score) <- c("Scan", "MS.GF.SpecEValue", "MS.GF.QValue")}
            write.csv(score, SC, row.names = F)
          } 
          
        }
    })
    
    # If files exist, read them. 
    if (file.exists(PS)) {ps <- mzR::psms(openIDfile(PS))} else {ps <- mzR::psms(openIDfile(idPath()))}
    if (file.exists(SC)) {sc <- read.csv(SC)} else {sc <- mzR::score(openIDfile(idPath()))}
    
    # Some files have messed up retention times, so this tries to standardize all inputs
    scanStart <- ps$scan.start.time
    if (is.null(scanStart)) {
      if (getFileType() == "mzms") {scanStart <- round(getHeader()[ps$scan.number.s.,]$retentionTime / 60, 3)} else
      if (getFileType() == "raw") {scanStart <- round(getRAWScanSM()[ps$scan.number.s.,]$StartTime, 3)}} else 
      if (getFileType() == "h5") {scanStart <- round(getH5scan()[ps$scan.number.s.,]$retentionTime, 3)} else {
        scanStart <- round(as.numeric(as.character(ps$scan.start.time)), 3)}
    
    # Not all files have QValues, so this makes up for that 
    QVal <- sc$MS.GF.QValue
    if (is.null(QVal) == F) {QVal <- round(QVal, 2)} else {QVal <- NA}
    
    ID <- data.frame(seq(1, nrow(ps)), ps$acquisitionNum, 
                     scanStart, ps$sequence, ps$DatabaseAccess, 
                     round(mw(ps$sequence, monoisotopic = T), 3), ps$chargeState, 
                     sc$MS.GF.SpecEValue, 
                     QVal, ps$isDecoy, ps$DatabaseDescription,
                     ps$start)
    colnames(ID) <- c("Order", "Scan", "RT", "Sequence", 
                      "Protein", "Mass", "Charge", "Score", "QVal",
                      "isDecoy", "Description", "Start")
    return(ID)
  }),
  
  # Get the abbreviated names and experimental weight differences of modifications
  getModDF <- reactive({
    
    # Get Raw ID
    if (is.null(idPath())) {return(NULL)}
    
    # Try catch opening the ID file, if not, try PS ID file
    id <- tryCatch({
      
      openIDfile(idPath())}, 
      
      error = function(e) { 
        PS <- unlist(strsplit(idPath(), "/"))
        PS[length(PS)] <- paste(gsub(".mzid", "", tail(PS, 1)), "_MSPathFinder_PSMS.mzid", sep = "")
        openIDfile(paste(PS, collapse = "/"))
    })
    
    # Generate modifications dataframe
    mod <- data.frame(mzR::modifications(id))
    
    # If no modification data, then return null
    if (nrow(mod) > 0) {
      mod$spectrumID <- as.numeric(lapply(strsplit(as.character(mod$spectrumID), "scan=|index="), 
                                          function(el) {return(el[2])}))
      
      # Account for the location from the C-terminus (for position of XYZ fragments)
      mod$cpos <- lapply(1:nrow(mod), function(row) {
        nchar(as.character(mod$sequence[row])) - mod$location[row] + 1}) %>% unlist()
      
      # Add sequence length
      mod$length <- nchar(as.character(mod$sequence))
      
      return(mod)} else {return(NULL)}
  }),
  
  # Get modifications for only the clicked sequence
  getModPerSeq <- reactive({
    
    # Get required information
    mod <- getModDF()
    scan <- getScan()
    if (is.null(mod) | is.null(scan)) {return(NULL)}
    
    # Get scan number and sequence
    clicked <- getScanClick()
    scanNum <- scan[clicked, 2]
    seq <- as.character(scan[clicked, 12])
    
    # Determine if the scan/sequence match has modifications
    return(mod[mod$spectrumID == scanNum & mod$sequence == seq,])
  }),
  
  # Get BRAIN isotopes, adjusting for target ion type. Used in getFrag. 
  adjustForFragType <- function(type, seq) {
    at <- getAtomsFromSeq(seq)
    if (type == "a") {at$C <- at$C - 1; at$H <- at$H - 1; at$O <- at$O - 2} else
    if (type == "b") {at$H <- at$H - 1; at$O <- at$O - 1} else
    if (type == "c") {at$H <- at$H + 2; at$N <- at$N + 1; at$O <- at$O - 1} else
    if (type == "x") {at$C <- at$C + 1; at$H <- at$H - 1; at$O <- at$O + 1} else
    if (type == "y") {at$H <- at$H + 1} else
    if (type == "z") {at$H <- at$H - 2; at$N <- at$N - 1} 
    return(at)
  },
  
  # Get fragment data (a,b,c,x,y,z ions) for plotting purposes
  getFrag <- reactive({
    
    # Source the getFrag functions
    source(file.path("Server", "Get", "Get_Frag.R"), local = T)$value
    
    # Return NULL if no scan or peak data
    if (is.null(getScan()) | is.null(getSSPeak())) {return(NULL)}

    # Get all the required data: Scan (seq, charge, scanNum), ionGroups,
    # modification, Glossary, peak data, mztolppm, % intensity filter
    peak <- getSSPeak()
    
    # Scan data 
    scanNum <- getScan()[getScanClick(), "Scan.Num"]
    charge <- 1:getScan()[getScanClick(), "Pre.Charge"]
    seq <- revals$testSeq
 
    # Get other dataframe (modifications and glossary)
    mod <- getModDF()
    Glossary <- getGlossary()
    
    # Get user inputs
    ionGroups <- input$ionGroups
    mztolppm <- getTol()
    isoFilter <- input$ssIsoPerMin / 100
    corrScore <- input$ssCorrScoreFilter
    
    # 1. Get Calc Frag
    if (is.null(seq) || is.na(seq)) {return(NULL)}
    frag <- getCalcFrag(seq, ionGroups, charge)
    if (is.null(frag)) {return(NULL)}
    
    # 2. Add Mod Mass
    modMass <- addModMass(mod, frag, scanNum, seq, Glossary)
    
    # 3. Apply Mod
    frag <- applyMod(modMass, frag)
    
    # 4. Get Molecular Formula
    frag <- getMolecularFormula(frag)
    
    # 5. Get Calculated Isotope Values
    frag <- getCalcIso(frag, isoFilter)
    if (is.null(frag)) {return(NULL)}
    
    # 6. Get Annotated Peaks
    frag <- getAnnotatedPeaks(peak, frag, mztolppm)
    if (is.null(frag) || nrow(frag) == 0) {return(NULL)}
    frag$error <- (frag$mzExp - frag$mz) / frag$mz * 1e6
    
    # 7. Add Correlation Score
    frag <- getCorrelationScore(frag)
    
    # Filter out frag data by correlation
    if (input$ssCorrScoreFilter != "ALL") {
      frag$corrScore <- as.numeric(frag$corrScore)
      frag <- frag[!(is.na(frag$corrScore)),]
      frag <- frag[frag$corrScore > as.numeric(input$ssCorrScoreFilter),]
    }
    
    # Convert frag to NULL if it's an empty data.frame
    if (is.null(frag) == F) {if (nrow(frag) == 0) {frag <- NULL}}
    
    # Remove isotopes from frag if remove isotopes from spectra is enabled
    if (is.null(input$ssISOspectra) == F && input$ssISOspectra == T) {frag <- frag[frag$isoPeak == "M+0",]}
    
    return(frag)
  }),
  
  # Get ORIginal Sequence DataFrame, which consists of x, y coordinates of each
  # amino acid for the sequence graph. This is a graph specific getter.
  getOriSDF <- reactive({
    
    # Get clicked sequence
    clicked <- getScanClick()
    scan <- getScan()
    seq <- revals$testSeq
    
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
      
      # Add modification data
      if (is.null(getModPerSeq()) == F) { if (nrow(getModPerSeq()) > 0) {
       
       # Get the modification data 
       seqMods <- getModPerSeq()
       
       # For each row mod data, get AA position and add an asterisk next to it
       oriSDF <- rbind(oriSDF, do.call(rbind, lapply(1:nrow(seqMods), function(rowNum) {
         
         # If the position is zero, place at the top. Otherwise, add to the correct location.
         AApos <- oriSDF[seqMods[rowNum,]$location,]
         if (nrow(AApos) == 0) {
           return(c(Peptide = "*", X = 1, Y = -1.5, npos = 0, 
                    Mod = paste(seqMods[rowNum,]$name, seqMods[rowNum,]$mass)))
         } else {
           return(c(Peptide = "*", X = AApos$X, Y = AApos$Y - 0.5, npos = AApos$npos,
                    Mod = paste(seqMods[rowNum,]$name, seqMods[rowNum,]$mass)))
         }
       })))
        
      }}
      return(oriSDF)
    }}
    
    return(NULL)
  }),
  
  # Get dataframe for sequence view with best fit annotated by flags 
  getFlagDF <- reactive({
    
    # Get fragment data
    frag <- getFrag()
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
      scanNum <- scan[getScanClick(), "Scan.Num"]
      mod <- getModDF()
      mod <- mod[mod$spectrumID == scanNum & mod$sequence == seq,]
      
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
            return(c(Peptide = "*", X = 0.9, Y = -1, Position = 0,
                     Mod = as.character(mod[row, "name"])))
          }
        })), stringsAsFactors = F)
        
        # Ensure correct data types for the modifications and then bind dataframes
        modData$X <- as.numeric(modData$X)
        modData$Y <- as.numeric(modData$Y)
        modData$Position <- as.numeric(modData$Position)
        allPepSDF <- rbind(allPepSDF, modData)
      }}
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
  
  # Get All Fragment Data for its datatable
  getAllFrag <- reactive({
    
    # Get frag data and original sequence data frame
    frag <- getFrag()
    if (is.null(frag)) {return(NULL)}
    oriSDF <- getOriSDF()
    
    # Remove the modification symbol
    oriSDF <- oriSDF[!oriSDF$Peptide == "*",]
   
    # Generate SDF and keep only the important information: peptide, ion, z, and isoPeak
    SDF <- merge(oriSDF, frag, by = c("npos", "npos"), all.x = T)
    SDF <- data.frame(NPos = SDF$npos, Peptide = paste(SDF$Peptide, SDF$npos, sep = ""), 
            IonNames = paste(SDF$ion, " ", SDF$z, "+ ", SDF$isoPeak, sep = ""))
    AllFrag <- aggregate(SDF, by = list(SDF$NPos), paste)
    AllFrag$IonNames[AllFrag$IonNames == "NA NA+ NA"] <- ""
    
    # Simplify AllFrag dataframe for easier intepretability
    AllFrag <- data.frame(Peptide = unlist(lapply(AllFrag$Peptide, function(Pep) 
                {tail(unlist(strsplit(Pep, ",")), 1)})), Number.Of.Ions = unlist(lapply(AllFrag$IonNames, 
                function(el) {if (el[1] != "") {return(length(strsplit(el, ",")))} else 
                  {return(0)}})), Ion.Names = unlist(lapply(AllFrag$IonNames, 
                function(el) {return(paste(el, collapse = "<br/>"))})))
    AllFrag <- data.frame(Order = 1:nrow(AllFrag), Peptide = AllFrag$Peptide,
                 Number.Of.Ions = AllFrag$Number.Of.Ions, Ion.Names = AllFrag$Ion.Names)
    
    return(AllFrag)
    
  }),
  
  # Get data to generate error heat map, the ppm error for each detected fragment
  getErrorHM <- reactive({
    
    # Get required data, returning NULL for missing data
    if (is.null(getFrag()) | is.null(getOriSDF())) {return(NULL)}
    frag <- getFrag()
    oriSDF <- getOriSDF()

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
  
  ##################
  ## PROTEIN TREE ##
  ##################
  
  # Get PT Scan Data
  getPTscan <- reactive({
    
    req(getID(), getScan())
    scan <- getScan()
    
    # Create a scan dataframe for protein tree
    PTscan <- data.frame(scan$Protein.ID, scan$Description, scan$Sequence,
                         scan$isDecoy, scan$Q.Value, scan$Start, scan$Scan.Num)
    colnames(PTscan) <- c("Protein.ID", "Description", "Sequence", "isDecoy", 
                          "Q.Value", "Start", "Scan.Num")
    
    # Remove decoys, optionally remove contaminants, and apply threshold
    PTscan <- PTscan[PTscan$isDecoy == F,]
    if (is.null(input$PTCont) == F && input$PTCont == T) {PTscan <- PTscan[grepl("Contaminant", PTscan$Description) == F,]}
  
    # Determine if there are only NA values for Q Value 
    if (FALSE %in% unique(is.na(PTscan$Q.Value))) {
      tol <- input$PTTolerance
      if (is.null(tol) || is.na(tol)) {tol <- 0}
      PTscan <- PTscan[PTscan$Q.Value <= abs(tol),]
    }
    PTscan <- PTscan[!is.na(PTscan$Protein.ID),] 
    return(PTscan)
    
  }),
  
  # Get a specific ID object for protein tree plots
  getPTID <- reactive({
    
    # Require the necessary data
    ID <- getID()
    PTscan <- getPTscan()
    if (is.null(ID) | is.null(PTscan)) {return(NULL)}
    
    # Create PTID data which summarizes PTscan
    PTID <- data.frame(summary(PTscan$Protein.ID, maxsum = 1e8))
    PTID <- data.frame(rownames(PTID), PTID)
    colnames(PTID) <- c("Protein.ID", "Number.Of.Peptides")
    PTID <- PTID[PTID$Number.Of.Peptides > 0,]
    PTID <- unique(PTID[order(PTID$Number.Of.Peptides, decreasing = T),])
    Desc <- unique(data.frame(PTscan$Protein.ID, PTscan$Description))
    colnames(Desc) <- c("Protein.ID", "Description")
    PTID <- merge(PTID, Desc, by = "Protein.ID")
    colnames(PTID) <- c("Protein", "Number.Of.Peptides", "Description")
    PTID <- PTID[order(-PTID$Number.Of.Peptides),]
    return(PTID)
  })
  
)