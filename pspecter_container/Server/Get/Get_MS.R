## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2021_01_08

# DESCRIPTION: This contains the get functions which are directly related to the MS file:
# getRAW, getRAWall, getRAWScanSM, getMZMS, getHeader, getSSPeak, getXIC, 
# peakDataRange, getPreIsoDist, getMatchedPrevious, getAllMs1, and getMatchedNext. 

list(
  
  # Generate raw object with restricted variables with consistent names
  getRAW <- reactive({
    ifelse(is.null(msPath()), return(NULL), return(read.raw(msPath(), rawDiag = T)))
  }),
  
  # Generate raw object with all variables 
  getRAWall <- reactive({
    ifelse(is.null(msPath()), return(NULL), return(read.raw(msPath(), rawDiag = F)))
  }),
  
  # Get RAW file metadata, similar to header for XML-based files
  getRAWScanSM <- reactive({
    
    # Prevent further action if no raw file is uploaded
    if (is.null(getRAW())) {return(NULL)}
    RAW <- getRAW()
    
    # Change MS levels to numeric, remove mass values for Ms1, and change scan NA to 0
    RAW$MS_Level <- as.numeric(as.factor(RAW$MSOrder))
    RAW$PrecursorMass[is.na(RAW$PrecursorMass)] <- 0
    RAW$MasterScanNumber[is.na(RAW$MasterScanNumber)] <- 0
    
    # Gather scan data
    scan <- data.frame(RAW$scanNumber, RAW$MS_Level, round(RAW$StartTime, 3), 
                       RAW$PrecursorMass, RAW$ChargeState, RAW$MasterScanNumber, RAW$ScanType)
    colnames(scan) <-  c("acquisitionNum", "msLevel", "retentionTime", "precursorMZ", 
                         "precursorCharge", "precursorScanNum", "filterString") 

    return(scan)
    
  }),
  
  # Generate mzms object (XML equivalent to RAW object)
  getMZMS <- reactive({
    if (is.null(msPath())) {return(NULL)}
    if (is.null(getFileType())) {return(NULL)}
    if (getFileType() == "mzms") {return(openMSfile(msPath(), backend = "pwiz"))} else
    {return(NULL)}
  }),
  
  # Get metadata from mzms object (XML equivalent to RAWScanSM)
  getHeader <- reactive({
    ifelse(is.null(getMZMS()), return(NULL), return(header(getMZMS())))
  }),
  
  # Get h5 file metadata
  getH5scan <- reactive({
    
    # Return null if no msPath
    if (is.null(msPath())) {return(NULL)}
    
    # Pull and organize scan metadata
    scan <- data.frame(h5read(msPath(), "Metadata"))
    
    # Subset required columns and rename 
    scan <- scan[,c("Scan", "MSLevel", "RetentionTime", "IsolationWindowTargetMz", 
            "PrecursorCharge", "PrecursorScan", "Fragmentation")]
    colnames(scan) <- c("acquisitionNum", "msLevel", "retentionTime", "precursorMZ", 
           "precursorCharge", "precursorScanNum", "filterString")
    
    # Return scan data
    return(scan)
    
  }),
  
  # Get peak data for selected spectrum (intensity vs mz)
  getSSPeak <- reactive({
    
    # Return NULL if no scan data
    if (is.null(getScan())) {return(NULL)}
    
    # Get minimum intensity and scan number
    intenMin <- getIntenMin()
    scanNum <- getScan()[getScanClick(), "Scan.Num"]
    
    # Get peak values if the file is mzms
    if (getFileType() == "mzms") {
      
      mzms <- getMZMS()
      if (is.null(mzms)) {return(NULL)}
      
      # The mzR package reads entry number and not scan
      entryNum <- match(scanNum, unique(getScan()$Scan.Num[order(getScan()$Scan.Num)]))
      peak <- data.frame(peaks(mzms, scan = entryNum))
      
    } else
        
    # Get peak values if the file is raw 
    if (getFileType() == "raw") {
      
      scans <- readScans(msPath(), scans = scanNum)
      peak <- data.frame(scans[[1]]$mZ, scans[[1]]$intensity)
      
    } else 
      
    # Get peak data values if the file is h5
    if (getFileType() == "h5") {
      peak <- data.frame(h5read(msPath(), paste("Spectra_mz_arrays/", scanNum, sep = "")),
                         h5read(msPath(), paste("Spectra_intensity_arrays/", scanNum, sep = "")))
    }
    
    # Return null if no peaks
    if (is.null(peak) || nrow(peak) < 1) {
      sendSweetAlert(session, "No Peaks Reported", type = "error")
      return(NULL)
    }
    
    # Apply the intensity minimum filter
    colnames(peak) <- c("mz", "intensity")
    peak <- peak[peak$intensity > 0,]
    peak <- subset(peak, peak$intensity >= intenMin)
  
    return(peak)
    
  }),
  
  # Get XICs for h5 files
  getExtractedIonChromatograms <- function(h5file, mzMin, mzMax, metadata, xics_mz) {
    
    # Create a matrix with:
    #   columns corresponds to each m/z (for example 4 if there are 4 isotopic peaks)
    #   rows correspond to the intensity for each scan
    #   a first extract column for RT
    xics <- matrix(data = 0, nrow = length(metadata[,1]), ncol = (length(mzMin) + 1))
    
    for (k in 1:length(mzMin)) {  
      
      indices <- which(xics_mz$value >= mzMin[k] & xics_mz$value <= mzMax[k])
      
      for (i in indices) {
        
        scan_array <- h5read(h5file, compoundAsDataFrame = FALSE, 
                            paste("/Extracted_ion_chromatograms/scan_arrays/", 
                                  format(round(xics_mz$value[i], 2), nsmall = 2), sep = ''))
        
        intensity_array <- h5read(h5file, compoundAsDataFrame = FALSE,
                                 paste("/Extracted_ion_chromatograms/intensity_arrays/", 
                                       format(round(xics_mz$value[i], 2), nsmall = 2), sep = '')) 
        
       for (z in 1:length(scan_array)) {
          xics[scan_array[z], (k+1)] <- xics[scan_array[z], (k+1)] + intensity_array[z]
        }
        
      }
    }
    
    # Add RT to first column
    xics[,1] <- metadata$RetentionTime
    
    # Return XICs
    return(xics)
  
  },
  
  # Get XIC Object
  getXIC <- reactive({
    
    # Return null if no scan data or file type
    if (is.null(getScan()) || getFileType() == "mzms") {return(NULL)}
    
    # If MS Level is 1, return NULL
    if (getScan()[getScanClick(), "MS.Level"] == 1) {return(NULL)}
    
    # Set Color Value
    colorList <- c("rgb(204,000,000)", "rgb(204,102,000)", "rgb(204,204,000)", 
                   "rgb(000,204,000)", "rgb(000,102,000)", "rgb(000,204,204)", 
                   "rgb(000,102,204)", "rgb(000,000,204)", "rgb(102,000,204",
                   "rgb(204,000,204)", "rgb(204,000,102)", "rgb(128,128,128)",
                   "rgb(000,000,000)")
    
    # Get Precursor MZ and Charge Values
    preMZ <- input$premzXIC
    preCh <- input$prechXIC
    
    # Return NULL if nothing exists yet
    if (is.null(preMZ) || is.na(preMZ)) {return(NULL)}
    if (is.null(preCh) || is.na(preCh)) {return(NULL)}
    
    # Check if the values are equal to 0. If so, use the precursor mz/charge value 
    # in the scan metadata table. If it's still zero, return NULL.
    if (preMZ == 0) {preMZ <- getScan()[getScanClick(), "Pre.MZ"]}
    if (preMZ == 0) {return(NULL)}
    if (preCh == 0) {preCh <- getScan()[getScanClick(), "Pre.Charge"]}
    if (preCh == 0) {return(NULL)}
  
    # If there are no isotopes selected, then return the preMZ
    if (length(input$isoXIC) < 1) {calcIsotopes <- preMZ} else {
      
      # Else, determine the MZ values first by calculating isotopes
      calcIsotopes <- unlist(lapply(as.numeric(input$isoXIC), function(isoXIC) {
        preMZ + (isoXIC / preCh)}))
      
    }
    
    # If there are no charge states selected, then just return the calcIsotopes
    if (length(input$chargeTraceXIC) < 1) {adjMZ <- calcIsotopes} else { 
      
      # Then divide by charge states to get adjusted MZ values
      adjMZ <- unlist(lapply(calcIsotopes, function(isotopes) {
        isotopes / as.numeric(input$chargeTraceXIC)
      }))
      
    }
    
    # Create function to return RT that need 0's 
    gen0 <- function(trace) {
      return(unlist(lapply(0:(round(max(trace$rt))) + 1, function(rt) {
        if (rt %in% trunc(trace$rt) == F) {rt}
      })))
    }
    
    # Do the following if the file is raw 
    if (getFileType() == "raw") {
      
      # Calculate the XIC
      XIC <- readXICs(msPath(), adjMZ, tol = input$tolXIC)
      
      # Create labels vector
      labels <- expand.grid(paste0("[M+", input$isoXIC, "]<sup>+"), 
                            paste0(input$chargeTraceXIC, "</sup>"))
      labels <- paste0(labels$Var1, labels$Var2)
      labels <- sort(labels)
      
      # Pull Retention Time and Intensity from XICs
      AllTraces <- do.call(rbind, lapply(1:length(XIC), function(idx) {
      
        # Get retention time and intensity
        rt <- XIC[[idx]]$times
        int <- XIC[[idx]]$intensities
        
        # Return NULL if no RT nor INT
        if (is.null(rt) || is.null(int)) {return(NULL)}
        
        # Create col index
        colIdx <- idx
        if (colIdx > length(colorList)) {coldIdx <- length(colorList)}
        
        return(data.frame(
          "mz" = adjMZ[idx], rt, int, "lab" = labels[idx], "color" = colorList[colIdx]
        ))
        
      }))
      
      return(AllTraces)
      
    } else 
    
    if (getFileType() == "h5") {
      
      # Get variables for XIC function
      h5file <- msPath()
      mzMin <- floor((adjMZ - (adjMZ * (input$tolXIC / 1e6)) / 2) * 100) / 100
      mzMax <- round((adjMZ + (adjMZ * (input$tolXIC / 1e6)) / 2), 2)
      metadata <- data.frame(h5read(h5file, "Metadata"))
      xics_mz <- data.frame(value = as.numeric(t(h5read(h5file, "/Extracted_ion_chromatograms/mz_array/")), 
                                               stringsAsFactors = FALSE))
      XIC <- getExtractedIonChromatograms(h5file, mzMin, mzMax, metadata, xics_mz)

      
      ## Construct dataframe for plotting
      # Format mz data
      mz <- rep(adjMZ, nrow(XIC))
      mz <- mz[order(mz)]
      
      # Get positional information
      pos <- rep(0:(length(levels(as.factor(mz))) - 1), nrow(XIC))
      pos <- pos[order(pos)]
      
      # Create labels vector
      labels <- expand.grid(paste0("[M+", input$isoXIC, "]<sup>"), 
                            paste0(input$chargeTraceXIC, "</sup>"))
      labels <- paste0(labels$Var1, labels$Var2)
      labels <- sort(labels)
      
      # Set color position
      colPos <- unlist(lapply(pos, function(pos) {
        pos <- pos %% 13
        if (pos == 0) {pos <- 13}
        return(pos)
      }))
      
      # Fix positional information 
      pos <- pos + 1
      
      AllTraces <- data.frame(mz = mz, rt = rep(XIC[,1], length(levels(as.factor(mz)))), 
                 int = c(XIC[,-1]), lab = labels[pos], color = colorList[colPos])
      
      return(AllTraces)
    } 
    
  }),
  
  # Get scan data for specified region
  peakDataRange <- function(PreScanNum, Pre.MZ, NumPeaks) {
    
    # Get the file type as function calls are different for raw
    fileType <- getFileType()
    if (is.null(fileType)) {return(NULL)}
    
    # If is null NumPeaks, make it the default of 20
    if (is.null(NumPeaks) || is.na(NumPeaks)) {NumPeaks <- 20}
    
    # Grab peak data from XML file
    if (fileType == "mzms") {
      mzms <- getMZMS()
      if (is.null(mzms)) {return(NULL)}
      scanNum <- PreScanNum
      entryNum <- match(scanNum, getScan()$Scan.Num[order(getScan()$Scan.Num)])
      peak <- data.frame(peaks(mzms, entryNum))
    } 
    
    # Grab peak data from raw file
    if (fileType == "raw") {
      scans <- readScans(msPath(), scans = PreScanNum)
      peak <- data.frame(scans[[1]]$mZ, scans[[1]]$intensity)
    }
    
    # Get peak data from h5 file
    if (fileType == "h5") {
      scanNum <- PreScanNum
      peak <- data.frame(h5read(msPath(), paste("Spectra_mz_arrays/", scanNum, sep = "")),
                         h5read(msPath(), paste("Spectra_intensity_arrays/", scanNum, sep = "")))
    }
    
    # Return NULL if no peak information. If yes, add zeros around the data. 
    if (nrow(peak) == 0) {return(NULL)} else {
      
      # Calculate the most abundant isotope and subset out data by number of peaks 
      # near the closest match of Pre.MZ
      colnames(peak) <- c("mz", "int")
      matchIndex <- which.min(abs(peak$mz - Pre.MZ))
      
      # Set the minimum index, with a minimum value of 1
      minIndex <- matchIndex - NumPeaks
      if (minIndex < 1) {minIndex <- 1}
      
      # Set the maximum index, with a maximum value of the number of peaks
      maxIndex <- matchIndex + NumPeaks
      if (maxIndex > nrow(peak)) {maxIndex <- nrow(peak)}
      
      # Subset peak data by these indices
      peak <- peak[minIndex:maxIndex,]
      
      # Return dataframe with zeroes around peaks
      if (nrow(peak) == 0) {return(NULL)} else {
        peak <- data.frame(do.call(rbind, lapply(1:nrow(peak), function(row) {
          rbind(c(mz = peak$mz[row] - 1e-12, int = 0), c(mz = peak$mz[row], int = peak$int[row]),
                c(mz = peak$mz[row] + 1e-12, int = 0))})))
      return(peak)}}
  },
  # Get scan data for specified region
  peakDataRange <- function(PreScanNum, Pre.MZ, window) {
    
    # Get the file type as function calls are different for raw
    fileType <- getFileType()
    if (is.null(fileType)) {return(NULL)}
    
    # Set window ranges
    low <- Pre.MZ - window
    high <- Pre.MZ + window
    
    # Grab peak data from XML file
    if (fileType == "mzms") {
      mzms <- getMZMS()
      if (is.null(mzms)) {return(NULL)}
      scanNum <- PreScanNum
      entryNum <- match(scanNum, getScan()$Scan.Num[order(getScan()$Scan.Num)])
      peak <- data.frame(peaks(mzms, entryNum))
    } 
    
    # Grab peak data from raw file
    if (fileType == "raw") {
      scans <- readScans(msPath(), scans = PreScanNum)
      peak <- data.frame(scans[[1]]$mZ, scans[[1]]$intensity)
    }
    
    # Get peak data from h5 file
    if (fileType == "h5") {
      scanNum <- PreScanNum
      peak <- data.frame(h5read(msPath(), paste("Spectra_mz_arrays/", scanNum, sep = "")),
                         h5read(msPath(), paste("Spectra_intensity_arrays/", scanNum, sep = "")))
    }
    
    # Return NULL if no peak information. If yes, add zeros around the data. 
    if (nrow(peak) == 0) {peak <- data.frame(mz = c(low, high), int = c(0, 0))} else {
      
      # Calculate the most abundant isotope and subset out data
      colnames(peak) <- c("mz", "int")
      peak <- peak[peak$mz >= low & peak$mz <= high & peak$int != 0,] 
      
      # Return dataframe with zeroes around peaks
      if (nrow(peak) == 0) {
        peak <- data.frame(mz = c(low, high), int = c(0, 0))
      } else {
        peak <- data.frame(do.call(rbind, lapply(1:nrow(peak), function(row) {
          rbind(c(mz = peak$mz[row] - 1e-9, int = 0), c(mz = peak$mz[row], int = peak$int[row]),
                c(mz = peak$mz[row] + 1e-9, int = 0))})))}
      return(peak)}
  },
  
  # Get precursor isotope distribution
  getPreIsoDist <- reactive({
    
    # Return null if no scan data or ID data
    if (is.null(getScan()) || is.null(getID())) {return(NULL)}
    
    # Get charge
    Pre.Charge <- getScan()[getScanClick(), "Pre.Charge"]
    if (Pre.Charge == 0) {return(NULL)}
    
    # Get sequence
    seq <- revals$testSeq
    if (is.null(seq)) {return(NULL)}
    
    # Get scan number
    scanNum <- getScan()[getScanClick(), "Scan.Num"]
    
    # Get atoms from sequence
    if (is.na(seq) == F) { 
      seqForm <- data.frame(getAtomsFromSeq(seq))
      
      # Add modifications
      mods <- as.character(getModDF()[getModDF()$sequence == seq & getModDF()$spectrumID == scanNum, "name"])
      modForm <- NULL
      if (length(mods) == 0) {
        modForm <- do.call(bind_rows, lapply(mods, function(mod) {
          data.frame(stringToList(paste(getGlossary()[getGlossary()$Interim.Name == mod, "Molecular.Formula"])))
        }))
      }  
      
      # Add formulas together and converted to a string for RDisop
      completeForm <- bind_rows(seqForm, modForm) 
      completeForm <- paste(lapply(1:ncol(completeForm), function(col) {
        if (sum(completeForm[,col]) != 0) {
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
  getMatchedPrevious <- reactive({ 
    
    # Return null if no scan data
    if (is.null(getScan())) {return(NULL)}
    
    # If scan is a MS1, return null
    MS.Level <- getScan()[getScanClick(), "MS.Level"]
    if (MS.Level == 1) {return(NULL)}
    
    # If no window, return null
    window <- input$MPwinsize
    if (is.null(window)) {return(NULL)}
    if (window == 0) {window <- 5} 
    
    # Get scan number, peak information based on window size, and pre mz
    scanNum <- getScan()[getScanClick(), "Scan.Num"]
    PreScanNum <- getScan()[getScanClick(), "Pre.Scan"]
    if (is.na(PreScanNum) || PreScanNum == 0) {return(NULL)}
    Pre.MZ <- getScan()[getScanClick(), "Pre.MZ"]
    
    # Get range
    MatchedPre <- peakDataRange(PreScanNum, Pre.MZ, window)
    
    if (is.null(MatchedPre) || nrow(MatchedPre) == 0) {return(NULL)}
    
    # Get precursor isotope distribution
    isoDist <- getPreIsoDist()
    
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
  
  # Get All Precursor Scans
  getAllMs1 <- reactive({
    
    # If no scan data, return null
    if (is.null(getScan())) {return(NULL)}
    
    return(sort(unique(getScan()[, "Pre.Scan"])))
    
  }),
  
  # Get Matched Precursor Data for Next
  getMatchedNext <- reactive({
    
    # If no scan data, return null
    if (is.null(getScan())) {return(NULL)}
    
    # If scan is a MS1, return null
    MS.Level <- getScan()[getScanClick(), "MS.Level"]
    if (MS.Level == 1) {return(NULL)}
    
    # If no window, return null. If window is 0, change it to 5. 
    window <- input$MPwinsize
    if (is.null(window)) {return(NULL)}
    if (window == 0) {window <- 5} 
    
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
    isoDist <- getPreIsoDist()
    
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
    
  })
)
