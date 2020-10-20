## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_09_29

# DESCRIPTION: This contains objects that contain a mix of either MS, ID, or FASTA data:
# getScan, getScanMod, getProteinTree

list(
  
  # Get all of the data for the scan table
  getScan <- reactive({
    
    # Get metadata if it exists
    if (is.null(getFileType())) {return(NULL)}
    
    # Adjust variables if the scan data is mzms or raw
    if (getFileType() == "mzms") {
      scan <- getHeader() 
      scan$retentionTime <- scan$retentionTime / 60
      if (nrow(scan[scan$msLevel == 1,]) > 0) {scan[scan$msLevel == 1,]$precursorCharge <- 1}} else 
    if (getFileType() == "raw") {scan <- getRAWScanSM()} else
    if (getFileType() == "h5") {scan <- getH5scan()}
    
    # Get activation method
    if (getFileType() == "mzms" | getFileType() == "raw") {
      scan$filterString <- getActMet(as.character(scan$filterString))}
    
    # Assimilate scan data
    scan <- data.frame(1:nrow(scan), "Scan.Num" = scan$acquisitionNum, scan$msLevel, 
                       round(scan$retentionTime, 4), round(scan$precursorMZ, 4), 
                       scan$precursorCharge, scan$precursorScanNum, scan$filterString)
    
    # Add protein data if it exists
    ID <- getID()
    scan <- data.frame(scan, NA, NA, NA, NA, NA, NA, NA, NA)
    if (is.null(ID) == F) {
      scanID <- scan[match(ID$Scan, scan$Scan.Num),]
      scanID[,c(9:16)] <- list(ID$Sequence, ID$Protein, ID$Mass, 
                                ID$Score, ID$QVal, ID$isDecoy,
                                ID$Description, ID$Start)
      names(scan) <- names(scanID)
      scan <- rbind(scanID, scan[scan$Scan.Num %in% ID$Scan == F,])
      scan[,1] <- 1:nrow(scan)
    }
    colnames(scan) <- c("Order", "Scan.Num", "MS.Level", "RT", "Pre.MZ", "Pre.Charge", "Pre.Scan",
                      "Act.Met", "Sequence", "Protein.ID", "Mass",
                      "Score", "Q.Value", "isDecoy", "Description", "Start")
    scan <- scan[!is.na(scan$Scan.Num),]
    scan$Sequence <- as.character(scan$Sequence)
    return(scan)
    
  }),
  
  # Get scan data with modifications (not all graphics contain this information,
  # which is why it is a separate function).
  getScanMod <- reactive({
    
    # Get Scan and Modification Dataframe
    if (is.null(getScan()) | is.null(getModDF()) | nrow(getScan()) < 1) {return(NULL)}
    scan <- getScan()
    mod <- getModDF()
    
    # Generate unique keys since scans can have multiple keys
    mod$key <- paste(mod$spectrumID, "_", mod$sequence, sep = "")
    scan$key <- paste(scan$Scan.Num, "_", scan$Sequence, sep = "")
    
    # Get modifications key and sequence with annotations
    mod <- data.frame(key = unique(mod$key), sequence = lapply(unique(mod$key), function(key) {
      seqMods <- mod[mod$key == key,]
      seqMods <- seqMods[seqMods$location > -1 & 
                         seqMods$location <= nchar(as.character(seqMods$sequence)),]
      modLoc <- seqMods$location
      seq <- as.character(seqMods$sequence)[1]
      seqSplit <- strsplit(seq, "") %>% unlist()
      seqSplit[modLoc] <- paste(seqSplit[modLoc], "*", sep = "")
      seq <- paste(seqSplit, collapse = "")
      if (0 %in% modLoc) {seq <- paste("*", seq, sep = "")}
      seq
    }) %>% unlist())
    
    # Add annotations
    scan[scan$key %in% mod$key,]$Sequence <- as.character(merge(scan, mod, 
                                             by = c("key", "key"), sort = F)$sequence)
    return(scan)
  }),
  
  # Get data needed to generate protein tree figures.
  getProteinTree <- reactive({
    
    # Get required files
    req(getPTID(), getFasta(), getID(), getPTscan())
    PTID <- getPTID()
    fasta <- getFasta()
    ID <- getID()
    PTscan <- getPTscan()
    
    # Get user click on Protein Tree table
    clicked <- input$PTTable_row_last_clicked
    if (is.null(clicked)) {clicked = 1}
    
    # Get the protein name and all the ID data with that name, removing any NAs
    protein <- PTID[clicked, 1]
    litSeq <- as.character(fasta[[as.character(protein)]][1])
    match <- PTscan[as.character(PTscan$Protein.ID) == as.character(protein),]
    match <- match[!is.na(match$Protein.ID),]
    
    # Get the scan number and sequence information, adding on the direction and colors
    proteinTree <- data.frame(match$Sequence, match$Scan.Num, match$Start, "N to C",
                              "rgb(124,205,124)", match$Start + nchar(as.character(match$Sequence)))
    colnames(proteinTree) <- c("Sequence", "Scan", "MatchPos", "Dir", "Color", "Total")
    
    # Ensure correct data types, add extra data for graphing, and the protein seq
    proteinTree$Sequence <- as.character(proteinTree$Sequence)
    proteinTree$Dir <- as.character(proteinTree$Dir)
    proteinTree$Color <- as.character(proteinTree$Color)
    proteinTree <- rbind(proteinTree, c(litSeq, 1, 0, "Lit Seq", "rgb(0,0,0)", nchar(litSeq) + 1))
    proteinTree$Dir <- as.factor(proteinTree$Dir)
    proteinTree$MatchPos <- as.numeric(proteinTree$MatchPos)
    proteinTree$Scan <- as.numeric(proteinTree$Scan)
    proteinTree$Total <- as.numeric(proteinTree$Total)
    return(proteinTree)
  })
  
)