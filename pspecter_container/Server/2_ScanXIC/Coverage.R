## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_07_15

# DESCRIPTION: Contains coverage and number of peaks calculations

output$coverage <- renderText({
  
  # These are the variables needed to render at the bottom of the sidebar
  coverage <- NULL
  numPeaks <- NULL
  
  # Get Scan Data
  scan <- getScan()
  if (is.null(scan) == F) {
    clicked <- getScanClick()
    peak <- getSSPeak()
    peak <- peak[peak$intensity > 0,]
    numPeaks <- nrow(peak)
  
  # Get Fragment Data to Calculate Coverage 
  if (is.null(getFrag()) == F) { 
    
    # Get the sequence length, removing one as the first amino acid is not countedin the coverage calculation
    seqLen <- nchar(scan[clicked, "Sequence"]) - 1
    
    # If a new sequence has been inputted, test that new sequence
    if (is.na(seqLen)) {seqLen <- nchar(revals$testSeq) - 1}
    
    # Get all nposition data of identified fragments, remove npos 1, and take the length
    covLen <- length(unique(getFrag()$npos[getFrag()$npos != 1]))
    
    coverage <- paste(round((covLen / seqLen * 100), 1), "%", sep = "")
    
  }} 
  
  # Paste coverage output
  paste("Number of Peaks: ", numPeaks, ", Coverage: ", coverage, sep = "")
  
})