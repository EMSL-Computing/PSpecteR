## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains coverage and number of peaks calculations

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