## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_10_04

# DESCRIPTION: This contains a function to render the B. ProMexFeatureMap: getPMFM

list(
  
  ############################################
  ## DEFINE FUNCTIONS AND LISTS FOR GETTERS ##
  ############################################

  # Define which values by position go into each bin
  binSplitColorGroup <- function(len) {
    entriesPerBin <- len %/% 19
    remainder <- len %% 19
    
    # If there's less than 19entries, then make that number the max one
    if (len > 19) {len <- 19}
    
    # Return the colors
    lapply(1:len, function(i) {
      if (remainder > 0) {
        remainder <<- remainder - 1; 
        return(rep(i, entriesPerBin + 1))} else {
      return(rep(i, entriesPerBin))}
    })
  },
  
  ##########################
  ## FILE CHECK FUNCTIONS ##
  ##########################
  
  # Make function for reading only the first line
  readFirstLine <- function(con) {
    return(readLines(con, n = 1))
  },
  
  # Check if a file exists continuously
  checkFile <- function(con) {
    suppressWarnings({
      FILE <- tryCatch(reactiveFileReader(500, session, con, readFirstLine), 
                      error = function(e) {return(NULL)})
      FILE <- tryCatch(FILE(), error = function(e) {return(NULL)})
    })
    return(FILE)
  },
  
  
  ####################
  ## DEFINE GETTERS ##
  ####################

  # Read and save data from the MS1FT File  
  getPMFM <- reactive({
    
    if (is.null(ms1ftPath())) {return(NULL)}
    
    # Read in MS1FT file and keep only the necessary columns
    PMFM <- read.table(ms1ftPath(), header = T, stringsAsFactors = F)
    PMFM <- data.frame("FeatureID" = PMFM$FeatureID, "MinElutionTime" = PMFM$MinElutionTime,
                       "MaxElutionTime" = PMFM$MaxElutionTime, "MonoMass" = PMFM$MonoMass,
                       "Log10Abundance" = log10(PMFM$Abundance))
    
    # Order list from highest to lowest abundance and provide a color
    PMFM <- PMFM[order(-PMFM$Log10Abundance),]
    PMFM$Color <- as.factor(unlist(binSplitColorGroup(length(PMFM$Log10Abundance))))
    
    # Include protein data if file exists
    proteinDataFile <- gsub(".ms1ft", "_IcTarget.tsv", ms1ftPath())
    
    if (is.null(checkFile(proteinDataFile))) {
    return(data.frame(PMFM, "ProteinName" = rep("File Not Found", nrow(PMFM))))} else {
      
      # Read in protein data. Due to weird tsv formatting issues, each line has to be read in as a character
      proteinData <- data.frame(do.call(rbind, (lapply(readLines(proteinDataFile), function(line) {
        splitLine <- unlist(strsplit(line, "\t"))
        return(c(splitLine[7], splitLine[15]))
      }))))
      
      # Rename dataframe and remove first line
      proteinData <- proteinData[2:nrow(proteinData),]
      colnames(proteinData) <- c("ProteinName", "FeatureID")
      proteinData$FeatureID <- as.numeric(as.character(proteinData$FeatureID))
      
      # Add back to PMFM dataframe
      PMFM <- merge(PMFM, proteinData, by = "FeatureID", all = T)
      
      # Change NA data from protein to blank string
      PMFM$ProteinName <- as.character(PMFM$ProteinName)
      PMFM$ProteinName[is.na(PMFM$ProteinName)] <- "No Protein Identified"
      
      # Remove duplicate rows
      PMFM <- unique(PMFM)
      
      # Remove any NAs
      PMFM <- PMFM[is.na(PMFM$MonoMass) == F,]
      
      return(PMFM)
      
    }
    
  }),
  
  # Read and save data from the MS1FT File  
  getPMFMTable <- reactive({
    
    if (is.null(ms1ftPath())) {return(NULL)}
    
    # Read in MS1FT file and keep only the necessary columns
    PMFM <- read.table(ms1ftPath(), header = T, stringsAsFactors = F)
    PMFM <- data.frame("FeatureID" = PMFM$FeatureID, "MinElutionTime" = round(PMFM$MinElutionTime, 4),
                       "MaxElutionTime" = round(PMFM$MaxElutionTime, 4), "MonoMass" = round(PMFM$MonoMass, 4),
                       "Log10Abundance" = round(log10(PMFM$Abundance), 4))
    
    # Order list from highest to lowest abundance and provide a color
    PMFM <- PMFM[order(-PMFM$Log10Abundance),]
    return(PMFM)
  })

)