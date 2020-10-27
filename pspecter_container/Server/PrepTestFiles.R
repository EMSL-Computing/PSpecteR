## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_10_26

# DESCRIPTION: Check if test files are in the proper directory

# Create a destination path
prepTestFiles <- function() {
  
  # Name paths
  sourcePath <- "/TestFiles"
  destinationPath <- file.path("/data", "data", "TestFiles")
  
  # Create missing directories 
  importantDir <- c(
    file.path(destinationPath),
    file.path(destinationPath, "BottomUp"),
    file.path(destinationPath, "TopDown")
  )
  
  # For each missing directory, create it
  for (dir in importantDir) {if (dir.exists(dir) == F) {dir.create(dir)}}
  
  # List all of the test files
  filesToCheck <- function(path = "/") {
    return(c(
      file.path(path, "MSGF_Parameters.txt"),
      file.path(path, "MSPathFinder_Mods.txt"),
      file.path(path, "QC_Shew.fasta"),
      file.path(path, "BottomUp", "BottomUp.mzid"),
      file.path(path, "BottomUp", "BottomUp.mzML"),
      file.path(path, "TopDown", "TopDown_IcTarget.tsv"),
      file.path(path, "TopDown", "TopDown_MSPathFinder_PSMS.mzid"),
      file.path(path, "TopDown", "TopDown_MSPathFinder_Score.csv"),
      file.path(path, "TopDown", "TopDown.ms1ft"),
      file.path(path, "TopDown", "TopDown.mzid"),
      file.path(path, "TopDown", "TopDown.mzML")
    ))
  }
  
  # Check whether each file exists, and if not copy it into the data directory
  for (fileNum in 1:length(filesToCheck())) {
    
    # Check if file exists
    if (file.exists(filesToCheck(destinationPath)[fileNum]) == F) {
      file.copy(filesToCheck(sourcePath)[fileNum], filesToCheck(destinationPath)[fileNum],
                overwrite = T)
      message(paste("Copied file", filesToCheck(sourcePath)[fileNum]))
    }
    
  }
}

