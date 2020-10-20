## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_09_21

# DESCRIPTION: This contains the command for exporting multiple CSVs 
# including All Ion, 

list(
  
  # Export ALLION CSV
  output$ALLIONcsv <- downloadHandler(
    filename = function() {paste("AllIons_SCAN", getScan()[getScanClick(), "Scan.Num"], "_", 
                                 "PPMTHRESH", getTol(), "_", "IONS", gsub("Spec" ,"",
                                 paste(getActMetIon(), collapse = "")), "_",
                                 unlist(strsplit(paste(unlist(strsplit(msPath(), ".", 
                                 fixed = T))[1], ".csv", sep = ""), "/"))[-1], sep = "")},
    content = function(file) {fwrite(getAllFrag(), file)}
  ),
  
  # Export FRAGMENT CSV
  output$FRAGcsv <- downloadHandler(
    filename = function() {paste("FragData_SCAN", getScan()[getScanClick(), "Scan.Num"], "_", 
                                 "PPMTHRESH", getTol(), "_", "IONS", gsub("Spec" ,"",
                                 paste(getActMetIon(), collapse = "")), "_",
                                 unlist(strsplit(paste(unlist(strsplit(msPath(), ".", 
                                 fixed = T))[1], ".csv", sep = ""), "/"))[-1], sep = "")},
    content = function(file) {fwrite(getFrag(), file)}
  ),
  
  # Export GLOSSARY CSV
  output$glEXP <- downloadHandler(
    filename = function() {"Modifications_Glossary.csv"},
    content = function(file) {fwrite(getGlossary(), file)}
  ),
  
  # Export PEAK CSV
  output$PEAKcsv <- downloadHandler(
    filename = function() {paste("PeakData_SCAN", getScan()[getScanClick(), "Scan.Num"], "_",
                                 unlist(strsplit(paste(unlist(strsplit(msPath(), ".", 
                                 fixed = T))[1], ".csv", sep = ""), "/"))[-1], sep = "")},
    content = function(file) {fwrite(getSSPeak(), file)}
  ),
  
  # Export PROTEIN COVERAGE
  output$PTcsv <- downloadHandler(
    filename = function() {paste("ProteinCoverage_",
                                 unlist(strsplit(paste(unlist(strsplit(idPath(), ".", 
                                 fixed = T))[1], ".csv", sep = ""), "/"))[-1], sep = "")},
    content = function(file) {fwrite(getPTID(), file)}
  ),
  
  # Export SCAN
  output$SCANcsv <- downloadHandler(
    filename = function() {paste("ScanMetadata_", unlist(strsplit(paste(unlist(strsplit(
                           msPath(), ".", fixed = T))[1], ".csv", sep = ""), "/"))[-1], sep = "")},
    content = function(file) {fwrite(getScan(), file)}
  ),
  
  # Export VisPTM
  output$VPcsv <- downloadHandler(
    filename = function() {paste("VisPTM_SCAN", getScan()[getScanClick(), "Scan.Num"], "_",
                                 "PPMThresh", getTol(), "_seq", getNewVSeq(), "_",
                                 unlist(strsplit(paste(unlist(strsplit(msPath(), ".", 
                                 fixed = T))[1], ".csv", sep = ""), "/"))[-1], sep = "")},
    content = function(file) {fwrite(getVPMetrics(), file)}
    
  )
  
)