## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: This contains the command for exporting multiple CSVs 

list(
  
  # Export MATCHED PEAKS csv
  output$FRAGcsv <- downloadHandler(
    filename = function() {paste0("FragData_SCAN", GET_scan_metadata()[GET_scan_click(), "Scan Number"], "_", 
                                 "PPMTHRESH", input$ssTolerance, "_", "IONS", gsub("Spec" ,"",
                                 paste(input$ionGroups, collapse = "")), "_",
                                 msPath() %>% strsplit(".", fixed = T) %>% unlist() %>% head(1) %>% strsplit("/", fixed = T) %>% unlist() %>% tail(1), 
                                 ".csv")},
    content = function(file) {fwrite(GET_matched_peaks(), file)}
  ),
  
  # Export GLOSSARY CSV
  output$glEXP <- downloadHandler(
    filename = function() {"Modifications_Glossary.csv"},
    content = function(file) {fwrite(GET_glossary(), file)}
  ),
  
  # Export ADDITIONAL MODFICATINONS CSV
  output$glnewEXP <- downloadHandler(
    filename = function() {"Added_Modifications.csv"},
    content = function(file) {fwrite(gloss$AddedMods, file)}
  ),
  
  # Export PEAK CSV
  output$PEAKcsv <- downloadHandler(
    filename = function() {paste0("PeakData_SCAN", GET_scan_metadata()[GET_scan_click(), "Scan Number"], "_", 
                                         msPath() %>% strsplit(".", fixed = T) %>% unlist() %>% head(1) %>% strsplit("/", fixed = T) %>% unlist() %>% tail(1), 
                                         ".csv")},
    content = function(file) {fwrite(GET_peak_data(), file)}
  ),
  
  # Export PROTEIN COVERAGE
  output$PTcsv <- downloadHandler(
    filename = function() {paste0("ProteinCoverage_",
                                  idPath() %>% strsplit(".", fixed = T) %>% unlist() %>% head(1) %>% strsplit("/", fixed = T) %>% unlist() %>% tail(1),
                                 ".csv")},
    content = function(file) {fwrite(GET_protein_table(), file)}
  ),
  
  # Export SCAN
  output$SCANcsv <- downloadHandler(
    filename = function() {paste0("ScanMetadata_", 
                                  msPath() %>% strsplit(".", fixed = T) %>% unlist() %>% head(1) %>% strsplit("/", fixed = T) %>% unlist() %>% tail(1), 
                                  ".csv")},
    content = function(file) {fwrite(GET_scan_metadata(), file)}
  ),
  
  # Export VisPTM
  output$VPcsv <- downloadHandler(
    filename = function() {paste("VisPTM_SCAN", GET_scan_metadata()[GET_scan_click(), "Scan Number"], "_",
                                 msPath() %>% strsplit(".", fixed = T) %>% unlist() %>% head(1) %>% strsplit("/", fixed = T) %>% unlist() %>% tail(1), 
                                 ".csv")},
    content = function(file) {fwrite(revals$PTMdf, file)}
    
  )
  
)