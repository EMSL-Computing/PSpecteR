## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_06_09

# DESCRIPTION: Gets the csv glossary (getGlossary) and any added modifications
# (getAddedDataFile)

list(

  # Set the root directoy 
  roots <- c(wd = Sys.getenv("HOME")),
  
  # Allow user to choose added data
  shinyFileChoose(input, "glCSVadd", roots = roots, filetypes = c("csv")),
  
  # Extract modification data from the optional file
  getAddedDataFile <- reactive({
    
    # Add Data from additional file
    tryPath <- parseFilePaths(roots, input$glCSVadd)[1,]$datapath
    if (is.na(tryPath)) {return(NULL)}
    AddedData <- read.csv(tryPath, header = T)
    
    # Catch error if data.frame does not have 8 columns
    if (length(AddedData) != 9) {
      sendSweetAlert(session, "Upload Mod CSV Error", "CSV must contain the same number of columns
                     as the glossary (9).", "error")
      return(NULL)
    }
    
    # Ensure that colnames are correct
    if (F %in% (colnames(AddedData) %in% c("Accession.Number", "PSI.MS.Name", "Interim.Name",
                                           "Description", "Monoisotopic.Mass", "Average.Mass",      
                                           "Composition", "Molecular.Formula", "Modified.Sites"))) {
      sendSweetAlert(session, "Upload Mod CSV Error", "CSV must contain the same column names
                     as the glossary.", "error")
      return(NULL)
    }
    
    # Set monoisotopic mass to numeric
    AddedData$Monoisotopic.Mass <- as.numeric(AddedData$Monoisotopic.Mass)
    
    # Write CSV if no errors
    sendSweetAlert(session, "Upload Mod CSV", "CSV Uploaded!", "success")
    
    return(AddedData)
  }),
  
  # Get Unimod Glossary
  getGlossary <- reactive({
   GlossPath <- file.path("Glossary", "UnimodGlossary_v07.2019.csv")
   if (file.exists(GlossPath) == F) {return(NULL)}
   Glossary <- read.csv(GlossPath, header = T)
   AddedData <- getAddedDataFile()
   result <- data.frame(rbind(Glossary, AddedData, gloss$AddedMods))
   return(result)
  })
  

)