## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_07_07

# DESCRIPTION: This contains the get functions which filter data into the graphs.

# Included: getActMet, getPrecursorWindow, getFileType, getActMetIon, getIntenMin,
# getTol, getScanClick

list(
  
  # This function formats activation method data correctly
  getActMet <- function(filterString) {
    unlist(lapply(filterString, function(filter) {
      if (grepl("@", filter)) {
        toupper(gsub("[^A-z]|\\[|\\]", "", unlist(strsplit(filter, "@"))[2]))
      } else {NA}
    }))
  }, 
  
  # Get file's type to determine data processing. Returns null if no file.
  getFileType <- reactive({
    
    # If no MS Path, return NULL
    if (is.null(msPath())) {return(NULL)}
    
    if (grepl(".mzML", msPath()) | grepl(".mzXML", msPath())) {return("mzms")} else
    if (grepl(".raw", msPath())) {return("raw")} else
    if (grepl(".h5", msPath())) {return("h5")}

  }),
  
  # Get the ions associated with the activation method. Default is all values.
  getActMetIon <- reactive({
    
    # Declare ions variable to hold all ion types
    Ions <- c("a", "b", "c", "x", "y", "z", "Spec")
    
    # Test for scan data
    if (is.null(msPath())) {return(Ions)}
    
    # Get most frequent activation method
    AM <- getScan()[, "Act.Met"]
    AM <- AM[!is.na(AM)]; AM <- table(AM); AM <- names(AM[which.max(AM)])
    if (is.null(AM)) {return(Ions)}
    if (AM == "HCD") {Ions <- c("b", "y", "Spec")} else 
    if (AM == "CID") {Ions <- c("a", "b", "y", "Spec")} else
    if (AM == "ETD") {Ions <- c("b", "c", "y", "z", "Spec")} 
    return(Ions)
    
  }),
  
  # Get intensity minimum values
  getIntenMin <- reactive({
    intenMin <- input$ssIntenMin
    if (is.null(intenMin)) {intenMin <- 100}
    return(intenMin)
  }),
  
  # Get tolerance value for the scan & seq data
  getTol <- reactive({
    tol <- input$ssTolerance
    if (is.null(tol)) {tol <- 10}
    return(tol)
  }),
  
  # Get Scan Table Click, returning 1 if nothing is selected
  getScanClick <- reactive({
    clicked <- input$ssScan_row_last_clicked
    if (is.null(clicked)) {clicked <- 1}
    return(clicked)
  })
  
)