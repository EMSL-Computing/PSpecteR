## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_10_22

# DESCRIPTION: Contains all the MSPathFinder Interface Reactives and Outputs
list(
  
  ##########################
  ## SET GLOBAL VARIABLES ##
  ##########################
  
  # Set global variable: check mark symbol
  checkmark <- img(src = "checkmark.png"),
  
  # Set global variable: x mark symbol
  xmark <- img(src = "xmark.png"),
  
  ###############################
  ## MODIFICATIONS FILE UPLOAD ##
  ###############################
  
  # Create reactive value for MS Path Finder Mods
  ModsMSPFpath <- reactiveVal(file.path("/data", "data", "TestFiles", "MSPathFinder_Mods.txt")),
  
  # Create UI for uploading a modifications file
  shinyFileChoose(input, "TDModFile", roots = uploadFolder, defaultPath = "/data/data", filetypes = c("txt")),
  
  # Add observer to change reactive variable
  observeEvent(input$TDModFile, {
    tryPath <- parseFilePaths(uploadFolder, input$TDModFile)[1,]$datapath
    if (is.na(tryPath)) {ModsMSPFpath(file.path("TestFiles", "MSPathFinder_Mods.txt"))} else {
      ModsMSPFpath(tryPath)
    }
  }),
  
  ###############
  ## REACTIVES ##
  ###############
  
  # Reactive text shows whether appropriate files have been uploaded
  output$TDinputfiles <- renderText({
  
    MSmark <- FAmark <- paste(xmark, "File Not Uploaded")
    if (is.null(msPath()) == F) {MSmark <- checkmark}
    if (is.null(fastaPath()) == F) {FAmark <- checkmark}
    
    HTML(paste(
      '<p><strong><span style="font-size: 18px;">Required Input Files:</span></strong></p>',
      '<ol style="list-style-type: lower-alpha;">',
      '<li><strong>mzML:</strong>', MSmark, msPath(), '</li>',
      '<li><strong>Fasta:</strong>', FAmark, fastaPath(), '</li>',
      '</ol>'
    ))
  }),
  
  # Reactive text shows which parameters the user has set 
  output$TDSetParameters <- renderText(
    HTML(paste(
      '<p><strong><span style="font-size: 18px;">Set Parameters:</span></strong></p>',
      '<ol>',
      '<li><strong>Charge:</strong>', min(input$TDCharge), '-', max(input$TDCharge), '</li>',
      '<li><strong>Mass:</strong>', min(input$TDMass), '-', max(input$TDMass), '</li>',
      '<li><strong>Length:</strong>', min(input$TDLength), '-', max(input$TDLength), '</li>',
      '<li><strong>Fragment Charge:</strong>', min(input$TDFragCharge), '-', max(input$TDFragCharge), '</li>',
      '<li><strong>Precursor Tolerance:</strong>', input$TDPrecursorTol, '</li>',
      '<li><strong>Fragment Tolerance:</strong>', input$TDFragmentTol, '</li>',
      '<li><strong>Database Search Mode:</strong>', input$TDDatabaseSearch, '</li>',
      '<li><strong>Activation Method:</strong>', input$TDActMet, '</li>',
      '</ol>'  
    ))),
  
  # Reactive text displays modification file
  output$TDModifications <- renderText({
    if (is.null(ModsMSPFpath()) == F) {
      HTML(paste('<p><strong><span style="font-size: 18px;">',
        'Modifications File:</span></strong></p>',
         paste(readLines(ModsMSPFpath()), collapse = "<p></p>")
      ))
    }
  }),
  
  ######################
  ## RUN MSPATHFINDER ##
  ######################
  
  # Run MSPathFinder's 3 algorithms: PbfGen, ProMex, and MSPathFinderT
  observeEvent(input$TDRunAll, {
  
    # Determine if MSPathFinder is already running
    if (is.null(DS$MSPF) == F) {
      sendSweetAlert(session, "MSPathFinder Running", 
                     "Click 'Get Status' to check MSPathFinder status.", "success")
      return(NULL)
    }
    
    # Test for MS file
    if (is.null(msPath())) {
      sendSweetAlert(session, "MSPathFinder Error", "No MS File", "error")
      return(NULL)
    }
    
    # Test for FASTA file
    if (is.null(fastaPath())) {
      sendSweetAlert(session, "MSPathFinder Error", "No FA File", "error")
      return(NULL)
    }
    
    # Test for Modification File
    if (is.null(ModsMSPFpath())) {
      sendSweetAlert(session, "MSPathFinder Error", "No Modifications File", "error")
      return(NULL)
    }
    
    # Get environmental variable
    mspathfindert_url <- Environment[Environment$Name == "MSPATHFINDERT_PATH", "Var"]
    
    # Construct URL Call 
    call <- paste(paste0(mspathfindert_url, "MSPathFinderT?mzmlFile="), 
              msPath(), "&fastaFile=", fastaPath(), "&modsFile=",
              ModsMSPFpath(), "&minCharge=", min(input$TDCharge),
              "&maxCharge=", max(input$TDCharge), "&minMass=",
              min(input$TDMass), "&maxMass=", max(input$TDMass),
              "&minLength=", min(input$TDLength), "&maxLength=", 
              max(input$TDLength), "&minFragCharge=", min(input$TDFragCharge),
              "&maxFragCharge=", max(input$TDFragCharge), "&t=",
              input$TDPrecursorTol, "&f=", input$TDFragmentTol, "&tda=0&act=",
              unlist(strsplit(input$TDActMet, " - "))[1], sep = "")
    
    # Send url call to terminal
    message(call)
    
    # Curl fetch memory for call
    h <- curl::new_handle()
    curl::handle_setopt(h, customrequest = "PUT")
    res <- curl::curl_fetch_memory(call, h)
    
    # Get taskID 
    taskID <- jsonlite::fromJSON(rawToChar(res$content))$taskID
    
    # Save url task ID
    DS$MSPF <- taskID
    
    # Send sweet alert when running
    sendSweetAlert(session, "MSPathFinderT Running", 
                   "Click 'Get Status' to check MSPathFinder status.", "success")
    
  }), 
  
  # Get Status Button
  observeEvent(input$TDgetstatus, {
    
    # If no input stored, return null  
    if (is.null(DS$MSPF)) {
      sendSweetAlert(session, "MSPathFinderT Error", "Nothing running!", "warning")
      return(NULL)
    }
    
    # Get environmental variable
    mspathfindert_url <- Environment[Environment$Name == "MSPATHFINDERT_PATH", "Var"]
    
    # Else, have task ID status pop up in sweet alert window
    status <- curl::curl_fetch_memory(paste0(mspathfindert_url, "status/", DS$MSPF))
    statusMessage <- jsonlite::fromJSON(rawToChar(status$content))
    
    # If state is started, create specific message. Otherwise, return state and clear temp variable.
    if (statusMessage$state == "STARTED") {
      message <- paste("MSPathFinder is running and currently on the", statusMessage$info$stage,
                       "algorithm.")
    } else {
      message <- paste("MSPathFinder is no longer running. Status is", statusMessage$state)
      DS$MSPF <- NULL
    }
    
    # Send sweet alert with message
    sendSweetAlert(session, "MSPathFinderT Status", message, "success")
    
  })
  
)