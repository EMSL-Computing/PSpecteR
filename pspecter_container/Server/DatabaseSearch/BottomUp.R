## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_10_22

# DESCRIPTION: Contains all the MSGF+ Interface Outputs and Reactives

list(
  
  ##########################
  ## SET GLOBAL VARIABLES ##
  ##########################
  
  # Set global variable: check mark symbol
  checkmark <- img(src = "checkmark.png"),
  
  # Set global variable: x mark symbol
  xmark <- img(src = "xmark.png"),
  
  ############################
  ## PARAMETERS FILE UPLOAD ##
  ############################
  
  # Create reactive value for MSGF+ Modifications
  ParamsMSGFpath <- reactiveVal(file.path("/data", "data", "TestFiles", "MSGF_Parameters.txt")),
  
  # Create UI for uploading a parameters file
  shinyFileChoose(input, "BUParamFile", roots = uploadFolder, defaultPath = "/data/data", filetypes = c("txt")),
  
  # Add observer to change reactive variable
  observeEvent(input$BUParamFile, {
    tryPath <- parseFilePaths(uploadFolder, input$BUParamFile)[1,]$datapath
    if (is.na(tryPath)) {ParamsMSGFpath(file.path("TestFiles", "MSGF_Parameters.txt"))} else {
      ParamsMSGFpath(tryPath)
    }
  }),
  
  ###############
  ## REACTIVES ##
  ###############
  
  # Reactive text shows whether appropriate files have been uploaded
  output$BUinputfiles <- renderText({
    
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
  
  # Reactive text displays modification file
  output$BUParameters <- renderText({
    if (is.null(ModsMSPFpath()) == F) {
      HTML(paste('<p><strong><span style="font-size: 18px;">',
                 'Parameters File:</span></strong></p>',
                 paste(readLines(ParamsMSGFpath()), collapse = "<p></p>")
      ))
    }
  }),
  
  ###############
  ## RUN MSGF+ ##
  ###############
  
  # Run MSGF+
  observeEvent(input$BURun, {
    
    # Determine if MSGF is alredy running 
    if (is.null(DS$MSGF) == F) {
      sendSweetAlert(session, "MSGF+ Running", 
                     "Click 'Get Status' to check MSGF+ status.", "success")
      return(NULL)
    }
    
    # Test for MS file
    if (is.null(msPath())) {
      sendSweetAlert(session, "MSGF+ Error", "No MS File", "error")
      return(NULL)
    }
    
    # Test for FASTA file
    if (is.null(fastaPath())) {
      sendSweetAlert(session, "MSGF+ Error", "No FA File", "error")
      return(NULL)
    }
    
    # Test for Modification File
    if (is.null(ParamsMSGFpath())) {
      sendSweetAlert(session, "MSGF+ Error", "No Modifications File", "error")
      return(NULL)
    }
    
    # Get environmental variable
    msgf_url <- Environment[Environment$Name == "MSGF_PATH", "Var"]
    
    # Construct URL Call 
    call <- paste(paste0(msgf_url, "MSGF?mzmlFile="), 
                  msPath(), "&fastaFile=", fastaPath(), "&paramsFile=",
                  ParamsMSGFpath(), sep = "")
    
    # Send url call to terminal
    message(call)
    
    # Curl fetch memory for call
    h <- curl::new_handle()
    curl::handle_setopt(h, customrequest = "PUT")
    res <- curl::curl_fetch_memory(call, h)
    
    # Get taskID 
    taskID <- jsonlite::fromJSON(rawToChar(res$content))$taskID
    
    # Save url task ID
    DS$MSGF <- taskID
    
    # Send sweet alert when running
    sendSweetAlert(session, "MSGF+ Running", 
                   "Click 'Get Status' to check MSGF+ status.", "success")
    
  }),
  
  # Get Status Button
  observeEvent(input$BUgetstatus, {
    
    # If no input stored, return null  
    if (is.null(DS$MSGF)) {
      sendSweetAlert(session, "MSGF+ Error", "Nothing running!", "warning")
      return(NULL)
    }
    
    # Get environmental variable
    msgf_url <- Environment[Environment$Name == "MSGF_PATH", "Var"]
    
    # Else, have task ID status pop up in sweet alert window
    status <- curl::curl_fetch_memory(paste0(msgf_url, "status/", DS$MSGF))
    statusMessage <- jsonlite::fromJSON(rawToChar(status$content))
    
    # If state is started, create specific message. Otherwise, return state and clear temp variable.
    if (statusMessage$state == "STARTED") {
      message <- paste("MSGF is running!")
    } else {
      message <- paste("MSGF is no longer running. Status is", statusMessage$state)
      DS$MSGF <- NULL
    }
    
    # Send sweet alert with message
    sendSweetAlert(session, "MSGF Status", message, "success")
    
  })
  
)