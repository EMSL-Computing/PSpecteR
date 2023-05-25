## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_12_29

# DESCRIPTION: Uploads data and visualizes output from MS Path Finder's ProMex

list(
  
  ###############
  ## FUNCTIONS ##
  ###############
  
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
  
  # Define the colors for abundance metric
  ColorList <- c('#B30011', '#BD171F', '#C7332D', '#D5533D',
                 '#E16F4B', '#EE8C59', '#F6A770', '#F6B88B',
                 '#EEC9AF', '#CBCFE6', '#B9C2EF', '#89A1F1',
                 '#688AFA', '#5779F8', '#3F5FEE', '#2942C5', 
                 '#1D32BC', '#1124B7', '#1124B7'),
  
  ################
  ## MS1FT FILE ##
  ################
  
  # Specify the shiny file choose box
  shinyFileChoose(input, 'ms1ftFile', roots = uploadFolder, defaultPath = "/data/data", 
                  filetypes = c("ms1ft", "MS1FT")),
  
  # When the choose button is clicked, here is the volume it will open up to and what information will be given to ms1ftPath
  observeEvent(input$ms1ftFile, {
    
   if (!LightVersion) {  
    
     tryPath <- parseFilePaths(uploadFolder, input$ms1ftFile)[1,]$datapath
     
     # Remove specific directory path information when macOS or Linux
     if (Sys.info()["sysname"] %in% c("Darwin", "Linux")) {
        tryPath <- gsub("/Volumes/Macintosh HD", "", tryPath)
     }
     ifelse(is.na(tryPath), ms1ftPath(NULL), ms1ftPath(tryPath))
     
   } else {
     ms1ftPath(input$ms1ftFile$datapath)
   }
    
  }),
  
  # Applicable only to light mode
  observeEvent(input$targetsFile, {
    targetsPath(input$targetsFile$datapath)
  }),
  
  # Add a warning if the MS1FT file for whether the file is loaded
  output$ms1ftWarn <- renderText({
    
    if (is.null(ms1ftPath())) {
      
      if (!LightVersion) {
        if (nchar(input$ms1ftHandle) == 0) {
          paste("No MS1FT file uploaded")
        } else if (file.exists(input$ms1ftHandle) & grepl(".ms1ft|.MS1FT", tail(unlist(strsplit(input$ms1ftHandle, "/")), 1))) {
            paste(checkmark, HTML(paste("<strong>", input$ms1ftHandle, "</strong>")), "is a valid MS1FT file path.")
        } else {
            paste(xmark, HTML(paste("<strong>", input$ms1ftHandle, "</strong>")), "is not a valid MS1FT file path.")
        } 
      } else {paste("No MS1FT file uploaded")} 
      
    } else {
      
      if (LightVersion & input$testMS1FT == FALSE) {
        if (is.null(input$targetsFile)) {
          HTML(paste0("<p>Uploaded MS1FT: ", input$ms1ftFile$name, "</p><p>For full functionality, upload the IC Targets.tsv.</p>"))
        } else {
          HTML(paste0("<p>Uploaded MS1FT: ", input$ms1ftFile$name, "</p><p>Uploaded Targets: ", input$targetsFile$name), "</p>")
        } 
      } else if (LightVersion & input$testMS1FT) {
        HTML(paste0("<p>Uploaded MS1FT: ", ms1ftPath(), "</p><p>Uploaded Targets: ", targetsPath(), "</p>"))  
      }
      
      
    }
    
  }),
  
  # If button is clicked and the path is valid, then lock in this variable's path
  observeEvent(input$ms1ftHandleGo, {
    
    if (!LightVerison) {
      # Set the message to a warning
      alert <- sendSweetAlert(session, "MS1FT File Path Error", "Input file must exist 
               and be a .ms1ft or .MS1FT file.", "error") 
      # If the entered file is valid, inform user and set the ms1ftPath() to that file
      if (nchar(input$ms1ftHandle) > 0 && file.exists(input$ms1ftHandle) && 
          grepl(".MS1FT|.ms1ft", tail(unlist(strsplit(input$ms1ftHandle, "/")), 1))) {
        alert <- sendSweetAlert(session, "MS1FT File Path Success!", "Input file accepted.", "success") 
        ms1ftPath(input$ms1ftHandle)
      }
      # Send alert
      alert
      
    }
  }),
  
  # Clear MS Path and File 
  observeEvent(input$ms1ftHandleClear, {
    # When button is clicked, clear the stored path information and typed path
    updateTextInput(session, "ms1ftHandle", value = "")
    ms1ftPath(NULL)
  }),
  
  ##################
  ## PROTEIN FILE ##
  ##################
  
  # Add text warning to say whether a protein TSV has been identified or not
  output$PMproteinWarn <- renderText({
    
    if (!LightVersion) {
    
      # If no MS1FT file, let the user know
      if (is.null(ms1ftPath())) {""} else {
        
        # Get protein file name
        proteinData <- gsub(".ms1ft", "_IcTarget.tsv", ms1ftPath())
        
        if (is.null(checkFile(proteinData)) == F) {paste("")} else {
          paste(xmark, proteinData, "is not in the same folder as the MS1FT file.")}   
      }
      
    }
    
  }),
  
  ####################
  ## RENDER WIDGETS ##
  ####################
  
  # Render "Use MS1FT Test Files?" switch
  output$testMS1FTSWITCH <- renderUI({
    MSFT <- materialSwitch("testMS1FT", label = HTML("<strong>Use MS1FT & IC Targets Test File?</strong>"), 
              value = F, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(MSFT, "Use MS1FT Test File?", "Test ProMex Feature Map with a MS1FT test file.",
             options = list(selector = '.material-switch'), placement = 'right')
    } else {MSFT}
  }), 
  
  # Render the "Select Proteins" picker input
  output$PMproteinChoose <- renderUI({
  
    if (is.null(GET_ms1ft())) {return(NULL)}
    
    if (length(unique(GET_ms1ft()$ProteinName)) == 1 && unique(GET_ms1ft()$ProteinName) == "Protein Not Found") {
      "Upload an IC Targets file to subset by protein name."
    }
    
    # Unique and ordered
    orderedProt <- unique(GET_ms1ft()$ProteinName)[order(unique(GET_ms1ft()$ProteinName))]
    
    pickerInput("PMselectProtein", label = "Filter by Protein", 
                choices = orderedProt, selected = orderedProt,
                multiple = T, options = list(`live-search` = T, `virtual-Scroll` = 10,
                `actions-Box` = T))
    
  }),
  
  ####################################
  ## OUTPUT WIDGETS AND RENDER PLOT ##
  ####################################
  
  # Load test file for PromMex Feature Map
  observeEvent(input$testMS1FT, {
    if (!LightVersion) {
      if (input$testMS1FT == T) {
        ms1ftPath("./data/TestFiles/TopDown/TopDown.ms1ft")
      }
    } else {
      if (input$testMS1FT == T) {
        ms1ftPath("TestFiles/TopDown/TopDown.ms1ft")
        targetsPath("TestFiles/TopDown/TopDown_IcTarget.tsv")
      }
    }
  }),
  
  # Plot ProMex Feature Map
  output$PMFM <- renderPlotly({
    
    # If no MS1FT file has been uploaded, then no graph will be generated
    if (is.null(GET_ms1ft())) {return(NULL)}

    # Make plotly
    p <- promex_feature_plot(
      MS1FT = GET_ms1ft(),
      Proteins = input$PMselectProtein,
      Interactive = TRUE
    )
    
    # Store the current plot
    plots$currPMFM <- p
    
    plotly::toWebGL(p)

  }),
  
  # ProMex datatable
  output$ms1ftTab <-  DT::renderDataTable({
    
    if (is.null(GET_ms1ft())) {return(NULL)}
    
    datatable(GET_ms1ft(), 
              rownames = F, filter = 'top', options = list(pageLength = 5),
              selection = list(mode = 'single'))
  })
)