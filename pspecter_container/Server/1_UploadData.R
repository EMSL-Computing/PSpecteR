## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains the functions necessary to upload files to PSpecteR

list(
  
  ##########################
  ## SET GLOBAL VARIABLES ##
  ##########################
  
  # Include a check mark and an xmark for valud and invalid file paths
  checkmark <- img(src = "checkmark.png"),
  xmark <- img(src = "xmark.png"),  
  if (!LightVersion) {uploadFolder <- getVolumes()},
  
  ###############
  ## MS UPLOAD ##
  ###############
  
  # Set a reactive value for the mass spec file path (msPath)
  msPath <- reactiveVal(NULL),
  
  if (!LightVersion) {
  
    # Specify the shiny file choose box
    shinyFileChoose(input, 'mzmsFile', roots = uploadFolder, defaultPath = "/data/data",
                    filetypes = c("mzML", "mzml", "mzxml", "mzXML", "raw"))
    
  },
  
  # When the choose button is clicked, here is the volume it will open up to and what information will be given to msPath
  observeEvent(input$mzmsFile, {
    
    if (!LightVersion) {
      
      tryPath <- parseFilePaths(uploadFolder, input$mzmsFile)[1,]$datapath
      
      # Remove specific directory path information when macOS or Linux
      if (Sys.info()["sysname"] %in% c("Darwin", "Linux")) {
        tryPath <- gsub("/Volumes/Macintosh HD", "", tryPath)}
      if (is.na(tryPath)) {msPath(NULL)} else {
        msPath(tryPath)
        
        # Add ID and FASTA file if they exist
        testID <- gsub(".mzML|.mzXML|.raw|.mzml|.mzxml", ".mzid", tryPath) 
        if (file.exists(testID)) {idPath(testID)}
        testFASTA <- gsub(".mzML|.mzXML|.raw|.mzml|.mzxml", ".fasta", tryPath)
        if (file.exists(testFASTA)) {fastaPath(testFASTA)}
      }
      
    } else {
      msPath(input$mzmsFile$datapath)
    }
    
  }),
  
  # Let the user know which file will be used in the app and whether the manally inputted file is an acceptable path or not
  output$msUpload <- renderText({
    
    # If there is not msPath
    if (is.null(msPath())) {
      
      # If this is not the PSpecteR Light version
      if (!LightVersion) {
        if (nchar(input$mzmsHandle) == 0) {paste("No MS file uploaded")} else
        if (file.exists(input$mzmsHandle) & grepl(".mzML|.mzXML|.raw|.h5", tail(unlist(strsplit(input$mzmsHandle, "/")), 1))) {
            paste(checkmark, HTML(paste("<strong>", input$mzmsHandle, "</strong>")), "is a valid MS file path.")
        } else {
            paste(xmark, HTML(paste("<strong>", input$mzmsHandle, "</strong>")), "is not a valid MS file path.")
        }
      } else {
        paste("No MS file uploaded")
      }
    
    } else {
      if (!is.null(input$testBU) & !is.null(input$testTD)) {
        if (!LightVersion | input$testBU | input$testTD) {paste(msPath())} else {input$mzmsFile$name}
      } else {
        if (!LightVersion) {paste(msPath())} else {input$mzmsFile$name}
      }
    }
  
  }),
  
  # If button is clicked and the path is valid, then lock in this variable's path
  observeEvent(input$mzmsHandleGo, {
    

    # Set the message to a warning
    alert <- sendSweetAlert(session, "MS File Path Error", "Input file must exist 
             and be a .mzML, .mzXML, or .raw file.", "error") 
    
    # If the entered file is valid, inform user and set the msPath() to that file
    if (nchar(input$mzmsHandle) > 0 && file.exists(input$mzmsHandle) && 
        grepl(".mzML|.mzXML|.raw|.h5", tail(unlist(strsplit(input$mzmsHandle, "/")), 1))) {
      alert <- sendSweetAlert(session, "MS File Path Success!", "Input file accepted.", "success") 
      msPath(input$mzmsHandle)
      
      # Add ID and FASTA file if they exist
      testID <- gsub(".mzML|.mzXML|.raw|.h5", ".mzid", input$mzmsHandle)
      if (file.exists(testID)) {idPath(testID)}
      testFASTA <- gsub(".mzML|.mzXML|.raw|.h5", ".fasta", input$mzmsHandle)
      if (file.exists(testFASTA)) {fastaPath(testFASTA)}
    }
    
    # Send alert
    alert
  }),
  
  # Clear MS Path and File 
  observeEvent(input$mzmsHandleClear, {
    # When button is clicked, clear the stored path information and typed path
    updateTextInput(session, "mzmsHandle", value = "")
    msPath(NULL)
  }),
  
  ###############
  ## ID UPLOAD ##
  ###############
  
  # Set a reactive value for the protein id file path (idPath)
  idPath <- reactiveVal(value = NULL),
  
  # Specify the shiny file choose box
  if (!LightVersion) {
    
    shinyFileChoose(input, 'idFile', roots = uploadFolder, defaultPath = "/data/data",
                    filetypes = c("mzid", "mzID"))
    
  },
  
  # When choose button is clicked, determine what information is passed to idPath
  observeEvent(input$idFile, {
    
    if (!LightVersion) {
      tryPath <- parseFilePaths(uploadFolder, input$idFile)[1,]$datapath
      if (is.na(tryPath)) {idPath(NULL)} else {
        idPath(tryPath)
      }
    } else {
      idPath(input$idFile$datapath)
    }
    
  }),
  
  # Let the user know which file will be used in the app whether the manually inputed file is an acceptable path or not
  output$idUpload <- renderText({
    
    # If there is not an idPath
    if (is.null(idPath())) {
      
      # If this is not the light version
      if (!LightVersion) {
        if (nchar(input$idHandle) == 0) {paste("No ID file uploaded")} else
        if (file.exists(input$idHandle) & grepl(".mzid|.mzID", tail(unlist(strsplit(input$idHandle, "/")), 1))) {
          paste(checkmark, HTML(paste("<strong>", input$idHandle, "</strong>")), "is a valid ID file path.")
        } else {
            paste(xmark, HTML(paste("<strong>", input$idHandle, "</strong>")), "is not a valid ID file path.")
        }
      } else {
        paste("No ID file uploaded")
      }
    } else {
      if (!is.null(input$testBU) & !is.null(input$testTD)) {
        if (!LightVersion | input$testBU | input$testTD) {paste(idPath())} else {input$idFile$name}
      } else {
        if (!LightVersion) {paste(idPath())} else {input$idFile$name}
      }
    }
    
  }),
  
  # If button is clicked and the path is valid, then lock in this variable's path
  observeEvent(input$idHandleGo, {
    # Set the message to a warning
    alert <- sendSweetAlert(session, "ID File Path Error", "Input file must exist 
             and be a .mzid or .mzID.", "error") 
    # If the entered file is valid, inform user and set the idPath() to that file
    if (nchar(input$idHandle) > 0 && file.exists(input$idHandle) && 
        grepl(".mzid|.mzID", tail(unlist(strsplit(input$idHandle, "/")), 1))) {
      alert <- sendSweetAlert(session, "ID File Path Success!", "Input file accepted.", "success") 
      idPath(input$idHandle)
      
    }
    # Send alert
    alert
  }),
  
  # Clear ID Path and File 
  observeEvent(input$idHandleClear, {
    # When button is clicked, clear the stored path information and typed path
    updateTextInput(session, "idHandle", value = "")
    idPath(NULL)
  }),
  
  ##################
  ## FASTA UPLOAD ##
  ##################
  
  # Set a reactive value for the fasta file path (fastaPath)
  fastaPath <- reactiveVal(value = NULL),
  
  if (!LightVersion) {
    
    # Specify the shiny file choose box
    shinyFileChoose(input, 'fastaFile', roots = uploadFolder, defaultPath = "/data/data", 
                    filetypes = c("fa", "FA", "fasta", "FASTA")) 
    
  },
  
  # When choose button is clicked, determine which information is passed to fastaPath
  observeEvent(input$fastaFile, {
    
    if (!LightVersion) {
      tryPath <- parseFilePaths(uploadFolder,input$fastaFile)[1,]$datapath
      if (is.na(tryPath)) {fastaPath(NULL)} else {
         fastaPath(tryPath)
      }
    } else {
      fastaPath(input$fastaFile$datapath)
    }
  }),
  
  # Let the user know which file will be used in the app whether the manually inputed file is an acceptable path or not
  output$fastaUpload <- renderText({
    
    # If not FASTA file is uploaded
    if (is.null(fastaPath())) {
      
      # If this is not the light version
      if (!LightVersion) {
        if (nchar(input$fastaHandle) == 0) {paste("No FA file uploaded")} else
        if (file.exists(input$fastaHandle) & grepl(".fa|.FA|.fasta|.FASTA", tail(unlist(strsplit(input$fastaHandle, "/")), 1))) {
          paste(checkmark, HTML(paste("<strong>", input$fastaHandle, "</strong>")), "is a valid FA file path.")
        } else {
            paste(xmark, HTML(paste("<strong>", input$fastaHandle, "</strong>")), "is not a valid FA file path.")
        }
      } else {
      paste("No FA file uploaded")
    }
    
    } else {
      if (!is.null(input$testBU) & !is.null(input$testTD)) {
        if (!LightVersion | input$testBU | input$testTD) {paste(fastaPath())} else {input$fastaFile$name}
      } else {
        if (!LightVersion) {paste(fastaPath())} else {input$fastaFile$name}
      }
    }
    
  }),
  
  # If button is clicked and the path is valid, then lock in this variable's path
  observeEvent(input$fastaHandleGo, {
    # Set the message to a warning
    alert <- sendSweetAlert(session, "FA File Path Error", "Input file must exist 
             and be a .fa, .FA, .fasta, or .FASTA file.", "error") 
    # If the entered file is valid, inform user and set the fastaPath() to that file
    if (nchar(input$fastaHandle) > 0 && file.exists(input$fastaHandle) && 
        grepl(".fa|.FA|.fasta|.FASTA", tail(unlist(strsplit(input$fastaHandle, "/")), 1))) {
      alert <- sendSweetAlert(session, "FA File Path Success!", "Input file accepted.", "success") 
      fastaPath(input$fastaHandle)
    }
    # Send alert
    alert
  }),
  
  # Clear FA Path and File 
  observeEvent(input$fastaHandleClear, {
    # When button is clicked, clear the stored path information and typed path
    updateTextInput(session, "fastaHandle", value = "")
    fastaPath(NULL)
  }),
  
  ####################
  ## TEST FILE DATA ##
  ####################
  
  # Let user know which test files they're using.
  output$testUpload <- renderText({
    if (is.null(input$testBUSWITCH) == F && input$testBUSWITCH == T) {"<br><br><br>Using Bottom-Up Test Files."}
    if (is.null(input$testTDSWITCH) == F && input$testTDSWITCH == T) {"<br><br><br>Using Top-Down Test Files."}  
  }),
  
  # Render Bottom Up Test File Switch
  output$testBUSWITCH <- renderUI({
    BUTF <- materialSwitch("testBU", label = HTML("<strong>Use Bottom Up Test Files</strong>"), 
              value = F, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(BUTF, "Test with bottom up data?",
             "Test app with only bottom up data. Use these files to test MSGF+.",
             options = list(selector = '.material-switch'), placement = 'right')
    } else {BUTF}
  }),
  
  # If the Bottom-Up Test File switch is enabled, load the appropriate data
  observeEvent(input$testBU, {
    if (input$testBU == T) {
      if (LightVersion) {
        msPath(file.path("TestFiles", "BottomUp", "BottomUp.mzML"))
        idPath(file.path("TestFiles", "BottomUp", "BottomUp.mzid"))
        fastaPath(file.path("TestFiles", "QC_Shew.fasta"))
      } else {
        msPath("./data/TestFiles/BottomUp/BottomUp.mzML")
        idPath("./data/TestFiles/BottomUp/BottomUp.mzid")
        fastaPath("./data/TestFiles/QC_Shew.fasta")
      }
      disable("testTD")
    } else {
      msPath(NULL)
      idPath(NULL)
      fastaPath(NULL)
      enable("testTD")
    }
  }),
  
  # Render "Test RAW" Switch
  output$testTDSWITCH <- renderUI({
    TDTF <- materialSwitch("testTD", label = HTML("<strong>Use Top Down Test Files</strong>"), 
             value = F, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(TDTF, "Test with top down data?",
             "Test app with only top down data. Use these files to test MSPathFinder.",
             options = list(selector = '.material-switch'), placement = 'right')
    } else {TDTF}
  }),
  
  # Load only the raw file to demonstrate that it can be loaded
  observeEvent(input$testTD, {
    if (input$testTD == T) {
      if (LightVersion) {
        msPath(file.path("TestFiles", "TopDown", "TopDown.mzML"))
        idPath(file.path("TestFiles", "TopDown", "TopDown.mzid"))
        fastaPath(file.path("TestFiles", "QC_Shew.fasta"))
      } else {
        msPath(file.path("/data", "TestFiles", "TopDown", "TopDown.mzML"))
        idPath(file.path("/data", "TestFiles", "TopDown", "TopDown.mzid"))
        fastaPath(file.path("/data", "TestFiles", "QC_Shew.fasta"))
      }
      disable("testBU")
    } else {
      msPath(NULL)
      idPath(NULL)
      fastaPath(NULL)
      enable("testBU")
    }
  })
  
)