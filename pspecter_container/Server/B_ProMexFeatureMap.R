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
   tryPath <- parseFilePaths(uploadFolder, input$ms1ftFile)[1,]$datapath
   
   # Remove specific directory path information when macOS or Linux
   if (Sys.info()["sysname"] %in% c("Darwin", "Linux")) {
      tryPath <- gsub("/Volumes/Macintosh HD", "", tryPath)
   }
   ifelse(is.na(tryPath), ms1ftPath(NULL), ms1ftPath(tryPath))
  }),
  
  # Add a warning if the MS1FT file for whether the file is loaded
  output$ms1ftWarn <- renderText({
    if (is.null(ms1ftPath())) {
      if (nchar(input$ms1ftHandle) == 0) {paste("No MS1FT file uploaded")} else
      if (file.exists(input$ms1ftHandle) & grepl(".ms1ft|.MS1FT", tail(unlist(strsplit(input$ms1ftHandle, "/")), 1))) {
        paste(checkmark, HTML(paste("<strong>", input$ms1ftHandle, "</strong>")), "is a valid MS1FT file path.")} else {
        paste(xmark, HTML(paste("<strong>", input$ms1ftHandle, "</strong>")), "is not a valid MS1FT file path.")}}
      else {paste("")}
  }),
  
  # If button is clicked and the path is valid, then lock in this variable's path
  observeEvent(input$ms1ftHandleGo, {
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
    
    # If no MS1FT file, let the user know
    if (is.null(ms1ftPath())) {""} else {
    
     # Get protein file name
     proteinData <- gsub(".ms1ft", "_IcTarget.tsv", ms1ftPath())
     
     if (is.null(checkFile(proteinData)) == F) {paste("")} else {
       paste(xmark, proteinData, "is not in the same folder as the MS1FT file.")}   
    }
  }),
  
  ####################
  ## RENDER WIDGETS ##
  ####################
  
  # Render "Use MS1FT Test Files?" switch
  output$testMS1FTSWITCH <- renderUI({
    MSFT <- materialSwitch("testMS1FT", label = HTML("<strong>Use MS1FT Test File?</strong>"), 
              value = F, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(MSFT, "Use MS1FT Test File?", "Test ProMex Feature Map with a MS1FT test file.",
             options = list(selector = '.material-switch'), placement = 'right')
    } else {MSFT}
  }), 
  
  # Render the "Select Proteins" picker input
  output$PMproteinChoose <- renderUI({
  
    PMFM <- getPMFM()
    if (is.null(PMFM)) {return(NULL)}
    
    # Unique and ordered
    orderedProt <- unique(PMFM$ProteinName)[order(unique(PMFM$ProteinName))]
    
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
    if (input$testMS1FT == T) {
      ms1ftPath(file.path("/data", "data", "TestFiles", "TopDown", "TopDown.ms1ft"))}
  }),
  
  # Plot ProMex Feature Map
  output$PMFM <- renderPlotly({
    
    # If no MS1FT file has been uploaded, then no graph will be generated
    PMFM <- getPMFM()
    if (is.null(PMFM)) {return(NULL)}
    
    # Subset PMFM layers if picker input "Filter by Protein" exists
    if (is.null(input$PMselectProtein) == F) {
      PMFM <- PMFM[PMFM$ProteinName %in% input$PMselectProtein,]
    }
    
    # Add a "Grouping" variable to PMFM with one variable per group, for plotting
    PMFM$Grouping <- as.factor(c(1:nrow(PMFM)))
    
    # Melt PMFM dataframe by elution time for faster plotting
    PMFM <- PMFM %>% melt(id.vars = c("FeatureID", "MonoMass", "Log10Abundance", 
              "Color", "ProteinName", "Grouping")) %>% group_by(Grouping)

    # Define the legend title image (will only work in Shiny app)
    image <- list(list(source = paste("data:image/png;base64,", 
                base64enc::base64encode(file.path("www", "Abundance_Scale.png")), sep = ""), 
                xref = "paper", yref = "paper", x = 0.9, y = 1, sizex = 0.5, sizey = 0.5, 
                opacity = 0.6, layer = "above"))

    # Make plotly
    p <- plot_ly(data = PMFM, x = ~value, y = ~MonoMass, hoverinfo = "text",
      type = "scatter", mode = "lines", color = ~Color, colors = ColorList[PMFM$Color],
      showlegend = F, text = paste("Elution Time:", round(PMFM$value, 2), "<br>Monoisotopic Mass:", 
      round(PMFM$MonoMass, 2), "<br>Log10 Abundance:", round(PMFM$Log10Abundance, 4), 
      "<br>Protein:", PMFM$ProteinName)
    ) %>% layout(xaxis = list(title = "Elution Time [Minutes]"), 
                 yaxis = list(title = "Monoisotopic Mass [Da]"),
                 title = paste(gsub("\\.[^.]*$", "", tail(unlist(strsplit(ms1ftPath(), "/")), 1))),
                 images = image)
    
    # Store the current plot
    plots$currPMFM <- p
    
    plotly::toWebGL(p)

  }),
  
  # ProMex datatable
  output$ms1ftTab <-  DT::renderDataTable({
    datatable(getPMFMTable(), 
              rownames = F, filter = 'top', options = list(pageLength = 5),
              selection = list(mode = 'single'))
  })
)