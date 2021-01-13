## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_08_03

# DESCRIPTION: Contains all the functions to render the feature plot functions

list(
  
  ######################
  ## GLOBAL VARIABLES ##
  ######################
  
  # Define vector for X/Y data of metadata plot without ID
  XY_SM <- c("Scan" = "Scan", "Retention Time" = "Retention Time", 
             "Precursor M/Z" = "Precursor M/Z", "Precursor Scan" = "Precursor Scan"),
  
  # Define vector for X/Y data for metadata plot with ID
  XY_SM_ID <- c(XY_SM, "Mass" = "Mass"),
  
  # Define vector for lab data without ID and one for data with ID
  LAB_SM <- c("MS Level" = "MS Level", "Charge" = "Charge"),
  LAB_SM_ID <- c(LAB_SM, "Score" = "Score", "Q Value" = "Q Value"),
  
  ###############
  ## RENDER UI ##
  ###############
  
  # Set scan number range 
  output$SMscannumUI <- renderUI({
    
    # Get scan data
    if (is.null(getScan())) {return(NULL)}
    scan <- getScan()
    
    # Set scan number range
    FSN <- sliderInput("SMscannum", "Scan Range", min(scan$Scan.Num), max(scan$Scan.Num),
                c(min(scan$Scan.Num), max(scan$Scan.Num)))
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(FSN, Desc[Desc$Name == "SMscannum", "Title"], Desc[Desc$Name == "SMscannum", "Description"])
    } else {FSN}
  }),
  
  # Set MS Level range
  output$SMmslevelUI <- renderUI({
    
    # Get scan data
    if (is.null(getScan())) {return(NULL)}
    scan <- getScan()
    
    # Set MS Level range
    FMS <- sliderInput("SMmslevel", "MS Level", min(scan$MS.Level), max(scan$MS.Level),
                c(min(scan$MS.Level), max(scan$MS.Level)), 1, T)
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(FMS, Desc[Desc$Name == "SMmslevel", "Title"], Desc[Desc$Name == "SMmslevel", "Description"])
    } else {FMS}
  }),
  
  # Have these parameters show up as options for the X variable
  output$xSMui <- renderUI({
    ID <- getID()
    if (is.null(ID)) {
       XSM <- selectInput("xSM", "Select X Variable", XY_SM, selected = "Precursor M/Z")} else {
       XSM <- selectInput("xSM", "Select X Variable", XY_SM_ID, selected = "Precursor M/Z")}
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(XSM, Desc[Desc$Name == "xSM", "Title"], Desc[Desc$Name == "xSM", "Description"])
    } else {XSM}
  }),
  
  # Hae these parameters show up as options for the Y variable
  output$ySMui <- renderUI({
    ID <- getID()
    if (is.null(ID)) {
       YSM <- selectInput("ySM", "Select Y Variable", XY_SM, selected = "Retention Time")} else {
       YSM <- selectInput("ySM", "Select Y Variable", XY_SM_ID, selected = "Retention Time")}
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(YSM, Desc[Desc$Name == "ySM", "Title"], Desc[Desc$Name == "ySM", "Description"])
    } else {YSM}
  }),
  
  # Have these parameters show up as option for the factor variable
  output$labSMui <- renderUI({
    ID <- getID()
    if (is.null(ID)) {
      LSM <- selectInput("labSM", "Select Labels", LAB_SM, selected = "MS Level")} else {
      LSM <- selectInput("labSM", "Select Labels", LAB_SM_ID, selected = "MS Level")}
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(LSM, Desc[Desc$Name == "labSM", "Title"], Desc[Desc$Name == "labSM", "Description"])
    } else {LSM}
  }),

  #################
  ## RENDER PLOT ##
  #################
  
  output$SMplot <- renderPlotly({
    
    # Get X, Y, and label metadata plot values
    xSM <- input$xSM
    ySM <- input$ySM
    labSM <- input$labSM
    
    # Get scan dataframe and subset data 
    scan <- getScan()
    if (is.null(scan)) {return(NULL)}
    
    # If null values return no plot
    if (is.null(xSM) | is.null(ySM) | is.null(labSM) | is.null(input$SMscannum) | 
        is.null(input$SMmslevel)) {
      return(NULL)
    }
    
    # Subset data by each of the filters
    scan <- scan[scan$Scan.Num >= min(input$SMscannum) & scan$Scan.Num <= max(input$SMscannum),]
    scan <- scan[scan$MS.Level >= min(input$SMmslevel) & scan$MS.Level <= max(input$SMmslevel),]
    
    # Assert proper data types
    scan$MS.Level <- as.factor(scan$MS.Level)
    scan$Pre.Charge <- as.factor(scan$Pre.Charge)
    
    colnames(scan)[1:13] <- c("Order", "Scan", "MS Level", "Retention Time", 
                              "Precursor M/Z", "Charge", "Precursor Scan",
                              "Activation Method", "Sequence", "Protein ID", "Mass",
                              "Score", "Q Value")
    
    # Generate metadata plot
    legendtitle <- list(yref = "paper", xref = "paper", y = 1.05, x = 1.1,
                        text = labSM, showarrow = F)
    
    # Set NA values to 0
    xVals <- lapply(scan[[xSM]], function(el) {if (is.na(el)) {return(0)} else{return(el)}}) %>% unlist()
    yVals <- lapply(scan[[ySM]], function(el) {if (is.na(el)) {return(0)} else{return(el)}}) %>% unlist()
    labVals <- lapply(scan[[labSM]], function(el) {if (is.na(el)) {return(0)} else{return(el)}}) %>% unlist()
    
    SM <- plot_ly(scan, x = xVals, y = yVals, type = "scatter", 
            mode = "markers", color = labVals, hoverinfo = "text",
            hovertext = paste(xSM, ": ", round(xVals, 3), "<br>",
                              ySM, ": ", round(yVals, 3), "<br>",
                              labSM, ": ", labVals, sep = "")) %>%
      layout(xaxis = list(title = xSM),  yaxis = list(title = ySM),
             annotations = legendtitle)
    
    plots$currSM <- SM
  
    plotly::toWebGL(SM)
      
  })

)