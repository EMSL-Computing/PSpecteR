## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_08_03

# DESCRIPTION: Contains all the functions to render the feature plot functions

list(
  
  ######################
  ## GLOBAL VARIABLES ##
  ######################
  
  # Define vector for X/Y data of metadata plot without ID
  XY_SM <- c("Scan Number" = "Scan Number", "Retention Time" = "Retention Time", 
             "Precursor M/Z" = "Precursor M/Z", "Precursor Scan" = "Precursor Scan"),
  
  # Define vector for X/Y data for metadata plot with ID
  XY_SM_ID <- c(XY_SM, "Calculated Mass" = "Calculated Mass", "Experimental Mass" = "Experimental Mass"),
  
  # Define vector for lab data without ID and one for data with ID
  LAB_SM <- c("MS Level" = "MS Level", "Precursor Charge" = "Precursor Charge"),
  LAB_SM_ID <- c(LAB_SM, "Score" = "Score", "Q Value" = "Q Value"),
  
  ###############
  ## RENDER UI ##
  ###############
  
  # Set scan number range 
  output$SMscannumUI <- renderUI({
    
    # Get scan data
    if (is.null(GET_scan_metadata())) {return(NULL)}
    scan <- GET_scan_metadata()
    
    # Set scan number range
    FSN <- sliderInput("SMscannum", "Scan Range", min(scan$`Scan Number`), max(scan$`Scan Number`),
                c(min(scan$`Scan Number`), max(scan$`Scan Number`)))
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(FSN, Desc[Desc$Name == "SMscannum", "Title"], Desc[Desc$Name == "SMscannum", "Description"])
    } else {FSN}
  }),
  
  # Set MS Level range
  output$SMmslevelUI <- renderUI({
    
    # Get scan data
    if (is.null(GET_scan_metadata())) {return(NULL)}
    scan <- GET_scan_metadata()
    
    # Set MS Level range
    FMS <- sliderInput("SMmslevel", "MS Level", min(scan$`MS Level`), max(scan$`MS Level`),
                c(min(scan$`MS Level`), max(scan$`MS Level`)), 1, T)
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(FMS, Desc[Desc$Name == "SMmslevel", "Title"], Desc[Desc$Name == "SMmslevel", "Description"])
    } else {FMS}
  }),
  
  # Have these parameters show up as options for the X variable
  output$xSMui <- renderUI({
    
    # Check for scan data
    if (is.null(GET_scan_metadata())) {return(NULL)}
    
    if (is.null(idPath())) {
       XSM <- selectInput("xSM", "Select X Variable", XY_SM, selected = "Precursor M/Z")} else {
       XSM <- selectInput("xSM", "Select X Variable", XY_SM_ID, selected = "Precursor M/Z")}
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(XSM, Desc[Desc$Name == "xSM", "Title"], Desc[Desc$Name == "xSM", "Description"])
    } else {XSM}
    
  }),
  
  # Hae these parameters show up as options for the Y variable
  output$ySMui <- renderUI({
    
    # Check for scan data
    if (is.null(GET_scan_metadata())) {return(NULL)}
    
    if (is.null(idPath())) {
       YSM <- selectInput("ySM", "Select Y Variable", XY_SM, selected = "Retention Time")} else {
       YSM <- selectInput("ySM", "Select Y Variable", XY_SM_ID, selected = "Retention Time")}
    if (is.null(input$infoMode) == F && input$infoMode == T) {
      popify(YSM, Desc[Desc$Name == "ySM", "Title"], Desc[Desc$Name == "ySM", "Description"])
    } else {YSM}
    
  }),
  
  # Have these parameters show up as option for the factor variable
  output$labSMui <- renderUI({
    
    # Check for scan data
    if (is.null(GET_scan_metadata())) {return(NULL)}
    
    if (is.null(idPath())) {
      LSM <- selectInput("labSM", "Select Labels", LAB_SM, selected = "Score")} else {
      LSM <- selectInput("labSM", "Select Labels", LAB_SM_ID, selected = "Score")}
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
    if (is.null(GET_scan_metadata())) {return(NULL)}
    
    # If null values return no plot
    if (is.null(xSM) | is.null(ySM) | is.null(labSM) | is.null(input$SMscannum) | 
        is.null(input$SMmslevel)) {
      return(NULL)
    }
    
    # Make interactive scan metadata plot
    SM <- scan_metadata_plot(
      ScanMetadata = GET_scan_metadata(),
      XVar = xSM,
      YVar = ySM, 
      LabVar = labSM,
      Interactive = TRUE,
      MSFilter = input$SMmslevel,
      ScanNumFilter = input$SMscannum
    )
    
    plots$currSM <- SM
  
    plotly::toWebGL(SM)
      
  })

)