## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2021_01_08

# DESCRIPTION: This code contains all the functionality for all image export button functions

list(
  
######################
## GETTER FUNCTIONS ##
######################

# Get exImage Dataframe
getExImages <- reactive({
  if (length(revals$imgData) >= 1) {
    Names <- as.character(names(revals$imgData))
    exImages <- data.frame("Row" = 1:length(Names), "Image.Name" = Names, stringsAsFactors = FALSE)
    return(exImages)} else {return(NULL)}
}),

###############
## FUNCTIONS ##
###############

applyPlotSizeChanges <- function(plot, plotName) {
  
  if (plotName %in% plots$plotSizes$PlotName) {
    Size <- plots$plotSizes[plots$plotSizes$PlotName == plotName, "Size"]
    Size <- unlist(strsplit(Size, " "))
    plot$x$layout$width <- as.numeric(Size[1])
    plot$x$layout$height <- as.numeric(Size[2])
  }
  
  return(plot)
},
  
# If there are images to export, have the modal box appear. Otherwise, a warning.  
observeEvent(input$exportIMG, {
  
  # If no plots, then send warning
  if (length(revals$imgData) < 1) {
    sendSweetAlert(session, title = "No plots to export", type = "error")
  } else {
    showModal(modalDialog(fluidPage(
      
      # Visualize the graphic
      plotlyOutput("exIMAGE", width = "100%", height = "250px")
        %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c")),
      
      # Use the table to select plotly graphics
      DT::dataTableOutput("exTABLE", width = "100%", height = "250px")
      
    ),
    title = HTML('<p style="text-align: center;">Select Images to Export</p>'),
    footer = list(
      column(4, uiOutput("exWidthUI")), 
      column(4, uiOutput("exHeightUI")),
      column(4, actionButton("exSizeChange", "Apply New Size")), 
      column(12, hr()),
      column(12, actionButton("delALLIMG", "Delete All"),
        actionButton("delIMG", "Delete Image"),
        downloadButton("sndPNG", "Make PNG"),
        downloadButton("sndJPG", "Make JPG"),
        downloadButton("sndHTML", "Make HTML"),
        modalButton("Exit"))),
    size = "l", easyClose = T))}
}),

# Length of export image
output$exWidthUI <- renderUI({
  
  # Get row click
  clicked <- input$exTABLE_row_last_clicked
  if (is.null(clicked)) {clicked <- 1}
  
  # Get plotName and see if plot sizes have been saved for this image
  plotName <- getExImages()[clicked, "Image.Name"]
  if (plotName %in% plots$plotSizes$PlotName) { 
    sizes <- plots$plotSizes[as.character(plots$plotSizes$PlotName) == plotName, "Size"]
    width <- as.numeric(unlist(strsplit(as.character(sizes), " "))[1])
  } else {
    plot <- revals$imgData[[plotName]]; width <- plot$x$layout$width
  }
  
  # Output the width
  numericInput("exWidth", "Width (in pixels)", width)
  
}),

# Width of export image
output$exHeightUI <- renderUI({
  
  # Get row click
  clicked <- input$exTABLE_row_last_clicked
  if (is.null(clicked)) {clicked <- 1}
  
  # Get plotName and see if plot sizes have been saved for this image
  plotName <- getExImages()[clicked, "Image.Name"]
  if (plotName %in% plots$plotSizes$PlotName) { 
    sizes <- plots$plotSizes[as.character(plots$plotSizes$PlotName) == plotName, "Size"]
    height <- as.numeric(unlist(strsplit(as.character(sizes), " "))[2])
  } else {
    plot <- revals$imgData[[plotName]]; height <- plot$x$layout$height
  }
  
  # Output the height
  numericInput("exHeight", "Height (in pixels)", height)

}),

# Apply changes in height and width
observeEvent(input$exSizeChange, {
  
  # Get row click
  clicked <- input$exTABLE_row_last_clicked
  if (is.null(clicked)) {clicked <- 1}
  
  # Width and height
  width <- input$exWidth
  height <- input$exHeight
  
  # Keep records of all plot size changes (this is done since actively changing 
  # plot size will distort the app)
  plotName <- getExImages()[clicked, "Image.Name"]
  newData <- data.frame("PlotName" = plotName, "Size" = paste(width, height),
               stringsAsFactors = F)
  if (plotName %in% plots$plotSizes$PlotName) {
    plots$plotSizes[as.character(plots$plotSizes$PlotName) == plotName, "Size"] <- newData$Size
  } else {plots$plotSizes <- rbind(plots$plotSizes, newData)}
  
}),

# Delete all images from the table
observeEvent(input$delALLIMG, {
  revals$imgData[c(1:length(revals$imgData))] <- NULL
  plots$index <- 0
  removeModal()
}),

# Delete selected image from table
observeEvent(input$delIMG, {
  clicked <- input$exTABLE_row_last_clicked
  if (is.null(clicked)) {return(NULL)} else {
    revals$imgData[[clicked]] <- NULL}
}),

# Reset all current 2. Scan & Seq plots when new scan is clicked
observeEvent(input$ssScan_row_last_clicked, {
  
  # Get active tabs
  tabs <- c(input$SStabs, input$PTBtabs, input$SSAtabs)
  
  # Clear all plots not in tabs
  if ("MS/MS" %in% tabs == F) {plots$currSPEC <- NULL}
  if ("Error Map" %in% tabs == F) {plots$currHM <- NULL}
  if ("XIC" %in% tabs == F)  {plots$currXIC <- NULL}
  if ("MS1" %in% tabs == F) {plots$currMPPRE <- NULL} 
  if ("Next MS1" %in% tabs == F) {plots$currMPNEXT <- NULL}
  if ("Bar" %in% tabs == F) {plots$currSSBAR <- NULL}
  if ("Sequence" %in% tabs == F) {plots$currFLAG <- NULL}
  
}),

# Reset all current 4. Protein Tree plots when new row is clicked
observeEvent(input$PTTable_row_last_clicked, {
  plots$currMATCH <- plots$currPTBAR <- plots$currLSEQ <- NULL
}),

# Create an image name for SS (done to prevent having highly repetive code)
getSSImgName <- reactive({
  scanNum <- 0
  if (is.null(GET_scan_metadata()) == F) {
    scan <- GET_scan_metadata()
    clicked <- GET_scan_click()
    scanNum <- scan[clicked, 2]}
  return(paste(plots$index, "_Scan_", scanNum, "_Tol_", input$ssTolerance, "ppm", sep = ""))
}),

# Create an image name for MS1
getMPImgName <- reactive({
  scanNum <- 0
  Pre.MZ <- 0
  if (is.null(GET_scan_metadata()) == F) {
    scan <- GET_scan_metadata()
    clicked <- GET_scan_click()
    scanNum <- scan[clicked, 2]
    Pre.MZ <- scan[clicked, 8]
  }
  return(paste(plots$index, "_Scan_", scanNum, "_PreMZ_", Pre.MZ,
               "_Window_", input$MPwindow, "mz", sep = ""))
}),

# Create an image name of PT
getPTImgName <- reactive({
  protein <- "protein"
  if (is.null(GET_protein_ID()) == F) {
    clicked <- input$PTTable_row_last_clicked
    if (is.null(clicked)) {clicked <- 1}
    PTID <- GET_protein_ID()
    protein <- as.character(PTID[clicked, 1])}
  return(paste(plots$index, "_Protein_", protein, sep = ""))
}),

# Add Spectra to exportable table
observeEvent(input$imgSPEC, {
  
  if (is.null(plots$currSPEC)) {
    sendSweetAlert(session, title = "No Spectrum to Export", type = "error")} else {
      
    # Modify the spectra x and y range before it is added
    showModal(modalDialog(fluidPage(
      
      plotlyOutput("exSPEC", width = "100%", height = "250px") %>% 
        withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c")),
      
      column(12, uiOutput("ssSpecXRange")), 
      column(6, numericInput("ssSpecMin", "Min MZ", min(GET_peak_data()$`M/Z`), width = "100%")),
      column(6, numericInput("ssSpecMax", "Max MZ", max(GET_peak_data()$`M/Z`), width = "100%")),
      column(6, actionButton("ssSpecNewRange", "Update M/Z Slider")),
      column(6, actionButton("ssSpecMZreset", "Reset M/Z Slider")),
      column(12, hr()), 
      column(12, uiOutput("ssSpecYRange")),
      column(6, numericInput("ssIntMin", "Min Int", min(GET_peak_data()$Intensity), width = "100%")),
      column(6, numericInput("ssIntMax", "Max Int", max(GET_peak_data()$Intensity), width = "100%")),
      column(6, actionButton("ssSpecNewINT", "Update Intensity Slider")),
      column(6, actionButton("ssSpecINTreset", "Reset Intensity Slider"))),
      
      title = HTML('<p style="text-align: center;">Select <em><strong>MZ & Intensity Ranges</strong></em> 
                 for the spectra</p>'),
      footer = list(
        actionButton("ssSpecAdd", "Add Spectra"), modalButton("Exit")),
      size = "l", easyClose = F))}
}),

# Graph spectra in modal box
output$exSPEC <- renderPlotly({
  if (is.null(GET_peak_data())) {return(NULL)}
  if (nrow(GET_peak_data()) > 10000) {
    sendSweetAlert(session, "Export Spectra Warning", 
                   "Spectra exportation limited to plots with no more than 10,000 peaks.",
                   type = "warning")
    return(NULL)
  } else {
    p <- plots$currSPEC %>% 
      layout(xaxis = list(range = c(min(input$ssSpecX), max(input$ssSpecX))),
             yaxis = list(range = c(min(input$ssSpecY), max(input$ssSpecY))))
    plotly::toWebGL(p)
  }
}),

# Reset Spectra MZ
observeEvent(input$ssSpecMZreset, {
  peak <- GET_peak_data()
  if (is.null(peak)) {return(NULL)}
  highest <- round(max(peak$mz), 3)
  updateSliderInput(session, "ssSpecX", value = c(0, highest))
}),

# Reset Intensity MZ
observeEvent(input$ssSpecINTreset, {
  peak <- GET_peak_data()
  if (is.null(peak)) {return(NULL)}
  highest <- round(max(peak$intensity))
  updateSliderInput(session, "ssSpecY", value = c(0, highest))
}),

# Add spectra if button is clicked
observeEvent(input$ssSpecAdd, {
  
  plots$index <- plots$index + 1
  plots$currSPEC <- plots$currSPEC %>% 
    layout(xaxis = list(range = c(min(input$ssSpecX), max(input$ssSpecX))),
           yaxis = list(range = c(min(input$ssSpecY), max(input$ssSpecY))))
  name <- paste(getSSImgName(), "_Spectrum", sep = "")
  revals$imgData[[name]] <- plots$currSPEC
  removeModal()
}),

# Add heatmap to exportable table
observeEvent(input$imgHM, {
  if (is.null(plots$currHM)) {
    sendSweetAlert(session, title = "No Error Map to Export", type = "error")} else {
    sendSweetAlert(session, title = "Error Map Snapshot", 
      "Click 'Export Snapshot Images' in the right hand corner to see plots.", type = "success")             
    plots$index <- plots$index + 1
    name <- paste(getSSImgName(), "_Heatmap", sep = "")
    revals$imgData[[name]] <- plots$currHM} 
}),

# Add XIC to exportable table
observeEvent(input$imgXIC, {
  
  if (is.null(plots$currXIC)) {
    sendSweetAlert(session, title = "No XIC to Export", type = "error")} else {
      
      # Modify the spectra x and y range before it is added
      showModal(modalDialog(fluidPage(
        
        plotlyOutput("exXIC", width = "100%", height = "250px") %>% 
          withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c")),
        
        column(12, uiOutput("XICxRange")), 
        column(6, numericInput("XICminRT", "Min RT", NULL, width = "100%")),
        column(6, numericInput("XICmaxRT", "Max RT", NULL, width = "100%")),
        column(6, actionButton("XICnewRT", "Update RT Slider")),
        column(6, actionButton("XICresetRT", "Reset RT Slider")),
        column(12, hr()), 
        column(12, uiOutput("XICyRange")),
        column(6, numericInput("XICminINT", "Min Int", NULL, width = "100%")),
        column(6, numericInput("XICmaxINT", "Max Int", NULL, width = "100%")),
        column(6, actionButton("XICnewINT", "Update Intensity Slider")),
        column(6, actionButton("XICresetINT", "Reset Intensity Slider"))),
        
        title = HTML('<p style="text-align: center;">Select 
                     <em><strong>Retention Time and Intensity Ranges</strong></em> 
                     for the spectra</p>'),
        footer = list(actionButton("XICAdd", "Add XIC"), modalButton("Exit")),
        size = "l", easyClose = F))}  
}),

# Graph XIC in modal box
output$exXIC <- renderPlotly({
  plots$currXIC %>% 
    layout(xaxis = list(range = c(min(input$XICx), max(input$XICx))),
           yaxis = list(range = c(min(input$XICy), max(input$XICy))))
}),

# Reset Spectra MZ
observeEvent(input$XICresetRT, {
  XIC <- GET_XIC()
  if (is.null(XIC)) {return(NULL)}
  highest <- round(max(XIC$RT), 3)
  updateSliderInput(session, "XICx", value = c(0, highest))
}),

# Reset Intensity MZ
observeEvent(input$XICresetINT, {
  XIC <- GET_XIC()
  if (is.null(XIC)) {return(NULL)}
  highest <- round(max(XIC$Intensity))
  updateSliderInput(session, "XICy", value = c(0, highest))
}),

# Add spectra if button is clicked
observeEvent(input$XICAdd, {
  plots$index <- plots$index + 1
  plots$currXIC <- plots$currXIC %>% 
    layout(xaxis = list(range = c(min(input$XICx), max(input$XICx))),
           yaxis = list(range = c(min(input$XICy), max(input$XICy))))
  name <- paste(getSSImgName(), "_XIC", sep = "")
  revals$imgData[[name]] <- plots$currXIC
  removeModal()
}),

# Add SSBAR to exportable table
observeEvent(input$imgSSBAR, {
  if (is.null(plots$currSSBAR)) {
    sendSweetAlert(session, title = "No Bar Chart to Export", type = "error")} else {
    sendSweetAlert(session, title = "Bar Chart Snapshot", 
      "Click 'Export Snapshot Images' in the right hand corner to see plots.", type = "success")
    plots$index <- plots$index + 1
    name <- paste(getSSImgName(), "_SSBar", sep = "") 
    revals$imgData[[name]] <- plots$currSSBAR} 
}),

# Add Flag Seq to exportable table
observeEvent(input$imgFLAG, {
  if (is.null(plots$currFLAG)) {
    sendSweetAlert(session, title = "No Sequence to Export", type = "error")} else {
    sendSweetAlert(session, title = "Sequence Snapshot", 
      "Click 'Export Snapshot Images' in the right hand corner to see plots.", type = "success")
    plots$index <- plots$index + 1
    name <- paste(getSSImgName(), "_Flag", sep = "")
    revals$imgData[[name]] <- plots$currFLAG} 
}),

# Add Previous MS1 to exportable table
observeEvent(input$imgMPPRE, {
  if (is.null(plots$currMPPRE)) {
    sendSweetAlert(session, title = "No Previous MS1 to Export", type = "error")} else {
    sendSweetAlert(session, title = "Previous MS1 Snapshot",
      "Click 'Export Snapshot Images' in the right hand corner to see plots.", type = "success")
    plots$index <- plots$index + 1
    name <- paste(getMPImgName(), "_PreviousMS1", sep = "")
    revals$imgData[[name]] <- plots$currMPPRE} 
}),

# Add Next MS1 to exportable table
observeEvent(input$imgMPNEXT, {
  if (is.null(plots$currMPNEXT)) {
    sendSweetAlert(session, title = "No Next MS1 to Export", type = "error")} else {
    sendSweetAlert(session, title = "Next MS1 Snapshot", 
       "Click 'Export Snapshot Images' in the right hand corner to see plots.", type = "success")
    plots$index <- plots$index + 1
    name <- paste(getMPImgName(), "_NextMS1", sep = "")
    revals$imgData[[name]] <- plots$currMPNEXT} 
}),

# Graph XIC in modal box
output$exMATCH <- renderPlotly({
  plots$currMATCH %>% 
    layout(xaxis = list(range = c(min(input$PTx), max(input$PTx))),
           yaxis = list(range = c(min(input$PTy), max(input$PTy))))
}),

# Reset Spectra MZ
observeEvent(input$PTposreset, {
  PT <- GET_protein_table()
  if (is.null(PT)) {return(NULL)}
  highest <- max(PT$Total)
  updateSliderInput(session, "PTx", value = c(0, highest))
}),

# Reset Intensity MZ
observeEvent(input$PTscanreset, {
  PT <- GET_protein_table()
  if (is.null(PT)) {return(NULL)}
  highest <- max(PT$Scan) + 1
  updateSliderInput(session, "PTy", value = c(0, highest))
}),

# Add spectra if button is clicked
observeEvent(input$PTAdd, {
  plots$index <- plots$index + 1
  plots$currMATCH <- plots$currMATCH %>% 
    layout(xaxis = list(range = c(min(input$PTx), max(input$PTx))),
           yaxis = list(range = c(min(input$PTy), max(input$PTy))))
  name <- paste(getSSImgName(), "_Match", sep = "")
  revals$imgData[[name]] <- plots$currMATCH
  removeModal()
}),

# Add Protein Bar to exportable table
observeEvent(input$imgPTBAR, {
  if (is.null(plots$currPTBAR)) {
    sendSweetAlert(session, title = "No Bar Chart to Export", type = "error")} else {
    sendSweetAlert(session, title = "Bar Chart Snapshot", 
      "Click 'Export Snapshot Images' in the right hand corner to see plots.", type = "success")
    plots$index <- plots$index + 1
    name <- paste(getPTImgName(), "_Bar", sep = "")
    revals$imgData[[name]] <- plots$currPTBAR} 
}),

# Add Literature Sequence to exportable table
observeEvent(input$imgLSEQ, {
  if (is.null(plots$currLSEQ)) {
    sendSweetAlert(session, title = "No Lit Sequence to Export", type = "error")} else {
      sendSweetAlert(session, title = "Lit Sequence Snapshot", 
        "Click 'Export Snapshot Images' in the right hand corner to see plots.", type = "success")
      plots$index <- plots$index + 1
      name <- paste(getPTImgName(), "_LitSeq", sep = "")
      revals$imgData[[name]] <- plots$currLSEQ} 
}),

# Add Spectra Metadata to exportable table
observeEvent(input$imgSM, {
  if (is.null(plots$currSM)) {
    sendSweetAlert(session, title = "No Spectra Metadata Plot to Export", type = "error")} else {
    sendSweetAlert(session, title = "Spectra Metadata Plot Snapshot", 
      "Click 'Export Snapshot Images' in the right hand corner to see plots.", type = "success")
    plots$index <- plots$index + 1
    SMname <- paste(plots$index, "_SpectraMetadata", sep = "")
    revals$imgData[[SMname]] <- plots$currSM} 
}),

# Add MS1 Feature Plot to exportable table
observeEvent(input$imgPMFM, {
  if (is.null(plots$currPMFM)) {
    sendSweetAlert(session, title = "No ProMex Feature Map to Export", type = "error")} else {
    sendSweetAlert(session, title = "ProMex Feature Plot Snapshot", 
      "Click 'Export Snapshot Images' in the right hand corner to see plots.", type = "success")
    plots$index <- plots$index + 1
    PMFMname <- paste(plots$index, "_ProMexFeaturePlot", sep = "")
    revals$imgData[[PMFMname]] <- plots$currPMFM} 
}),

# Generate the data table of all images
output$exTABLE <- DT::renderDataTable({
  exImages <- getExImages()
  if (is.null(exImages)) {return(NULL)} else {
    datatable(exImages, rownames = T, filter = 'top', options = list(pageLength = 5),
              selection = list(mode = 'single', scrollX = T))} 
}),

# Visualize image on table click
output$exIMAGE <- renderPlotly({
  exImages <- getExImages()
  if (is.null(exImages)) {return(NULL)} else {
    clicked <- input$exTABLE_row_last_clicked
    if (is.null(clicked)) {clicked <- 1}
    p <- revals$imgData[[clicked]]
    
    # Convert to webGL for plots that are spectra
    if (grepl("Spect", exImages[clicked, "Image.Name"])) {
      plotly::toWebGL(p)
    } else {p}
      
  }
}),

# Export data as PNG
output$sndPNG <- downloadHandler(
  filename = function() {
    basename <- "PSpecteR"
    if (is.null(msPath()) == F) {
      basename <- unlist(strsplit(unlist(strsplit(msPath(), "/"))[-1], ".", fixed = T))[1]
    }
    paste(basename, "_PNG_",format(Sys.time(), "%y%m%d_%H%M%S"), ".zip", sep = "")
  },
  content = function(fname) {
    
    # Start progress bar for download
    withProgress(message = "Exporting PNG", value = 0, {
      incProgress(0.1, "Starting PNG Export")
      
      # Keep track of plots to extract
      exPlots <- c()
      
      # Set one tempdir
      tmpdir <- tempdir()
      
      # For each image name, export plot to a tempdir
      for (img in names(revals$imgData)) {
        plot <- revals$imgData[[img]]
        plot <- applyPlotSizeChanges(plot, img)
        imgFilePath <- file.path(tmpdir, paste(img, ".png", sep = ""))
        exPlots <- c(exPlots, imgFilePath)
        export(plot, zoom = 3, file = imgFilePath)
        incProgress(1 / length(revals$imgData), paste("Exported", img))
      }
      
      # Export data from tempdir 
      zip(zipfile=fname, files=exPlots, flags="-j")
      
    })  
  },
  contentType = "application/zip"
),
  

# Export data as JPG
output$sndJPG <- downloadHandler(
  filename = function() {
    basename <- "PSpecteR"
    if (is.null(msPath()) == F) {
      basename <- unlist(strsplit(unlist(strsplit(msPath(), "/"))[-1], ".", fixed = T))[1]
    }
    paste(basename, "_JPG_",format(Sys.time(), "%y%m%d_%H%M%S"), ".zip", sep = "")
  },
  content = function(fname) {
    
    # Start progress bar for download
    withProgress(message = "Exporting JPG", value = 0, {
      incProgress(0.1, "Starting JPG Export")
      
      # Keep track of plots to extract
      exPlots <- c()
      
      # Set one tempdir
      tmpdir <- tempdir()
      
      # For each image name, export plot to a tempdir
      for (img in names(revals$imgData)) {
        plot <- revals$imgData[[img]]
        plot <- applyPlotSizeChanges(plot, img)
        imgFilePath <- file.path(tmpdir, paste(img, ".jpeg", sep = ""))
        exPlots <- c(exPlots, imgFilePath)
        export(plot, zoom = 3, file = imgFilePath)
        incProgress(1 / length(revals$imgData), paste("Exported", img))
      }
      
      # Export data from tempdir 
      zip(zipfile=fname, files=exPlots, flags="-j")
      
    })  
  },
  contentType = "application/zip"
),

# Export data as HTML
output$sndHTML <- downloadHandler(
  filename = function() {
    basename <- "PSpecteR"
    if (is.null(msPath()) == F) {
      basename <- unlist(strsplit(unlist(strsplit(msPath(), "/"))[-1], ".", fixed = T))[1]
    }
    paste(basename, "_HTML_",format(Sys.time(), "%y%m%d_%H%M%S"), ".html", sep = "")
  },
  content = function(file) {
    
    withProgress({
      
      incProgress(0.1, "Starting HTML Export")
      
      # Get R Markdown file
      RmdFile <- file.path("Server", "Export", "HTML_Export.Rmd")
      
      incProgress(0.3, "Collecing Parameter Data")
      
      # Put all the parameters to be passed to the markdown file into a list
      params <- list(htmlTitles = getExImages()$Image.Name,  
                     htmlFigs = revals$imgData)
      
      # Insert directory information for pandoc
      if (Sys.info()["sysname"] == "Windows") {
        Sys.setenv(RSTUDIO_PANDOC=file.path("C:", "Program Files", "Rstudio", "bin", "pandoc"))} else 
      if (Sys.info()["sysname"] == "Darwin") {
        Sys.setenv(RSTUDIO_PANDOC=file.path("", "Applications", "RStudio.app", "Contents", "MacOS", "pandoc"))
      }
      
      incProgress(0.3, "Writing HTML file")
      
      # Print Markdown
      rmarkdown::render(RmdFile, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
      incProgress(0.2, "Finished HTML file")
      
    })
  })
)