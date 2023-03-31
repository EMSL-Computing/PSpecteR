## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains all the protein coverage tables and figures

list(
  
  ###############
  ## OBSERVERS ##
  ###############
  
  # Change PT X (Amino Acid) Values
  observeEvent(input$PTposNew, {
    updateSliderInput(session, "PTx", value = c(input$PTposMin, input$PTposMax))
  }),
  
  # Change XIC Y (intensity) Spectra Values
  observeEvent(input$PTscanNew, {
    updateSliderInput(session, "PTy", value = c(input$PTscanMin, input$PTscanMax))
  }),
  
  ####################
  ## RENDER WIDGETS ##
  ####################
  
  # Render "Remove Contaminants" switch
  output$PTContSWITCH <- renderUI({
    PC <- materialSwitch("PTCont", HTML("<strong>Remove Contaminants?</strong>"), value = T, status = "success")
    if (is.null(input$infoMode) == F && input$infoMode == T) {
    popify(PC, Desc[Desc$Name == "PTCont", "Title"], Desc[Desc$Name == "PTCont", "Description"],
           options = list(selector = '.material-switch'), placement = 'right')
    } else {PC}
  }),
  
    # Set the PT Amino Acid Position (X variable) Range
  output$PTxRange <- renderUI({
    
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    
    browser()
    
    
    PT <- getProteinTree()
    if (is.null(PT)) {return(NULL)}
    highest <- nchar(PT[PT$Scan == 1,]$Sequence) + 1
    sliderInput("PTx", "Amino Acid Position Range", 0, highest, c(0, highest), 0.01,
                width = "150%")
  }),
  
  # Set the PT Scan Number (Y variable) Range
  output$PTyRange <- renderUI({
    
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    
    browser()
    
    PT <- getProteinTree()
    if (is.null(PT)) {return(NULL)}
    highest <- max(PT$Scan, na.rm = T) + 1
    sliderInput("PTy", "Scan Number Range", 0, highest, c(0, highest), 1, width = "150%")
  }), 
  
  ##########################
  ## RENDER PLOTS / TABLE ##
  ##########################
  
  # Check to see if a FASTA file is loaded, since I keep forgetting to do this
  output$PTnoFAwarn <- renderText({
    if (is.null(msPath()) | is.null(idPath()) | is.null(fastaPath())) {
      HTML('<p><span style="font-size: 14pt; color: #46a716;">
           Remember to upload an MS, ID, & FASTA file to see Protein Coverage Graphs.</span></p')}
  }),

  # Creates table with identified protein data 
  output$PTTable <- DT::renderDataTable({
    
    # If no protein table, return NULL 
    if (is.null(GET_protein_table())) {return(NULL)}
    
    datatable(GET_protein_table(), rownames = F, filter = 'top', options = list(pageLength = 4),
              selection = list(mode = 'single', selected = 1))
  }),
  
  # Reveal protein sequence
  output$LSeq <- renderPlotly({
    
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    
    browser()
    
    proteinTree <- getProteinTree()
    
    # This is proteinTree
    litSeq <- tail(proteinTree, 1)$Sequence
    
    # Generate a dataframe for graphing identified sequences
    plotLSeq <- function(litSeq) {
      char <- c()
      y <- c()
      x <- c()
      LPlot <- for (i in 1:nchar(litSeq)) {
        char[i] <- substr(litSeq, i, i)
        xtest <- i %% 70
        if (xtest != 0) {x[i] <- xtest} else {x[i] <- 70}
        y[i] <- -((i + 69) %/% 70)
      }
      LPlot <- data.frame(x, y, char, 1:nchar(litSeq), F, stringsAsFactors = F)
      colnames(LPlot) <- c("X", "Y", "AA", "Num", "Color")
      LPlot$X <- as.numeric(LPlot$X)
      LPlot$Y <- as.numeric(as.character(LPlot$Y))
      LPlot$Num <- as.numeric(as.character(LPlot$Num))
      return(LPlot)
    }
    
    # Save dataframe as a variable and make sure variable type is correct
    LPlot <- plotLSeq(litSeq)
    
    # Get position number of each identified AA
    identified <- unique(lapply(1:(nrow(proteinTree) - 1), function(i) {
      proteinTree$MatchPos[i]:proteinTree$Total[i]}) %>% unlist())
    
    # Set identified positions to T
    LPlot[LPlot$Num %in% identified,]$Color <- T
    
    # Change plot T/F to colors
    LPlot$Color <- as.factor(LPlot$Color)
    levels(LPlot$Color)[levels(LPlot$Color) == T] <- "rgb(0, 128, 0)"
    levels(LPlot$Color)[levels(LPlot$Color) == F] <- "rgb(192, 192, 192)"
    
    # Define an empty axis
    ax <- list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, 
               showgrid = FALSE)
    
    # Split features into layers so they appear on the legend correctly
    green <- LPlot[LPlot$Color == "rgb(0, 128, 0)",]
    white <- LPlot[LPlot$Color == "rgb(192, 192, 192)",]
    
    # Determine size of text, shrinking one point font for every 500 characters
    textSize <- 12 - (nchar(litSeq) %/% 500)
    
    # Finally, plot the graphic
    p <- plot_ly(green, x = green$X, y = green$Y, mode = "text", text = ~green$AA, 
            type = "scatter", textfont = list(color = green$Color, size = textSize), 
            hoverinfo = "text", hovertext = paste("Pos:", green$Num), name = "In Scans") %>%
    add_trace(white, x = white$X, y = white$Y, mode = "text", text = ~white$AA,
              type = "scatter", textfont = list(color = white$Color, size = textSize), 
              hoverinfo = "text", hovertext = paste("Pos:", white$Num), name = "Not in Scans") %>%
    layout(xaxis = ax, yaxis = ax, plot_bgcolor = "rgb(0,0,0)")
    
    plots$currLSEQ <- p
    
    p
  }),
  
  # Make chart of protein tree matches
  output$PTMatch <- renderPlotly({
    
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    # Make the plotly
    p <- coverage_plot(
      PeptideCoverage = GET_peptide_coverage(),
      Interactive = T,
      ColorByScore = "Score"
    )
    
    plots$currMATCH <- p
    
    plotly::toWebGL(p)
    
  }),
  
  # Makes barchart of protein tree data 
  output$PTBar <- renderPlotly({
    
    # Return null if no peptide coverage
    if (is.null(GET_peptide_coverage())) {return(NULL)}
    
    
    browser()
    
    proteinTree <- getProteinTree()
    if (is.null(proteinTree)) {return(NULL)}
    
    PTID <- getPTID()
    if (is.null(PTID)) {return(NULL)}
    
    # Get clicked row
    clicked <- input$PTTable_row_last_clicked
    if (is.null(clicked)) {clicked = 1}
    
    # Turn the protein sequence into a dataframe to contain extraction information
    litSeq <- tail(proteinTree$Sequence, n = 1)
    proteinBar <- data.table(paste(strsplit(litSeq, "") %>% unlist(), 1:nchar(litSeq), 
                                   sep = ""), 
                             rep(0, nchar(litSeq)))
    colnames(proteinBar) <- c("Character", "Number")
    proteinBar$Character <- as.character(proteinBar$Character)
    
    # For each read, determine if there are any matches between identified peptides
    # and protein sequence data 
    for (i in 1:(nrow(proteinTree) - 1)) {
      seq <- proteinTree$Sequence[i]
      dir <- as.character(proteinTree$Dir[i])
      if (dir == "C to N") {seq <- reverse(seq)}
      pos <- proteinTree$MatchPos[i]
      AAs <- paste(strsplit(seq, "") %>% unlist(), pos:(nchar(seq) + pos - 1), sep = "")
      proteinBar[which(proteinBar$Character %in% AAs),]$Number <-
        proteinBar[which(proteinBar$Character %in% AAs),]$Number + 1
    }
    
    # Remove any lit seq amino acids that don't have any matches
    proteinBar <- subset(proteinBar, proteinBar$Number > 0)
    
    # Add row numbers for specific plotting order
    proteinBar <- data.frame(1:nrow(proteinBar), proteinBar)
    colnames(proteinBar) <- c("Position", "Character", "Number")
    
    p <- plot_ly(proteinBar, x = proteinBar$Position, y = proteinBar$Number, type = "bar",
            marker = list(color = "rgb(0,0,0)", line = list("rgb(0,0,0)")), 
            hoverinfo = "text", hovertext = paste("AA & Pos: ", proteinBar$Character,
            "<br>Count: ", proteinBar$Number, sep = "")) %>% 
      layout(xaxis = list(title = "Amino Acid & Position", tickvals = proteinBar$Position,
            ticktext = proteinBar$Character), yaxis = list(title = "Frequency"),
            title = paste("ID:", PTID[clicked, 1]))
    
    plots$currPTBAR <- p
    
    p
  }),
  
  # Print Q Value Warning
  output$QValWarn <- renderText({
    
    scan <- GET_scan_metadata()
    if (is.null(scan)) {return(NULL)}
    
    # If all the Q-Values are NA give this warning
    if ((FALSE %in% unique(is.na(scan$`Q Value`))) == F) {
      HTML('<strong><span style="color: rgb(184, 49, 47); background-color: rgb(247, 218, 100);">
           No Q-Values provided. Filtering disabled. </span></strong>')
    } else {return(NULL)}
    
  })

)