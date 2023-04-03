## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains all the functions to append the Glossary 

list( 

  ######################
  ## GLOBAL VARIABLES ##
  ######################
  
  # All modified amino acids users can select from
  modSites <- c("Alanine" = "A", "Arginine" = "R", "Asparagine" = "N", "Aspartic acid" = "D", 
                "Cysteine" = "C", "Glutamine" = "Q", "Glutamic acid" = "E", "Glycine" = "G", 
                "Histidine" = "H", "Isoleucine" = "I", "Leucine" = "L", "Lysine" = "K", 
                "Methionine" = "M", "Phenylalanine" = "F", "Proline" = "P", "Serine" = "S", 
                "Threonine" = "T", "Tryptophan" = "W", "Tyrosine" = "Y", "Valine" = "V",
                "N-term" = "N-term", "C-term" = "C-term"),
  
  # All elements users can select from
  eleNames <- c("H", "C", "O", "N", "S", "P", "Na", "Cl", "K", "F", "I", "Se", 
                "Br", "Hg", "Cu", "Fe", "Mo", "Si", "B", "As", "Li", "Ca", "Ni",
                "Zn", "Ag", "Mg", "Al"),
  
  ###############
  ## OBSERVERS ##
  ###############
  
  # Add data to table manually
  observeEvent(input$glAdd, {
    
    showModal(modalDialog(fluidPage(fluidRow( 
      column(3, textInput("glIN", "Modification", value = "New Modification")),
      column(3, numericInput("glMASS", "Mass Change", 0, step = 1)),
      column(6, pickerInput("glMS", "Modified Sites", choices = modSites,
                multiple = T, options = list(`live-search` = T, `actions-Box` = T)))),
    fluidRow(column(6, list(pickerInput("glEN", "Element Name", choices = eleNames,
                       options = list(`live-search` = T)),
                       p(HTML('<p></p><p><strong>Molecular Formula:</strong></p>')))),
      column(6, list(numericInput("glNUM", "Num of Atoms", 1, step = 1),
                list(actionButton("glELadd", "Add"), 
                     actionButton("glELrem", "Clear"))))), hr(),
    fluidRow(column(12, htmlOutput("glMF")))),
      title = HTML('<p style="text-align: center;"><strong>Select Modification Information</strong></p>'),
      footer = list(actionButton("glManAdd", "Add"), modalButton("Exit")),
      size = "m", easyClose = T))
  }),
  
  # Generate molecular formula 
  observeEvent(input$glELadd, {
    if (gloss$molForm == "Generated Molecular Formula Appears Here") {gloss$molForm <- ""}
    gloss$molForm <- paste(gloss$molForm, " ", input$glEN, round(input$glNUM), sep = "")
  }),
  
  # Clear molecular formula
  observeEvent(input$glELrem, {
    gloss$molForm <- "Generated Molecular Formula Appears Here"
  }),
  
  # Add to glossary if everything is set correctly
  observeEvent(input$glManAdd, {
    
    # Get the glossary data
    Glossary <- GET_glossary()
    
    # Catches if no modified sites have been selected
    if (is.null(input$glMS)) {
      sendSweetAlert(session, "Glossary Amendment Error", 
        "Please select at least one modified site.", type = "error")
      return(NULL)}
    
    # Catches if no molecular formula has been generated
    if (gloss$molForm == "Generated Molecular Formula Appears Here") {
      sendSweetAlert(session, "Glossary Amendment Warning",
        "Unknown mass shifts are allowed, but since the molecular formula is unknown, this modification will not be accounted for in the isotope calculations.", type = "warning")
      Sys.sleep(8)
    }
    
    ## Generate line to be added to the Glossary
    Modification <- input$glIN
    MassChange <- input$glMASS
    Residues <- paste(input$glMS, collapse = " ")
    
    # Extract the molecular formula
    if (gloss$molForm != "Generated Molecular Formula Appears Here") {

      MolFormSplit <- trimws(gloss$molForm) %>% strsplit(" ") %>% unlist()
      Elements <- lapply(MolFormSplit, function(x) {gsub("[[:digit:]]", "", x)}) %>% unlist()
      Counts <- lapply(MolFormSplit, function(x) {gsub("[[:alpha:]]", "", x)}) %>% unlist() %>% as.numeric()

      # Make the row 
      NewMod <- c(Modification, MassChange, Residues, Counts) %>% t() %>% data.frame()
      colnames(NewMod) <- c("Modification", "Mass Change", "Residues", Elements)
  
      # Insist Numeric 
      NewMod[colnames(NewMod) %in% Elements] <- as.numeric(NewMod[colnames(NewMod) %in% Elements])
      
    } else {
      NewMod <- c(Modification, MassChange, Residues) %>% t() %>% data.frame()
      colnames(NewMod) <- c("Modification", "Mass Change", "Residues")
    }
    NewMod$`Mass Change` <- as.numeric(NewMod$`Mass Change`)
  
    # Fix data types
    gloss$AddedMods <- dplyr::bind_rows(gloss$AddedMods, NewMod)
      
    # Remove current molecular formula and the box
    gloss$molForm <- "Generated Molecular Formula Appears Here"
    
    # Success message that makes people feel warm and fuzzy inside
    sendSweetAlert(session, "Glossary Amended", "Modification added to Glossary!",
                   type = "success")
    
    removeModal()
    
  }),
  
  # Add uploaded file 
  observeEvent(input$glCSVadd, {
    
    # Read data 
    new_data <- fread(input$glCSVadd$datapath)
    
    # Columns must all be in the glossary 
    if (!all(colnames(new_data) %in% colnames(GET_glossary()))) {
      sendSweetAlert(session, "Adding modifications to glossary error", 
                     paste0("The following columns are not permitted: ", 
                            paste(colnames(new_data)[colnames(new_data) %in% colnames(GET_glossary()) == FALSE], collapse = ", ")
                            ),
                     "error")
      return(NULL)
    }
    
    # Otherwise, bind to our list
    gloss$AddedMods <- dplyr::bind_rows(gloss$AddedMods, new_data)
    
  }),
  
  ##########################
  ## RENDER TABLE / PLOTS ##
  ##########################
  
  # Glossary Table
  output$GlossTab <- DT::renderDataTable({
    
    # Pull the glossary 
    Glossary <- GET_glossary()
    
    # Extend added modifications 
    if (!(is.null(gloss$AddedMods))) {
      Glossary <- dplyr::bind_rows(gloss$AddedMods, Glossary)
    }
    
    datatable(Glossary, 
              rownames = F, filter = 'top', options = list(pageLength = 10),
              selection = list(mode = 'single'))
  }),
  
  # Visualize current molecular formula
  output$glMF <- renderText({
    HTML(paste('<p style="text-align: center;">', gloss$molForm))
  })
  
)