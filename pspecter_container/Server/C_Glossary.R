## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_06_09

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
  eleNames <- c("Hydrogen"  =  "H", "Helium" = "He", "Lithium" = "Li", "Beryllium" = "Be", 
                "Boron" = "B", "Carbon" = "C", "Nitrogen" = "N", "Oxygen" = "O", "Fluorine" = "F", 
                "Neon" = "Ne", "Sodium" = "Na", "Magnesium" = "Mg", "Aluminium" = "Al", 
                "Silicon" = "Si", "Phosphorus" = "P", "Sulfur" = "S", "Chlorine" = "Cl", 
                "Argon" = "Ar", "Potassium" = "K", "Calcium" = "Ca", "Scandium" = "Sc", 
                "Titanium" = "Ti", "Vanadium" = "V", "Chromium" = "Cr", "Manganese" = "Mn", 
                "Iron" = "Fe", "Cobalt" = "Co", "Nickel" = "Ni", "Copper" = "Cu", "Zinc" = "Zn", 
                "Gallium" = "Ga", "Germanium" = "Ge", "Arsenic" = "As", "Selenium" = "Se", 
                "Bromine" = "Br", "Krypton" = "Kr", "Rubidium" = "Rb", "Strontium" = "Sr", 
                "Yttrium" = "Y", "Zirconium" = "Zr", "Niobium" = "Nb", "Molybdenum" = "Mo", 
                "Technetium" = "Tc", "Ruthenium" = "Ru", "Rhodium" = "Rh", "Palladium" = "Pd", 
                "Silver" = "Ag", "Cadmium" = "Cd", "Indium" = "In", "Tin" = "Sn", "Antimony" = "Sb", 
                "Tellurium" = "Te", "Iodine" = "I", "Xenon" = "Xe", "Caesium" = "Cs", "Barium" = "Ba", 
                "Lanthanum" = "La", "Cerium" = "Ce", "Praseodymium" = "Pr", "Neodymium" = "Nd", 
                "Promethium" = "Pm", "Samarium" = "Sm", "Europium" = "Eu", "Gadolinium" = "Gd", 
                "Terbium" = "Tb", "Dysprosium" = "Dy", "Holmium" = "Ho", "Erbium" = "Er", 
                "Thulium" = "Tm", "Ytterbium" = "Yb", "Lutetium" = "Lu", "Hafnium" = "Hf", 
                "Tantalum" = "Ta", "Tungsten" = "W", "Rhenium" = "Re", "Osmium" = "Os", 
                "Iridium" = "Ir", "Platinum" = "Pt", "Gold" = "Au", "Mercury" = "Hg", 
                "Thallium" = "Tl", "Lead" = "Pb", "Bismuth" = "Bi", "Polonium" = "Po", 
                "Astatine" = "At", "Radon" = "Rn", "Francium" = "Fr", "Radium" = "Ra", 
                "Actinium" = "Ac", "Thorium" = "Th", "Protactinium" = "Pa", "Uranium" = "U"),
  
  ###############
  ## OBSERVERS ##
  ###############
  
  # Add data to table manually
  observeEvent(input$glAdd, {
    
    showModal(modalDialog(fluidPage(fluidRow( 
      column(3, textInput("glIN", "Interim Name", value = "New Modification")),
      column(3, numericInput("glMASS", "Mass", 0, step = 1)),
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
    Glossary <- getGlossary()
    
    # Catches if no modified sites have been selected
    if (is.null(input$glMS)) {
      sendSweetAlert(session, "Glossary Amendment Error", 
        "Please select at least one modified site.", type = "error")
      return(NULL)}
    
    # Catches if no molecular formula has been generated
    if (gloss$molForm == "Generated Molecular Formula Appears Here") {
      sendSweetAlert(session, "Glossary Amendment Error",
        "Please construct a molecular formula", type = "error")
      return(NULL)}
    
    # Success message that makes people feel warm and fuzzy inside
    sendSweetAlert(session, "Glossary Amended", "Modification added to Glossary!",
      type = "success")
    
    # Generate line to be added to the Glossary
    Accession.Number <- max(Glossary$Accession.Number) + 1
    PSI.MS.Name <- Interim.Name <- Description <- input$glIN
    Monoisotopic.Mass <- input$glMASS
    Average.Mass <- NA
    Composition <- Molecular.Formula <- trimws(gloss$molForm)
    Modified.Sites <- paste(input$glMS, collapse = " ")
    NewMod <- c(Accession.Number, PSI.MS.Name, Interim.Name, Description,
                Monoisotopic.Mass, Average.Mass, Composition, Molecular.Formula,
                Modified.Sites)
    gloss$AddedMods <- data.frame(rbind(gloss$AddedMods, NewMod), stringsAsFactors = F)
    colnames(gloss$AddedMods) <- c("Accession.Number", "PSI.MS.Name", "Interim.Name", 
                "Description", "Monoisotopic.Mass", "Average.Mass", "Composition", 
                "Molecular.Formula", "Modified.Sites")
    
    # Fix data types
    gloss$AddedMods$Accession.Number <- as.numeric(gloss$AddedMods$Accession.Number)
    gloss$AddedMods$Monoisotopic.Mass <- as.numeric(gloss$AddedMods$Monoisotopic.Mass)
    
    # Remove current molecular formula and the box
    gloss$molForm <- "Generated Molecular Formula Appears Here"
    removeModal()
  }),
  
  ##########################
  ## RENDER TABLE / PLOTS ##
  ##########################
  
  # Glossary Table
  output$GlossTab <- DT::renderDataTable({
    Glossary <- getGlossary()
    datatable(Glossary[, c(as.integer(input$glCheckboxes)), drop = F], 
              rownames = F, filter = 'top', options = list(pageLength = 10),
              selection = list(mode = 'single'))
  }),
  
  # Visualize current molecular formula
  output$glMF <- renderText({
    HTML(paste('<p style="text-align: center;">', gloss$molForm))
  })
  
)