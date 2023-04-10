## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains most of the getter functions

list(
  
  # Get the ions associated with the activation method. Default is all values.
  GET_activation_method_ions <- reactive({
    
    # Declare ions variable to hold all ion types
    Ions <- c("a", "b", "c", "x", "y", "z", "Spec")
    
    # Test for scan data
    if (is.null(msPath())) {return(Ions)}
    
    # Get most frequent activation method
    AM <- GET_scan_metadata()[GET_scan_click(), "Activation Method"]
    
    # Remove nulls
    if (is.na(AM)) {return(Ions)}
    
    # Autoselect specific ions for activation methods
    if (AM == "HCD") {Ions <- c("b", "y")} else 
    if (AM == "CID") {Ions <- c("a", "b", "y")} else
    if (AM == "ETD") {Ions <- c("b", "c", "y", "z")} 
    
    return(Ions)
    
  }),
  
  GET_glossary <- reactive({
    
    data.table::fread(
      system.file("extdata", "Unimod_v20220602.csv", package = "pspecterlib")
    )
    
  }),
  
  # Get matched peaks 
  GET_matched_peaks <- reactive({
    
    # Return NULL if any of the following are missing
    if (is.null(GET_scan_metadata())) {return(NULL)}
    if (is.null(GET_peak_data())) {return(NULL)}
    if (is.null(GET_sequence())) {return(NULL)}
    if (is.null(input$ssTolerance)) {return(NULL)}
    if (is.null(input$ionGroups)) {return(NULL)}
    if (is.null(input$ssCorrScoreFilter)) {return(NULL)}
    
    
    # Add ions 
    RegIons <- c("a", "b", "c", "x", "y", "z")[which(c("a", "b", "c", "x", "y", "z") %in% input$ionGroups)]
    ModIons <- input$ionGroups[which(input$ionGroups %in% c("a", "b", "c", "x", "y", "z") == FALSE)]
    if (length(ModIons) == 0) {ModIons <- NULL}
    
    # Filter to selected modified ions
    if (!is.null(ModIons)) {
      
      NewIons <- make_mass_modified_ion(
        Ion = revals$AddedIons$Ion,
        Symbol = revals$AddedIons$Annotation,
        AMU_Change = revals$AddedIons$`AMU Change`
      )
      class(NewIons) <- c("data.table", "data.frame")
      NewIons <- NewIons %>% dplyr::filter(Modified_Ion %in% ModIons)
      ModIons <- NewIons
      class(ModIons) <- c(class(ModIons), "modified_ion")
      
    }
    
    get_matched_peaks(
      ScanMetadata = GET_scan_metadata(),
      PeakData = GET_peak_data(),
      PPMThreshold = input$ssTolerance,
      IonGroups = RegIons,
      CalculateIsotopes = ifelse(is.null(input$ssISOspectra), TRUE, input$ssISOspectra),
      MinimumAbundance = 0.1,
      CorrelationScore = input$ssCorrScoreFilter,
      MatchingAlgorithm = "closest peak", 
      AlternativeIonGroups = ModIons,
      AlternativeSequence = GET_sequence()
    )
    
  }), 
  

  # Get MS1FT - only needed for ProMex feature plot 
  GET_ms1ft <- reactive({
    
    # If no MS1FT path, return NULL
    if (is.null(ms1ftPath())) {return(NULL)}
    
    # Else, run function
    get_ms1ft(
      MS1FTPath = ms1ftPath(),
      TargetsPath = targetsPath(),
      TrimColumns = TRUE
    )
    
  }), 
  
  # Get peak data - needed to match peaks 
  GET_peak_data <- reactive({
    
    # If no scan metadata or scan number return NULL 
    if (is.null(GET_scan_metadata())) {return(NULL)}
    if (is.null(GET_scan_number())) {return(NULL)}
    if (is.null(input$ssIntenMin)) {return(NULL)}
    
    get_peak_data(
      ScanMetadata = GET_scan_metadata(),
      ScanNumber = GET_scan_number(),
      MinIntensity = input$ssIntenMin
    ) 
    
  }),
  
  # Get peptide coverage 
  GET_peptide_coverage <- reactive({
    
    # If no ScanMetadata, ProteinTable, or ProteinID return NULL
    if (is.null(GET_scan_metadata())) {return(NULL)}
    if (is.null(GET_protein_table())) {return(NULL)}
    if (is.null(GET_protein_ID())) {return(NULL)}
    
    get_peptide_coverage(
      ScanMetadata = GET_scan_metadata(),
      ProteinTable = GET_protein_table(),
      ProteinID = GET_protein_ID()
    )
  
  }),
  
  # Get protein ID
  GET_protein_ID <- reactive({
    
    # Figure out the protein ID if the protein table is not NULL
    if (is.null(GET_protein_table())) {return(NULL)}
    
    # Get clicked position    
    clicked <- input$PTTable_row_last_clicked
    if (is.null(clicked)) {clicked <- 1}
  
    return(unlist(GET_protein_table()[clicked, "Protein"]))
  
  }),
  
  # Get protein table
  GET_protein_table <- reactive({
    
    # If no scan metadata or fasta path return NULL
    if (is.null(GET_scan_metadata)) {return(NULL)}
    if (is.null(fastaPath())) {return(NULL)}
    
    tryCatch(get_protein_table(
      ScanMetadata = GET_scan_metadata(),
      FASTAPath = fastaPath(),
      QValueMaximum = input$PTToleranceQValue,
      ScoreMaximum = as.numeric(input$PTToleranceScore),
      RemoveContaminants = ifelse(is.null(input$PTCont), TRUE, input$PTCont)
    ), error = function(e) {return(NULL)})
    
  }),
  
  # Get scan click
  GET_scan_click <- reactive({
    
    clicked <- input$ssScan_row_last_clicked
    if (is.null(clicked)) {clicked <- 1}
    return(clicked)
    
  }),
  
  # Get scan number
  GET_scan_number <- reactive({
    
    # If no scan metadata, return NULL
    if (is.null(GET_scan_metadata())) {return(NULL)} 
      
    return(unlist(GET_scan_metadata()[GET_scan_click(), "Scan Number"]))
    
    
  }),
  
  # Get scan metadata information - needed for all functionality
  GET_scan_metadata <- reactive({
    
    # If no MS path, return NULL
    if (is.null(msPath())) {return(NULL)}
    
    get_scan_metadata(
      MSPath = msPath(),
      IDPath = idPath()
    )
    
  }),
  
  # Get sequence - used by many functions. Default should always be the identified
  # sequence if there is one
  GET_sequence <- reactive({
    
    # Return null if scan metadata
    if (is.null(GET_scan_metadata())) {return(NULL)}
    
    # Extract a potential sequence - either from the file or overwritten by user 
    Seq <- ifelse(is.null(revals$testSeq), unlist(GET_scan_metadata()[GET_scan_click(), "Sequence"]), revals$testSeq)
    
    # Return NULL if no sequence 
    if (is.null(Seq) || is.na(Seq)) {return(NULL)}
    
    # Otherwise, return this sequence
    return(Seq)
    
  }),

  
  # Get XIC
  GET_XIC <- reactive({
    
    # If no ScanMetadata or PreMZ, return NULL 
    if (is.null(GET_scan_metadata())) {return(NULL)}

    # Return XIC object
    get_xic(
      ScanMetadata = GET_scan_metadata(),
      MZ = ifelse(is.null(input$premzXIC), unlist(GET_scan_metadata()[GET_scan_click(), "Precursor M/Z"]), input$premzXIC),
      RTRange = c(-1 * input$rtXIC, input$rtXIC) + unlist(GET_scan_metadata()[GET_scan_click(), "Retention Time"]),
      IsotopeNumber = as.numeric(input$isoXIC),
      Charges = as.numeric(input$chargeTraceXIC)
    )
    
  })
  
  
)