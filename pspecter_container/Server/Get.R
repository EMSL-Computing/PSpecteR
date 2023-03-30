## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains most of the getter functions

list(
  
  # Get matched peaks 
  GET_matched_peaks <- reactive({
    
    # Return NULL if any of the following are missing
    if (is.null(GET_scan_metadata())) {return(NULL)}
    if (is.null(GET_peak_data())) {return(NULL)}
    if (is.null(input$ssTolerance)) {return(NULL)}
    if (is.null(input$ionGroups)) {return(NULL)}
    if (is.null(input$ssISOspectra)) {CalcIso <- TRUE} else {CalcIso <- !input$ssIsoSpectra}
    if (is.null(input$ssCorrScoreFilter)) {return(NULL)}
    if (is.null(input$ssAlgorithm)) {return(NULL)}
    if (is.null(input$ssNewSeq)) {return(NULL)}
    
    get_matched_peaks(
      ScanMetadata = GET_scan_metadata(),
      PeakData = GET_peak_data(),
      PPMThreshold = input$ssTolerance,
      IonGroups = input$ionGroups,
      CalculateIsotopes = CalcIso,
      MinimumAbundance = 0.1,
      CorrelationScore = input$ssCorrScoreFilter,
      MatchingAlgorithm = input$ssAlgorithm, 
      AlternativeSequence = input$ssNewSeq
    )
    
  }), 
  

  # Get MS1FT - only needed for ProMex feature plot 
  GET_ms1ft <- reactive({
    
    # If no MS1FT path, return NULL
    if (is.null(ms1ftPath())) {return(NULL)}
    
    # Else, run function
    get_ms1ft(
      MS1FTPath = ms1ftPath(),
      TargetsPath = NULL,
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
    
    browser()
    
  }),
  
  # Get protein table
  GET_protein_table <- reactive({
    
    # If no scan metadata or fasta path return NULL
    if (is.null(GET_scan_metadata)) {return(NULL)}
    if (is.null(fastaPath())) {return(NULL)}
    
    get_protein_table(
      ScanMetadata = GET_scan_metadata(),
      FastaPath = fastaPath()
    )
    
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
    
  })
  
  
)