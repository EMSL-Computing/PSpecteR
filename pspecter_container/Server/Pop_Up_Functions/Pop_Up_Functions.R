## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_12_28

# DESCRIPTION: This contains all the popover information for graphs and input widgets.

list(

# The pop up which describes pop up mode remains on at all times
output$infoSWITCH <- renderUI({
    popify(shinyWidgets::materialSwitch("infoMode", label = HTML('<strong>Enable Description Mode</strong>'),
               value = F, status = "success"),
           "Description Mode",
           "Turn on this switch to enable description boxes (like this one) for every input and output in PSpecteR. Recommended for new app users.",
           options = list(selector = '.material-switch'), placement = 'right')
}),

observeEvent(input$infoMode, {
  
  # Define each of the inputs per page 
  
  upload <- c("mzmsFile", "mzmsHandle", "mzmsHandleGo", "mzmsHandleClear",
              "idFile", "idHandle", "idHandleGo", "idHandleClear",
              "fastaFile", "fastaHandle", "fastaHandleGo", "fastaHandleClear",
              "outputDirChoose", "outputHandle", "outputHandleGo", "outputHandleClear")
  
  seqxic <- c("ssTolerance", "ssIntenMin", "ssIsoPerMin", "ssCorrScoreFilter",
              "ssAnnoSize", "ssLabDist", "ssLargeSpec", "ssRSeq", "ssASeq",
              "MPpercdiff", "MPwinsize", "MPlargePre", "MPlargeNext", "tolXIC",
              "imgSPEC", "imgHM", "imgMPPRE", "imgMPNEXT", "imgXIC", "imgSSBAR", 
              "imgFLAG", "SCANcsv", "PEAKcsv", "FRAGcsv", "ALLIONcsv", 
              "ssSpectrum", "ErrorMap", "XIC", "ssMatPre", "ssMatNext", 
              "ssSeqTable", "ssSeqBar", "ssScan", "ssSeqFlag", "ssAllFrag")
  
  visptm <- c("VPrseq", "VPclear", "VPcommon", "VPmaxmod", "VPmaxpep",
              "VPposs", "VPspecific", "imgVPSPEC", "imgVPHM", "imgVPFLAG", 
              "imgVPPRE", "imgVPNEXT", "VPcsv", "VPmarkdown", "VPSpec", 
              "VPehm", "VPseqflags", "VPmatchpre", "VPmatchnext", "VPmetrics")
  
  
  protein <- c("PTTolerance", "imgMATCH", "imgPTBAR", "imgLSEQ", "PTcsv", "PTMatch",
               "PTBar", "LSeq", "PTTable")
  
  BU <- c("BURun", "BUgetstatus", "BUParamFile")
  
  TD <- c("TDRunAll", "TDgetstatus", "TDCharge", "TDMass", "TDLength", "TDFragCharge",
          "TDPrecursorTol", "TDFragmentTol", "TDDatabaseSearch", "TDActMet", "TDModFile")
  
  SM <- c("SMplot", "imgSM")
  
  ms1ft <- c("ms1ftFile", "ms1ftHandle", "ms1ftHandleGo", "ms1ftHandleClear", 
             "ms1ftTop", "ms1ftBottom", "imgPMFM", "PMFM", "ms1ftTab")
  
  glossary <- c("glAdd", "glCSVadd", "glEXP", "glEXPmod", "GlossTab")  
    
    
  # If info mode is true, then add all the popovers
  if (input$infoMode == T) {
    
    sendSweetAlert(session, "Popovers Enabled!", "Popovers are pop up boxes that
                   teach users how to use the app.", type = "success")
    
    ########################
    ## 1. UPLOAD POP OVER ##
    ########################
    
    for (inputName in upload) {
      addPopover(session, inputName, Desc[Desc$Name == inputName, "Title"],
                 Desc[Desc$Name == inputName, "Description"])
    }
    
    ###########################
    ## 2. SEQ & XIC POP OVER ##
    ###########################
    
    for (inputName in seqxic) {
      addPopover(session, inputName, Desc[Desc$Name == inputName, "Title"],
                 Desc[Desc$Name == inputName, "Description"])
    }
    
    #######################
    ## 3. VISUALIZE PTMS ##
    #######################
    
    for (inputName in visptm) {
      addPopover(session, inputName, Desc[Desc$Name == inputName, "Title"],
                 Desc[Desc$Name == inputName, "Description"])
    }
    
    ##################################
    ## 4. PROTEIN COVERAGE POP OVER ##
    ##################################
    
    for (inputName in protein) {
      addPopover(session, inputName, Desc[Desc$Name == inputName, "Title"],
                 Desc[Desc$Name == inputName, "Description"])
    }
    
    ###############################
    ## DB SEARCH: MSGF+ POP OVER ##
    ###############################
    
    for (inputName in BU) {
      addPopover(session, inputName, Desc[Desc$Name == inputName, "Title"],
                 Desc[Desc$Name == inputName, "Description"])
    }
    
    ##########################################
    ## DB SEARCH: MS PATH FINDER T POP OVER ##
    ##########################################
    
    for (inputName in TD) {
      addPopover(session, inputName, Desc[Desc$Name == inputName, "Title"],
                 Desc[Desc$Name == inputName, "Description"])
    }
    
    ##################################
    ## A. SPECTRA METADATA POP OVER ##
    ##################################
    
    for (inputName in SM) {
      addPopover(session, inputName, Desc[Desc$Name == inputName, "Title"],
                 Desc[Desc$Name == inputName, "Description"])
    }
    
    ####################################
    ## B. PROMEX FEATURE MAP POP OVER ##
    ####################################
    
    for (inputName in ms1ft) {
      addPopover(session, inputName, Desc[Desc$Name == inputName, "Title"],
                 Desc[Desc$Name == inputName, "Description"])
    }
    
    #################################
    ## C. UNIMOD GLOSSARY POP OVER ##
    #################################
    
    for (inputName in glossary) {
      addPopover(session, inputName, Desc[Desc$Name == inputName, "Title"],
                 Desc[Desc$Name == inputName, "Description"])
    }
    
  } else {
    
    popoverList <- c(upload, seqxic, visptm, protein, BU, TD, SM, ms1ft, glossary)
    
    for (popover in popoverList) {
      removePopover(session, popover)
    }}
    
  })

)