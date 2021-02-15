################################################################################
################### PSpecteR, a proteomics QC Shiny App ########################
########### David Degnan, Pacific Northwest National Laboratory ################
####################### Last Updated: 2021_02_15 ###############################
################################################################################

# Load the package for Shiny App infrastructure
library(shiny)
library(shinycssloaders) 
library(shinyWidgets)
library(shinyBS)
library(shinyFiles)
library(shinyjs)

# Load packages for proteomic file reading and LCMS analytics
library(rhdf5)
library(mzR)
library(MSnbase)
library(Peptides)
library(rawDiag) 

# Load packages for isotoping
library(BRAIN)
library(Rdisop)
library(lsa)

# Load packages for data processing
library(seqinr)
library(dplyr)
library(reshape2)
library(gtools)
library(readxl)

# Load packages for interactive data visualization
library(data.table)
library(plotly)
library(DT)

# If the test files are not in the right directory, copy and move them 
source(file.path("Server", "PrepTestFiles.R"), local = T)$value
prepTestFiles()

# Define function to render images in ui
imgRender <- function(id, path, width, height) {
  return(img(id = id, src = path, contentType = "image/png", width = width, height = height, align = "left"))
}

# Suppress all warnings
options(warn=-1)

# Build the interface in which the user will interact. Inverse colors on the 
# navigation bar will be used (black text as opposed to white). Title is PSpecteR.
ui <- navbarPage(id = "mainTabs", inverse = T, title = "PSpecteR", 
                                
   ################################
   ##  0. WELCOME USER INTERFACE ##
   ################################
   
   # Simply a welcome greeting and brief explanation of how to use this GUI.
   tabPanel("Welcome", mainPanel(
    
    # Enable shinyjs
    useShinyjs(),
     
    # Add Welcome Page GIF
    column(12, img(src = "WelcomeToPSpecteR.gif", contentType = "image/gif",
      width = "600px", height = "294px", align = "left")), 
    column(12, img(src = "Divider_Line.png", 
      width = "600px", contentType = "image/png", align = "left")),
    
    # Add Welcome Page PNG with buttons
    column(12, imgRender("AppFunctions", "WelcomeImage/AppFunctions.png", "600px", "75px")),
    column(2, imgRender("Page1", "WelcomeImage/Upload.png", "200px", "200px")),
    column(2, imgRender("Page2", "WelcomeImage/MSnXIC.png", "200px", "200px")),
    column(8, imgRender("Page3", "WelcomeImage/VisPTM.png", "200px", "200px")),
    column(2, imgRender("Page4", "WelcomeImage/ProteinCoverage.png", "200px", "200px")),
    column(2, imgRender("Page5", "WelcomeImage/DatabaseSearch.png", "200px", "200px")),
    column(8, imgRender("Page6", "WelcomeImage/AdditionalPlots.png", "200px", "200px")),
    column(12, img(src = "Divider_Line.png", contentType = "image/png", 
      width = "600px", align = "left")),
    
    # Add UI for the manual and citation information
    column(12, htmlOutput("citation")), column(12, htmlOutput("manual")))),
   
   ##############################
   ## 1. UPLOAD USER INTERFACE ##
   ##############################
   
   # This panel allows users to upload their data. 
   tabPanel("1. Upload", 
            
  # For the mass spec files: raw, mzml, mzml.gz, mzxml, and mzxml.gz are supported
  sidebarLayout(sidebarPanel(
    
    # This CSS code changes the settings for all the sidebars in the GUI
    tags$style(type = 'text/css', '.well { background-color: #DFDCE3;
               font-family: Calibri; font-size: 16px; color: #000000;}'),
    
    # Upload an MS File: Shiny files button or type path
    bsCollapse(multiple = T, bsCollapsePanel(
      title = HTML('<p><strong>Mass Spectra (MS) FILE (Required)</strong></p>'),
      shinyFilesButton("mzmsFile", HTML("<strong>Search Folders:</strong> mzML, mzXML, raw"), 
                       "Choose MS File: mzML, mzXML, or raw", F), hr(),
      textInput("mzmsHandle", "...or type in the MS file path", "", placeholder = "Type full MS file path with forward slashes"),
      list(actionButton("mzmsHandleGo", "Use MS Path"), actionButton("mzmsHandleClear", "Clear MS Path")))), width = 3),
    
    # All uploads include an output which contains filename, size, and number of 
    # datapoints for user sanity.  
    mainPanel(htmlOutput("msUpload"))),
  
  # The second side panel allows for a user to upload an optional mzID file.
  sidebarLayout(sidebarPanel(
    
    bsCollapse(bsCollapsePanel(title = HTML('<p><strong>Peptide Identification (ID) FILE (Optional)</strong></p>'),
      shinyFilesButton("idFile", HTML("<strong>Search Folders:</strong> mzid, mzID"), 
                       "Choose ID File: mzid", F), hr(),
      textInput("idHandle", "...or type in the ID file path", "", placeholder = "Type full ID file path with forward slashes"),
      list(actionButton("idHandleGo", "Use ID Path"), actionButton("idHandleClear", "Clear ID Path")))), width = 3),
    
      # Like before, a succesful upload will include output which informs the user.
      mainPanel(htmlOutput("idUpload"))),
  
  # The third side panel allows for a user to upload an optional fasta file.
  sidebarLayout(sidebarPanel(
    bsCollapse(bsCollapsePanel(title = HTML('<p><strong>FASTA Protein Database (FA) FILE (Optional)</strong></p>'),
    shinyFilesButton("fastaFile", HTML("<strong>Search Folders:</strong> FASTA, FA"), 
                     "Choose FA File: FASTA (FA)", F), hr(), 
      textInput("fastaHandle", "...or type in the FA file path", "", placeholder = "Type full FA file path with forward slashes"),
      list(actionButton("fastaHandleGo", "Use FA Path"), actionButton("fastaHandleClear", "Clear FA Path")))), width = 3),
    
    # Like before, a succesful upload will include output which informs the user.
    mainPanel(htmlOutput("fastaUpload"))),
  
  # The fourth sidebar allows for whole app settings to be set, and to use test files
  sidebarLayout(sidebarPanel(
    bsCollapse(multiple = T, 
     bsCollapsePanel(title = HTML('<p><strong>APP SETTINGS</strong></p>'), uiOutput("infoSWITCH")),
     bsCollapsePanel(title = HTML('<p><strong>USE TEST FILES</strong></p>'),
       uiOutput("testBUSWITCH"), uiOutput("testTDSWITCH")
     )), width = 3),
    mainPanel(htmlOutput("testUpload")))),
   
   ################################
   ## 2. MS & XIC USER INTERFACE ##
   ################################
   
   # This panel allows for interaction with MS2 fragment data.
   tabPanel("2. MS & XIC", 
            
  # The contents of this sidebar are checkboxes, numeric tolerance input, color
  # options, and outputted coverage, precursor, and number of peak information.
  sidebarLayout(sidebarPanel(
    
    # This chunk of code ensures that checkboxes line up left-justified, and that the
    # progress bar is green along with all the sliders.
    tags$head(tags$style(HTML(
      ".checkbox-inline {margin-left: 0px; margin-right: 10px;}
       .checkbox-inline+.checkbox-inline {margin-left: 0px; margin-right: 10px;}
       .progress-bar {background-color: #275d0c;}
        a {color: #46A716; }
       .irs-bar-edge, .irs-bar {
        background: #4EBA19; border-top: 1px solid #4EBA19; border-bottom: 1px solid #4EBA19;}
       .irs-from, .irs-to, .irs-single {background: #4EBA19;}"))),
    
    bsCollapse(id = "ssCollapse", multiple = T, 
     open = list("1. Filter Settings"),
     
     
     # 1. Selection Ions: Adjust selection ions based on activation method
     bsCollapsePanel("1. Filter Settings", 
        
       # Allows users to select how far off the experimental fragment value can be from the theoretical fragment value.
       HTML("<strong>Filter spectra data by...</strong></p>"), hr(),
       uiOutput("SelectedIons"),
       numericInput("ssIntenMin", "Intensity", 100, min = 1, max = 1e9, step = 10),
       sliderInput("ssTolerance", "Fragment Tolerance (PPM)", 0.1, 20, 10, 0.1, T),    
       sliderInput("ssIsoPerMin", "Isotopic Percentage", 1, 100, 10, 1, T),
       sliderTextInput("ssCorrScoreFilter", "Correlation Score", c(0:100/100, "ALL"), selected = "ALL")), 
     
     # 2. Change spectrum settings                
     bsCollapsePanel("2. Spectra Settings", bsCollapse(multiple = T, 
       bsCollapsePanel("2a. Spectra View",               
       
         # Set the font size of the annotated
         sliderInput("ssAnnoSize", "Label Size", 2, 15, 6, 1),
                       
         # Set the label relative label distance
         sliderTextInput("ssLabDist", "Set Relative Label Distance", 
                         c("Very Close", "Close", "Near", "Far", "Very Far"),
                         selected = "Close"),
         
         # Enable letter annotation on the spectrum
         uiOutput("ssLetterSWITCH"), 
                
         # Set slider for real-time edits of spectra
         uiOutput("RealTimeSWITCH"),
         
         # Place spectra in large pop up window
         actionButton("ssLargeSpec", "See the Spectrum in Full Screen")),
      
       bsCollapsePanel("2b. Isotope Settings",
          
         # Enable or disable isotopes for spectra
         uiOutput("ssISOspectraSWITCH")))),
     
     # 3. Sequence Settings
     bsCollapsePanel("3. Sequence Settings",
                     
       # Allow user test a different sequence: autofill with original sequence
       uiOutput("ssRenderNS"),
       
       # Reveal warnings if the input does not comply with necessary standard
       htmlOutput("ssNSWarn"), hr(),
       
       # Add an action button for adding modifications or restoring the sequence
       list(actionButton("ssASeq", "Apply Seq"),
            actionButton("ssRSeq", "Restore Seq"))),
     
     # 4. Graphics Settings
     bsCollapsePanel("4. Graphics Settings", bsCollapse(multiple = T, 
       bsCollapsePanel("4a. MS1 Plots Settings", 
         
         # Most Abundant Isotope switch, Trace switch, and Pre MZ range
         numericInput("MPpercdiff", "Filter by Percent Error", 25),
         numericInput("MPwinsize", "Set MS1 Window Size", 3),
         actionButton("MPlargePre", "MS1 Full Screen"),
         actionButton("MPlargeNext", "Next MS1 Full Screen")),
       
       bsCollapsePanel("4b. Fragment Figures",
                 
         # Set slider for bar chart counting by fragment or not
         uiOutput("ssBarCTFragSWITCH"), 
       
         # Set slider for adding annotations for PTMs
         uiOutput("ssAnoPTMSWITCH"),
         
         # Enable or disable isotopes for summary graphics 
         uiOutput("ssISOgraphsSWITCH")))),

     # 5. Scan metadata table     
     bsCollapsePanel("5. Table Column Settings",               
                     
       # Converts the checked boxes to column numbers for easy datatable manipulation.
       # Certain datatable columns are automatically selected and presented.    
        pickerInput("ssCheckboxes", "Select Scan Table Columns", 
           options = list(`live-search` = T, `actions-Box` = T),
           c("Order" = "1", "Scan Num" = "2", "MS Level" = "3", "RT" = "4", "Pre MZ" = "5", 
             "Pre Charge" = "6", "Pre Scan" = "7", "Act Met" = "8", "Sequence" = "9", 
             "Protein ID" = "10", "Mass" = "11", "Score" = "12", 
             "QVal" = "13", "isDecoy" = "14", "Description" = "15"), 
           selected = c("2", "3", "4", "5", "6", "7", "8", "9", "10"), multiple = T), hr(),
       
        # Select columns for fragment table
        pickerInput("ssFragColumns", "Select Fragment Columns", 
           options = list(`live-search` = T, `actions-Box` = T),
           c("Type" = "1", "Pos" = "2", "Pep" = "3", "Z" = "4", 
             "Iso" = "5", "MZ" = "6", "CorrScore" = "7"),
           selected = c("1", "2", "3", "4", "5", "6", "7"), multiple = T)),
     
     # 6. XIC Settings
     bsCollapsePanel("6. XIC Settings", 
                     
       # XIC Tolerance
       numericInput("tolXIC", "Set XIC Tolerance (ppm)", value = 10, step = 1), hr(),
       
       # Pick the number of isotopes to plot
       pickerInput("isoXIC", label = "Select Isotopes", choices = 0:5, 
                   selected = 0:2, multiple = T, options = list(`actions-Box` = T)), 
       
       # Pick the number of charge states to plot
       pickerInput("chargeTraceXIC", label = "Select Charges", choices = 1:3,
                   selected = 1:2, multiple = T, options = list(`actions-Box` = T)), hr(),
       
       # Set mass and charge
       uiOutput("massXIC"), uiOutput("chargeXIC")),      

     # 7. Export Images & Data
     bsCollapsePanel("7. Export Images & Data", bsCollapse(multiple = T, 
       bsCollapsePanel("7a. Export Images",               
         actionButton("imgSPEC", "Spectrum"),
         actionButton("imgHM", "Error Map"), HTML("<p></p>"), 
         actionButton("imgMPPRE", "Previous MS1"),
         actionButton("imgMPNEXT", "Next MS1"), HTML("<p></p>"),
         actionButton("imgXIC", "XIC"),
         actionButton("imgSSBAR", "Barchart"), HTML("<p></p>"),
         actionButton("imgFLAG", "Sequence")), 
       bsCollapsePanel("7b. Export Data",
         downloadButton("SCANcsv", "Export Scan Metadata"), HTML("<p></p>"),
         downloadButton("PEAKcsv", "Export Peak Data"), HTML("<p></p>"),
         downloadButton("FRAGcsv", "Export Annotated Peak Data"), HTML("<p></p>"),
         downloadButton("ALLIONcsv", "Export Annotation per Residue Table"))))),
    
    # This sidebar text output lets the user know how much of the peptide sequence
    # is explained by the fragments in the spectrum.
    width = 3),
              
      # To the right of the large Scan & Seq side panel is the following outputs.
      mainPanel(
        
      # Spectrum View: The actual MS itself, with annotations or the XIC view.
      # XIC: Visualize the intensity versus retention time graphs across all MS1 for 
      # a particular MS2's most abundant isotope. 
      # Error Map: The ppm errors for each identified spectrum's peak
      column(7, 
       tabsetPanel(id = "SStabs",
         tabPanel("MS/MS", plotlyOutput("ssSpectrum", width = "100%", height = "300px") 
                  %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
         tabPanel("Error Map", plotlyOutput("ErrorMap", width = "100%", height = "300px") 
                  %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
         tabPanel("XIC", htmlOutput("warnXIC"),  
                  plotlyOutput("XIC", width = "100%", height = "300px") 
                  %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))))),
        
      # Sequence View: A tabular display which includes an sequence view, 
      # interactive table with clicking functionality, 
      # and a bar chart which shows the number of fragment types.
      column(5, 
       tabsetPanel(id = "PTBtabs",
         tabPanel("MS1", plotlyOutput("ssMatPre", width = "125%", height = "300px") 
                  %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
         tabPanel("Next MS1", plotlyOutput("ssMatNext", width = "125%", height = "300px") 
                  %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
         tabPanel("Filter Ions", DTOutput("ssSeqTable", width = "125%", height = "300px")))),
        
      # Scan View: The datatable with information as determined by the checkboxes
      # with a sequence tab to visualize the ions with the smallest ppm error. 
      column(12, 
       tabsetPanel(id = "SSAtabs",      
         tabPanel("Scan", htmlOutput("coverage"), 
                  DT::DTOutput("ssScan", width = "115%", height = "250px")),
         tabPanel("Sequence", plotlyOutput("ssSeqFlag", width = "115%", height = "400px") 
                  %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
         tabPanel("Ion Annotation", DT::DTOutput("ssAllFrag", width = "115%", height = "250px")),
         tabPanel("Ion Barplot", plotlyOutput("ssSeqBar", width = "115%", height = "300px") 
                  %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c")))))))),

   #####################################
   ## 3. VISUALIZE PTM USER INTERFACE ##
   #####################################
   
   # Enables exploration of how PTMs affect the spectra.
   tabPanel("3. Vis PTM",
            
    # Select the spectra
    sidebarLayout(sidebarPanel(
      bsCollapse(multiple = T, open = list("1. Set Sequence"),
       bsCollapsePanel("1. Set Sequence", htmlOutput("VPscanWarn"), hr(),
         uiOutput("VPseq"), htmlOutput("VPnsWarn"), hr(), 
         actionButton("VPrseq", "Restore Seq")),
       bsCollapsePanel("2. Set Search Parameters", uiOutput("VPsetparams")),
       bsCollapsePanel("3. Dynamic Modification Search", uiOutput("VPselect"), 
         list(actionButton("VPclear", "Clear"), actionButton("VPcommon", "Autoselect Common Modifications")), hr(), 
         selectInput("VPmaxmod", "Max Modifications per Pep", choices = c("1", "2", "3", "4", "5"), selected = "2"),
         selectInput("VPmaxpep", "Max Modifications per Residue", choices = c("1", "2", "3"), selected = "1"),
         actionButton("VPposs", "Calculate")),
       bsCollapsePanel("4. Manual Modification Search", actionButton("VPspecific", "Manual Search")),
       bsCollapsePanel("5. Take Snapshot", 
         actionButton("imgVPSPEC", "Spectra"), 
         actionButton("imgVPHM", "Error Map"), HTML("<p></p>"), 
         actionButton("imgVPFLAG", "Sequence"), 
         actionButton("imgVPPRE", "Previous Precursor"), HTML("<p></p>"),
         actionButton("imgVPNEXT", "Next Precursor")),
       bsCollapsePanel("6. Export Data", 
         downloadButton("VPcsv", "Export Modifications Data"),
         hr() #downloadButton("VPmarkdown", "Export Markdown")
       )), width = 3),
      
      mainPanel(
        column(12, 
          tabsetPanel(id = "VisPTMtabs", tabPanel("Spectrum",
            htmlOutput("VPMS1scanwarn"), 
            plotlyOutput("VPSpec", width = "100%", height = "350px") %>% 
              withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
          tabPanel("Error Map",
            plotlyOutput("VPehm", width = "100%", height = "350px") %>% 
              withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
          tabPanel("Sequence", 
            plotlyOutput("VPseqflags", width = "100%", height = "350px") %>% 
              withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
          tabPanel("Precursors",
            column(6, plotlyOutput("VPmatchpre", width = "100%", height = "350px") %>% 
              withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))), 
            column(6, plotlyOutput("VPmatchnext", width = "100%", height = "350px") %>% 
              withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c")))))),
        column(12, DT::dataTableOutput("VPmetrics", width = "100%", height = "250px"))))),
   
   ########################################
   ## 4. PROTEIN COVERAGE USER INTERFACE ##
   ########################################
   
   # Allows user to explore matched ID data and answer questions about frequency,
   # patterns, protein families, etc. 
   tabPanel("4. Protein Coverage", 
            
    # Set the tolerance      
    sidebarLayout(sidebarPanel(
      bsCollapse(multiple = T,
       bsCollapsePanel("1. Protein Coverage Settings",
         numericInput("PTTolerance", "Q-Value Minimum", 0.10, min = 0, max = 1, step = 0.01),
         uiOutput("PTContSWITCH")),
       bsCollapsePanel("2. Take Image Snapshot",
                       actionButton("imgMATCH", "Match"),
                       actionButton("imgPTBAR", "Bar"),
                       actionButton("imgLSEQ", "Lit Seq")),
       bsCollapsePanel("3. Export Data", 
                       downloadButton("PTcsv", "Export Protein Coverage Data"))),
       htmlOutput("QValWarn"), width = 3),
      
      # To the right, users can select either a matched sequence or a bar view, with
      # a nonoptional table below. 
      mainPanel(
        tabsetPanel(id = "PTtabs",
          tabPanel("Match", htmlOutput("PTnoFAwarn"),
                   plotlyOutput("PTMatch", width = "100%", height = "250px")
                   %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
          tabPanel("Bar", plotlyOutput("PTBar", width = "100%", height = "250px")
                   %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
          tabPanel("Literature Sequence", plotlyOutput("LSeq", width = "100%", height = "250px") 
                   %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c")))),
        DT::dataTableOutput("PTTable", width = "100%", height = "250px")))),
   
   ###############################
   ## DB SEARCH USER INTERFACES ##
   ###############################
   
   navbarMenu("DB Search", 
              
    # This panel allows users to get an ID file if they do not have one with a bottom-up algorithm. 
    tabPanel("MS-GF+",
     sidebarLayout(sidebarPanel(
       HTML('<p style="text-align: center;"><span style="font-size: 16pt;"><strong>
             Use MS-GF+</strong></span></p>'),
       bsCollapse(multiple = T, open = "1. Run Algorithm",
       bsCollapsePanel("1. Run Algorithm",
          list(actionButton("BURun", "Run MS-GF+"),
               actionButton("BUgetstatus", "Get Status"))
       ),          
       bsCollapsePanel("2. Upload Parameter File", 
         shinyFilesButton("BUParamFile", HTML("<strong>Upload Parameter File:</strong> txt"), 
           "Choose File: txt", F))), width = 3),
     mainPanel(
       tabsetPanel(tabPanel("Input", htmlOutput("BUinputfiles")),
       tabPanel("Parameter File", htmlOutput("BUParameters"))
    )))),

    # Use MS Path Finder
    tabPanel("MSPathFinder",
       sidebarLayout(sidebarPanel(
         HTML('<p style="text-align: center;"><span style="font-size: 16pt;"><strong>
             Use MSPathFinder</strong></span></p>'),
         bsCollapse(multiple = T, open = "1. Run Algorithm",
         bsCollapsePanel("1. Run Algorithm",
           list(actionButton("TDRunAll", "Run MSPathFinder"),
                actionButton("TDgetstatus", "Get Status"))
         ),
         bsCollapsePanel("2. Set Parameters",
           sliderInput("TDCharge", "Min/Max Charge", 1, 60, c(2, 30), 1, T),
           sliderInput("TDMass", "Min/Max Mass", 600, 100000, c(2000, 50000), 100),
           sliderInput("TDLength", "Sequence Length", 20, 500, c(21, 300), 1, T),
           sliderInput("TDFragCharge", "Min/Max Frag Charge", 1, 20, c(1, 20), 1, T),
           sliderInput("TDPrecursorTol", "Precursor Tolerance (PPM)", 1, 100, 10, 1, T),
           sliderInput("TDFragmentTol", "Fragment Tolerance (PPM)", 1, 100, 10, 1, T),
           selectInput("TDDatabaseSearch", "Database Search", c("0 - Don't search decoy database",
                       "1 - Search shuffled decoy database"), "1 - Search shuffled decoy database"),
           selectInput("TDActMet", "Activation Method", c("0 - CID", "1 - ETD",
                       "2 - HCD", "3 - ECD", "4 - PQD", "5 - UVPD", "6 - Unknown"),
                       "6 - Unknown")
         ),
         bsCollapsePanel("3. Upload Modifications File",
            shinyFilesButton("TDModFile", HTML("<strong>Upload Modifications File:</strong> txt"), 
              "Choose File: txt", F))), width = 3),
       mainPanel(tabsetPanel(tabPanel("Input",
          
          # Input File
          htmlOutput("TDinputfiles"), 
                                                                  
          # Parameter Input
          htmlOutput("TDSetParameters")),
          
        # Modifications File Input
        tabPanel("Modifications File", htmlOutput("TDModifications"))
        
      ))))),
   
   ################################
   ## ADDITIONAL USER INTERFACES ##
   ################################
   
   # Underneath the "More" drop down navigation bar menu are more useful programs.
   navbarMenu("More",
              
    ########################################
    ## A. SPECTRA METADATA USER INTERFACE ##
    ########################################
    
    # This tab displays a feature plot for visualization of the entire spec file
    tabPanel("A. Spectra Metadata", 
             
     # Remind users which tab they're in
     sidebarLayout(sidebarPanel(
       HTML('<p style="text-align: center;"><span style="font-size: 16pt;"><strong>
             A. SPECTRA METADATA</strong></span></p>'),
       bsCollapse(open = list("1. Subset Data", "2. Select Variables"),
        multiple = T, bsCollapsePanel("1. Subset Data", uiOutput(
          "SMscannumUI"),  uiOutput("SMmslevelUI")),
        bsCollapsePanel("2. Select Variables", uiOutput("xSMui"), uiOutput("ySMui"), uiOutput("labSMui")),
        bsCollapsePanel("3. Take Image Snapshot", actionButton("imgSM", "Spectra Metadata Plot"))), width = 3), 
       mainPanel(
         plotlyOutput("SMplot", width = "100%", height = "500px") %>% 
           withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))))),
    
    ##########################################
    ## B. PROMEX FEATURE MAP USER INTERFACE ##
    ##########################################
    
    # Generate the ProMex Feature Map
    tabPanel("B. ProMex Feature Map", sidebarLayout(sidebarPanel(
      HTML('<p style="text-align: center;"><span style="font-size: 16pt;"><strong>
            B. PROMEX FEATURE MAP</strong></span></p>'),
      bsCollapse(multiple = T, open = list("1. Upload MS1FT"),
         bsCollapsePanel("1. Upload MS1FT", 
           shinyFilesButton("ms1ftFile", HTML("<strong>Search Folders:</strong> ms1ft"), 
             "Choose MS1FT File: ms1ft", F), hr(),
           textInput("ms1ftHandle", "...or type in the MS1FT file path", "", 
             placeholder = "Type full MS1FT file path with forward slashes"),
           list(actionButton("ms1ftHandleGo", "Use Path"), 
                actionButton("ms1ftHandleClear", "Clear Path")), hr(),
           uiOutput("testMS1FTSWITCH")),
         bsCollapsePanel("2. Protein Annotation", uiOutput("PMproteinChoose")),
         bsCollapsePanel("3. Take Image Snapshot", 
           actionButton("imgPMFM", "ProMex Feature Map"))), width = 3),
      mainPanel(
        uiOutput("ms1ftWarn"), uiOutput("PMproteinWarn"), hr(), 
        plotlyOutput("PMFM", width = "100%", height = "400px") %>% 
        withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c")),
        DTOutput("ms1ftTab", width = "100%", height = "200px")))),
    
    #######################################
    ## C. UNIMOD GLOSSARY USER INTERFACE ##
    #######################################
    
    # Make the unicode glossary
    tabPanel("C. Unimod Glossary", sidebarLayout(sidebarPanel(
      HTML('<p style="text-align: center;"><span style="font-size: 16pt;"><strong>
           C. UNIMOD GLOSSARY</strong></span></p>'), 
      bsCollapse(multiple = T, bsCollapsePanel("1. Select Columns",
        pickerInput("glCheckboxes", "Select Table Columns", options = list(`live-search` = TRUE),
        c("Accession Number" = "1", "PSI-MS Name" = "2", "Interim Name" = "3", 
          "Description" = "4", "Monoisotopic Mass" = "5", "Average Mass" = "6", 
          "Composition" = "7", "Molecular.Formula" = "8", "Modified.Sites" = "9"), multiple = TRUE, 
        selected = c("1","3","5","8","9"))),
      bsCollapsePanel("2. Append Table", actionButton("glAdd", "Add Modification"), HTML("<br></br>"),
        shinyFilesButton("glCSVadd", "Add Glossary File", "Choose Glossary File: CSV", F)),
      bsCollapsePanel("3. Export Data", downloadButton("glEXP", "Export Glossary"))), width = 3),
      mainPanel(DTOutput("GlossTab", width = "100%", height = "500px"))))),
   
   ########################################
   ## ADD CONSISTENT EXPORT IMAGE BUTTON ##
   ########################################
   div(id = "js_imgbutton", style = "position:absolute;top:8px;right:16px;z-index:1000", 
       actionButton("exportIMG", "Export Snapshot Images")),   
   
   #####################
   ## CHANGE DEFAULTS ##
   #####################
   
   # Change navigation bar color/text ls
   tags$style(type = 'text/css', 
              '.navbar { background-color: #000000; font-family: Calibri; font-size: 15px;}',
              '.navbar-default .navbar-brand { color: #4EBA19;}'),
   
   # Set icon
   tags$head(tags$link(rel="icon", href="favicon.ico", type="image/x-icon")),
   
   # Resize modal boxes
   tags$head(tags$style(".modal-dialog{ width:1000px}"))
)

# Here is what the computer does in the backend to generate displays in the UI
server <- function(input, output, session) {
  
  # This increases the alotted upload file to 10GB and supresses all warnings
  options(shiny.maxRequestSize = 10000*1024^2, warn = -1) 
  
  # Keep track of app start
  appStart <- reactiveValues(start = TRUE)
  
  # Keep track of output file handles while running database search algorithms
  DS <- reactiveValues(MSGF = NULL, MSPF = NULL)
  
  # Keep track of reactive values: testSeq, exporting images and autogenerated PTMS, 
  revals <- reactiveValues(testSeq = NULL, imgData = list(), 
              PTMs = list(), PTMdf = NULL, PTMmarkdown = F, PTMread = 0,
              exportPeakNum = NULL)
  
  # Keep track of specified search PTMs
  search <- reactiveValues(posMod = list())
  
  # Keep track of the current version of each plot and names
  plots <- reactiveValues(currSPEC = NULL, currHM = NULL, currXIC = NULL,
             currSSBAR = NULL, currFLAG = NULL, currMPPRE = NULL, 
             currMPNEXT = NULL, currMATCH = NULL, currVPSPEC = NULL, 
             currVPHM = NULL, currVPFLAG = NULL, currVPPRE = NULL,
             currVPNEXT = NULL, currPTBAR = NULL, 
             currLSEQ = NULL, currSM = NULL, currPMFM = NULL, 
             index = 0, CSVindex = 0, MDindex = 0, 
             plotSizes = data.frame("PlotName" = "PlotName", "Size" = "Size",
             stringsAsFactors = F))
  
  # Keep track of Glossary reactive values
  gloss <- reactiveValues(molForm = "Generated Molecular Formula Appears Here",
                          addedMods = NULL)
  
  # Initialize MS1FT file path
  ms1ftPath <- reactiveVal(NULL)
  
  # Load description data 
  Desc <- data.frame(read_excel(file.path("Server", "Pop_Up_Functions", "Function_Descriptions.xlsx")))
  
  # Set environment variables
  Environment <- read.csv("/SetEnvironment.csv", header = T)
  
  #######################
  ## 0. WELCOME SERVER ##
  #######################
  
  # Code for the welcome page is in 0_Welcome
  source(file.path("Server", "0_Welcome.R"), local = T)$value
  
  #########################
  ## 1. UPLOAD FUNCTIONS ## 
  #########################
  
  # File.choose only works in explorer. Thus, here is browser independent call. 
  source(file.path("Server", "1_UploadData.R"), local = T)$value
  
  ######################
  ## GETTER FUNCTIONS ## 
  ######################
  
  # Get the filtering getters: getFileType, getActMet, getActMetIon, 
  # getIntenMin, and getTol.
  source(file.path("Server", "Get", "Get_Filters.R"), local = T)$value
  
  # Get the MS-based getters: getRAW, getRAWScanSM, getMZMS, getHeader, getSSPeak, 
  # getAllMS1, getMS1DF. 
  source(file.path("Server", "Get", "Get_MS.R"), local = T)$value
  
  # Get all the ID-based getters: getID, getModDF, getFrag, getOriSDF, getFlagDF, 
  # getErrorHM, and getPTID.
  source(file.path("Server", "Get", "Get_ID.R"), local = T)$value
  
  # Get fasta file
  getFasta <- reactive({
    req(fastaPath())
    return(read.fasta(fastaPath(), seqtype = "AA", as.string = T))
  })
  
  # Get objects that contain a mix of either MS, ID, or FASTA data. Included:
  # getHDID, and getProteinTree.
  source(file.path("Server", "Get", "Get_Multiple.R"), local = T)$value
  
  # Get feature plot objects
  source(file.path("Server", "Get", "Get_Features.R"), local = T)$value
  
  # Get Unimod Glossary
  source(file.path("Server", "Get", "Get_Glossary.R"), local = T)$value
  
  #############################
  ## 2. SCAN & XIC FUNCTIONS ##
  #############################
  
  # Generate the XIC 
  source(file.path("Server", "2_ScanXIC", "XIC.R"), local = T)$value
  
  # Make the scan and seq graphs and tables
  source(file.path("Server", "2_ScanXIC", "ScanSeq.R"), local = T)$value
  
  # Display coverage data in sidebar
  source(file.path("Server", "2_ScanXIC", "Coverage.R"), local = T)$value
  
  # Plot the Error Heat Map 
  source(file.path("Server", "2_ScanXIC", "ErrorMap.R"), local = T)$value
  
  # Plot the Precursors
  source(file.path("Server", "2_ScanXIC", "Precursors.R"), local = T)$value
  
  ################################
  ## 3. VISUALIZE PTM FUNCTIONS ##
  ################################
  
  # Get modification 
  source(file.path("Server", "Get", "Get_Mod.R"), local = T)$value
  
  # Get server information for spectra information
  source(file.path("Server", "3_VisualizePTM", "VisPTM.R"), local = T)$value
  
  # Get server information for error map, sequence with flags, and precursor
  source(file.path("Server", "3_VisualizePTM", "OtherPlots.R"), local = T)$value
  
  ###################################
  ## 4. PROTEIN COVERAGE FUNCTIONS ##
  ###################################
  
  # Generate Protein Coverage figures and tables
  source(file.path("Server", "4_ProteinCoverage.R"), local = T)$value
  
  ###############################
  ## DATABASE SEARCH FUNCTIONS ##
  ###############################
  
  # Bottom Up: MSGF+ Interface
  source(file.path("Server", "DatabaseSearch", "BottomUp.R"), local = T)$value
  
  # Top Down: MSPathFinder Interface
  source(file.path("Server", "DatabaseSearch", "TopDown.R"), local = T)$value
  
  ###################################
  ## A. SPECTRA METADATA FUNCTIONS ##
  ###################################
  source(file.path("Server", "A_SpectraMetadata.R"), local = T)$value
  
  #####################################
  ## B. PROMEX FEATURE MAP FUNCTIONS ##
  #####################################
  source(file.path("Server", "B_ProMexFeatureMap.R"), local = T)$value
  
  ##################################
  ## C. UNIMOD GLOSSARY FUNCTIONS ##
  ##################################
  source(file.path("Server", "C_Glossary.R"), local = T)$value
  
  ########################
  ## EXPORT IMAGE & CSV ##
  ########################
  source(file.path("Server", "Export", "ExportIMG.R"), local = T)$value
  
  source(file.path("Server", "Export", "ExportCSV.R"), local = T)$value
  
  #########################
  ## POPOVER INFORMATION ##
  #########################
  source(file.path("Server", "Pop_Up_Functions", "Pop_Up_Functions.R"), local = T)$value
  
}  

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = T))

# Dedicated to Jay and Jen Degnan. Love you both, forever. <3
