################################################################################
################### PSpecteR, a proteomics QC Shiny App ########################
########### David Degnan, Pacific Northwest National Laboratory ################
####################### Last Updated: 2023_04_01 ###############################
################################################################################

# Load the package for Shiny App infrastructure
library(shiny)
library(shinycssloaders) 
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(shinyjqui)

# Load the pspecterlib package
library(pspecterlib)

# Load packages for interactive data visualization
library(data.table)
library(plotly)
library(DT)

# Add support packages
library(xlsx)

# If the test files are not in the right directory, copy and move them 
#source(file.path("Server", "PrepTestFiles.R"), local = T)$value
#prepTestFiles()

# Define function to render images in ui
imgRender <- function(id, path, width, height) {
  return(img(id = id, src = path, contentType = "image/png", width = width, height = height, align = "left"))
}

# Determine the version of the tool
LightVersion <- Sys.getenv("PSpecteRLight") == "1"

# Suppress all warnings
options(warn=-1)

# Build the interface in which the user will interact. Inverse colors on the 
# navigation bar will be used (black text as opposed to white). Title is PSpecteR.
ui <- navbarPage(id = "mainTabs", inverse = T, title = ifelse(LightVersion, "PSpecteR Light", "PSpecteR"), 
                                
   #############################
   ##  WELCOME USER INTERFACE ##
   #############################
   
   # Simply a welcome greeting and brief explanation of how to use this GUI.
   tabPanel("Welcome", mainPanel(
    
    # Enable shinyjs
    useShinyjs(),
    
    # Add Welcome Page GIF
    column(12, img(src = "WelcomeToPSpecteR.gif", contentType = "image/gif",
      width = "600px", height = "294px", align = "left")), 
    
    column(12, img(src = "Divider_Line.png", 
      width = "800px", contentType = "image/png", align = "left")),
    
    # Add Welcome Page PNG with buttons
    column(2, imgRender("Page2", "WelcomeImage/MSnXIC.png", "200px", "200px")),
    column(2, imgRender("Page3", "WelcomeImage/VisPTM.png", "200px", "200px")),
    column(2, imgRender("Page4", "WelcomeImage/ProteinCoverage.png", "200px", "200px")),
    column(6, imgRender("Page6", "WelcomeImage/AdditionalPlots.png", "200px", "200px")),
    column(12, img(src = "Divider_Line.png", contentType = "image/png", 
      width = "800px", align = "left")),
    
    # Add UI for the manual and citation information
    column(12, htmlOutput("citation")), column(12, htmlOutput("manual")))),
   
   ###########################
   ## UPLOAD USER INTERFACE ##
   ###########################
   
   # This panel allows users to upload their data. 
   tabPanel("Upload MS Data", 
            
  # For the mass spec files: raw, mzml, mzml.gz, mzxml, and mzxml.gz are supported
  sidebarLayout(sidebarPanel(
    
    # This CSS code changes the settings for all the sidebars in the GUI
    tags$style(type = 'text/css', '.well { background-color: #DFDCE3;
               font-family: Calibri; font-size: 16px; color: #000000;}'),
    
    # Upload an MS File: Shiny files button or type path
    bsCollapse(multiple = T, bsCollapsePanel(
      title = HTML('<p><strong>Mass Spectra (MS) FILE (Required)</strong></p>'),
      
      if (!LightVersion) {
        tagList(
          hr(), 
          textInput("mzmsHandle", "...or type in the MS file path", "", placeholder = "Type full MS file path with forward slashes"),
          list(actionButton("mzmsHandleGo", "Use MS Path"), actionButton("mzmsHandleClear", "Clear MS Path")),
          shinyFilesButton("mzmsFile", HTML("<strong>Search Folders:</strong> mzML, mzXML, raw"), 
                           "Choose MS File: mzML, mzXML, or raw", F)
         )
      } else {
        NULL

      },
    
      NULL)), width = 3),
   
    # All uploads include an output which contains filename, size, and number of 
    # datapoints for user sanity.  
    mainPanel(htmlOutput("msUpload"))),
  
  # The second side panel allows for a user to upload an optional mzID file.
  sidebarLayout(sidebarPanel(
    
    bsCollapse(bsCollapsePanel(title = HTML('<p><strong>Peptide Identification (ID) FILE (Optional)</strong></p>'),
      shinyFilesButton("idFile", HTML("<strong>Search Folders:</strong> mzid, mzID"), 
                       "Choose ID File: mzid", F), 
      
      if (!LightVersion) {
        tagList(
          hr(),
          textInput("idHandle", "...or type in the ID file path", "", placeholder = "Type full ID file path with forward slashes"),
          list(actionButton("idHandleGo", "Use ID Path"), actionButton("idHandleClear", "Clear ID Path"))
        )
      },
        
        
      NULL)), width = 3),
    
      # Like before, a succesful upload will include output which informs the user.
      mainPanel(htmlOutput("idUpload"))),
  
  # The third side panel allows for a user to upload an optional fasta file.
  sidebarLayout(sidebarPanel(
    bsCollapse(bsCollapsePanel(title = HTML('<p><strong>FASTA Protein Database (FA) FILE (Optional)</strong></p>'),
    shinyFilesButton("fastaFile", HTML("<strong>Search Folders:</strong> FASTA, FA"), 
                     "Choose FA File: FASTA (FA)", F), 
    
    if (!LightVersion) {
      tagList(
        hr(), 
        textInput("fastaHandle", "...or type in the FA file path", "", placeholder = "Type full FA file path with forward slashes"),
        list(actionButton("fastaHandleGo", "Use FA Path"), actionButton("fastaHandleClear", "Clear FA Path"))
      )
    },
    
    NULL)), width = 3),
    
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
   
   #############################
   ## MS & XIC USER INTERFACE ##
   #############################
   
   # This panel allows for interaction with MS2 fragment data.
   tabPanel("Visualize MS & XIC", 
            
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
       .irs--shiny .irs-bar {background: #A8D1A8;}
       .irs--shiny .irs-handle {background: #5CB85C;}
       .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {background-color: #5CB85C;}"))),
    
    bsCollapse(id = "ssCollapse", multiple = T, 
     open = list("Peak Matching Settings"),
     
     # Settings for the matching algorithm
     bsCollapsePanel("Peak Matching Settings", 
        
       # Allows users to select how far off the experimental fragment value can be from the theoretical fragment value.
       numericInput("ssTolerance", "M/Z Tolerance (PPM)", 10, min = 0.01, max = 100, step = 0.1),
       numericInput("ssIntenMin", "Intensity Minimum", 100, min = 0, max = 1e6, step = 100),
       numericInput("ssCorrScoreFilter", "Minimum Pearson Correlation Score", 0, min = 0, max = 1, step = 0.01),
       uiOutput("SelectedIons"), 
       uiOutput("ssISOspectraSWITCH"),
       hr(),
       actionButton("ManageIons", "Manage Ions"),
       hr(),
                     
       # Allow user test a different sequence: autofill with original sequence
       uiOutput("ssRenderNS"),
       
       # Reveal warnings if the input does not comply with necessary standard
       htmlOutput("ssNSWarn"), hr(),
       
       # Add an action button for adding modifications or restoring the sequence
       list(actionButton("ssASeq", "Apply Seq"),
            actionButton("ssRSeq", "Restore Seq"),
            actionButton("debug", "debug"))
       
     ),
     
     # Spectrum plot settings               
     bsCollapsePanel("Plot Settings", 
       
       bsCollapse(bsCollapsePanel("Spectrum Plot Settings",               
                     
         # Set the font size of the annotated
         numericInput("ssAnnoSize", "Label Size", 8, 0, 10, 1),
                       
         # Set the label relative label distance
         numericInput("ssLabDist", "Set Label Distance (M/Z)", 0, 0, 10, 0.1),
         
         # Enable letter annotation on the spectrum
         uiOutput("ssLetterSWITCH")
        
      ),
     
      bsCollapsePanel("MS1 Plots Settings", 
         
       # Most Abundant Isotope switch, Trace switch, and Pre MZ range
       numericInput("MPpercdiff", "Filter by Percent Error", 25),
       numericInput("MPwinsize", "Set MS1 Window Size", 3)
       
      ),
       
      bsCollapsePanel("Other Figures",
       
         # Set slider for adding annotations for PTMs
         uiOutput("ssAnoPTMSWITCH"),
         
         # Set slider for adding charges to sequence plots
         uiOutput("seqChargeSWITCH")
         
      )
      
    )),

     # Scan metadata table     
     bsCollapsePanel("Table Column Settings",               
                     
       # Converts the checked boxes to column numbers for easy datatable manipulation.
       # Certain datatable columns are automatically selected and presented.    
        pickerInput("ssScanMetadataCol", "Select Scan Metadata Table Columns", 
           options = list(`live-search` = T, `actions-Box` = T),
           c("Scan Number" = "1", "MS Level" = "2", "Retention Time" = "3", "Precursor M/Z" = "4", "Precursor Charge" = "5", 
             "Precursor Scan" = "6", "Activation Method" = "7", "Sequence" = "8", "Protein ID" = "9", 
             "Calculated Mass" = "10", "Experimental Mass" = "11", "Score" = "12", 
             "Q Value" = "13", "Decoy" = "14", "Description" = "15", "Protein Start Position" = "16",
             "Modifications" = "17", "Order" = "18"), 
           selected = as.character(c(1:7, 9:12, 16:18)), multiple = T), hr(),
       
        # Select columns for fragment table
        pickerInput("ssFragColumns", "Select Fragment Columns", 
           options = list(`live-search` = T, `actions-Box` = T),
           c("Ion", "N Position", "Residue", "Z", "Isotope", "M/Z Experimental", "Correlation Score"),
           selected = c("Ion", "N Position", "Residue", "Z", "Isotope", "M/Z Experimental", "Correlation Score"), multiple = T)
       
     ),
     
     # XIC Settings
     bsCollapsePanel("XIC Settings", 
                     
       # Set mass and charge
       uiOutput("massXIC"),
                     
       # Set a retention time window 
       numericInput("rtXIC", "Retention Time Window (min)", 2, 0.1, 100, 1),
       
       # Pick the number of isotopes to plot
       pickerInput("isoXIC", label = "Select Isotopes", choices = 0:5, 
                   selected = 0:2, multiple = T, options = list(`actions-Box` = T)), 
       
       # Pick the number of charge states to plot
       pickerInput("chargeTraceXIC", label = "Select Charges", choices = 1:3,
                   selected = 1:2, multiple = T, options = list(`actions-Box` = T)),
       
       # Line for smooth line 
       uiOutput("XICsmoothSWITCH")
       
     ),      

     # Export Images & Data
     bsCollapsePanel("Snapshot Images and Export Data", bsCollapse(multiple = T, 
       bsCollapsePanel("Snapshot Images",               
         actionButton("imgSPEC", "Spectrum"),
         actionButton("imgHM", "Error Map"), HTML("<p></p>"), 
         actionButton("imgMPPRE", "Previous MS1"),
         actionButton("imgMPNEXT", "Next MS1"), HTML("<p></p>"),
         actionButton("imgXIC", "XIC"),
         actionButton("imgSSBAR", "Barchart"), HTML("<p></p>"),
         actionButton("imgFLAG", "Sequence")), 
       bsCollapsePanel("Export Data",
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
      column(9, 
         tabsetPanel(id = "SStabs",
           tabPanel("Spectrum", jqui_resizable(plotlyOutput("ssSpectrum", width = "100%", height = "300px")) 
                    %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
           tabPanel("Error Map", jqui_resizable(plotlyOutput("ErrorMap", width = "100%", height = "300px")) 
                    %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
           tabPanel("XIC", 
                    jqui_resizable(plotlyOutput("XIC", width = "100%", height = "300px")) 
                    %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))))
      ),
        
      # Sequence View: A tabular display which includes an sequence view, 
      # interactive table with clicking functionality, 
      # and a bar chart which shows the number of fragment types.
      column(3, 
       jqui_draggable(
         tabsetPanel(id = "PTBtabs",
           tabPanel("MS1", jqui_resizable(plotlyOutput("ssMatPre", width = "150%", height = "300px")) 
                    %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
           tabPanel("Next MS1", jqui_resizable(plotlyOutput("ssMatNext", width = "150%", height = "300px")) 
                    %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
           tabPanel("Filter Ions", jqui_resizable(DT::DTOutput("ssSeqTable", width = "125%", height = "300px"))))
       )
      ),
        
      # Scan View: The datatable with information as determined by the checkboxes
      # with a sequence tab to visualize the ions with the smallest ppm error. 
      column(12, 
         tabsetPanel(id = "SSAtabs",      
           tabPanel("Scan", htmlOutput("coverage"), 
                    jqui_resizable(DT::DTOutput("ssScan", width = "115%", height = "250px"))),
           tabPanel("Sequence", jqui_resizable(plotOutput("ssSeqFlag", width = "115%", height = "400px")) 
                    %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
           tabPanel("Ion Annotation", jqui_resizable(DT::DTOutput("ssAllFrag", width = "115%", height = "250px"))),
           tabPanel("Ion Barplot", jqui_resizable(plotlyOutput("ssSeqBar", width = "115%", height = "300px")) 
                    %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))))
      )))),

   #############################
   ## TEST PTM USER INTERFACE ##
   #############################
   
   # Enables exploration of how PTMs affect the spectra.
   tabPanel("Test PTMs",
            
    # Select the spectra
    sidebarLayout(sidebarPanel(
      bsCollapse(multiple = T, open = list("1. Set Sequence"),
       bsCollapsePanel("1. Set Sequence", htmlOutput("VPscanWarn"), hr(),
         uiOutput("VPseq"), htmlOutput("VPnsWarn"), hr(), 
         actionButton("VPrseq", "Restore Seq")
       ),
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
   
   #####################################
   ## PROTEIN COVERAGE USER INTERFACE ##
   #####################################
   
   # Allows user to explore matched ID data and answer questions about frequency,
   # patterns, protein families, etc. 
   tabPanel("Protein Coverage", 
            
    # Set the tolerance      
    sidebarLayout(sidebarPanel(
      bsCollapse(multiple = T, open = "1. Protein Coverage Settings",
       bsCollapsePanel("1. Protein Coverage Settings",
         selectInput("PTPlotScore", "Select a Score to Color the Coverage Plot by", c("Score", "QValue", "None"), "Score"),
         numericInput("PTToleranceQValue", "Q-Value Maximum", 1, min = 0, max = 1, step = 0.01),
         numericInput("PTToleranceScore", "Score Maximum (Enter values like: 1e-10)", 1, min = 0, max = 1),
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
          tabPanel("Coverage", htmlOutput("PTnoFAwarn"),
                   jqui_resizable(plotlyOutput("PTMatch", width = "100%", height = "350px"))
                   %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
          tabPanel("Bar", jqui_resizable(plotlyOutput("PTBar", width = "100%", height = "350px"))
                   %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c"))),
          tabPanel("Literature Sequence", jqui_resizable(plotOutput("LSeq", width = "100%", height = "350px"))
                   %>% withSpinner(type = 5, color = getOption("spinner.color", default = "#275d0c")))),
        DT::dataTableOutput("PTTable", width = "100%", height = "250px")))),
   
   ###############################
   ## DB SEARCH USER INTERFACES ##
   ###############################
  
  if (!LightVersion) {
   
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
        
      )))))
    
   },
   
   ################################
   ## ADDITIONAL USER INTERFACES ##
   ################################
   
   # Underneath the "More" drop down navigation bar menu are more useful programs.
   navbarMenu("More",
              
    ########################################
    ## SPECTRA METADATA USER INTERFACE ##
    ########################################
    
    # This tab displays a feature plot for visualization of the entire spec file
    tabPanel("Visualize Spectra Metadata", 
             
     # Remind users which tab they're in
     sidebarLayout(sidebarPanel(
       HTML('<p style="text-align: center;"><span style="font-size: 16pt;"><strong>
             SPECTRA METADATA PLOT</strong></span></p>'),
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
    tabPanel("ProMex Feature Map", sidebarLayout(sidebarPanel(
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
    tabPanel("Unimod Glossary", sidebarLayout(sidebarPanel(
      HTML('<p style="text-align: center;"><span style="font-size: 16pt;"><strong>
           UNIMOD GLOSSARY</strong></span></p>'), 
      bsCollapse(multiple = T, 
      bsCollapsePanel("Append Table", actionButton("glAdd", "Add Modification"), HTML("<br></br>"),
        shinyFilesButton("glCSVadd", "Add Glossary File", "Choose Glossary File: CSV", F)),
      bsCollapsePanel("Export Data", 
                      downloadButton("glEXP", "Export Glossary"),
                      downloadButton("glnewEXP", "Export Added Modifications"))), width = 3),
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
  
  # Keep track of reactive values: testSeq, exporting images and autogenerated PTMS, and added ions
  revals <- reactiveValues(testSeq = NULL, imgData = list(), 
              PTMs = list(), PTMdf = NULL, PTMmarkdown = F, PTMread = 0,
              exportPeakNum = NULL, AddedIons = NULL)
  
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
  Desc <- data.frame(xlsx::read.xlsx(file.path("Server", "Pop_Up_Functions", "Function_Descriptions.xlsx"), 1))
  
  # Set environment variables
  #Environment <- read.csv("/SetEnvironment.csv", header = T)
  
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
  
  # Source all getter functions
  source(file.path("Server", "Get.R"), local = T)$value
  
  #############################
  ## 2. SCAN & XIC FUNCTIONS ##
  #############################
  
  # Generate the XIC 
  source(file.path("Server", "2_ScanXIC", "XIC.R"), local = T)$value
  
  # Make the scan and seq graphs and tables
  source(file.path("Server", "2_ScanXIC", "ScanSeq.R"), local = T)$value
  
  # Plot the Precursors
  source(file.path("Server", "2_ScanXIC", "Precursors.R"), local = T)$value
  
  ################################
  ## 3. VISUALIZE PTM FUNCTIONS ##
  ################################
  
  # Get server information for spectra information
  #source(file.path("Server", "3_VisualizePTM", "VisPTM.R"), local = T)$value
  
  # Get server information for error map, sequence with flags, and precursor
  #source(file.path("Server", "3_VisualizePTM", "OtherPlots.R"), local = T)$value
  
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
