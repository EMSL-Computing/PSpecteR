## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2023_04_01

# DESCRIPTION: Contains the welcome GIF, how to use app GIF, and a link to the manual 

list(

  ###########################
  ## RENDER LINKS AND TEXT ##
  ###########################

  # Render welcome image clicks
  shinyjs::onclick("Page2",
    updateTabsetPanel(session = session, inputId = "mainTabs", selected = "Visualize MS & XIC") 
  ),
  shinyjs::onclick("Page3",
    updateTabsetPanel(session = session, inputId = "mainTabs", selected = "Test PTMs") 
  ),
  shinyjs::onclick("Page4",
    updateTabsetPanel(session = session, inputId = "mainTabs", selected = "Protein Coverage") 
  ),
  shinyjs::onclick("Page6",
    updateTabsetPanel(session = session, inputId = "mainTabs", selected = "Visualize Spectra Metadata") 
  ),

  # Add link to the user manual 
  output$manual <- renderUI({
    tags$a(href = file.path("PSpecteR_Manual_ver1_0_0.html"), 
           target = "blank", "Click here to read the PSpecteR Manual!")
  }),
  
  # Add html output
  output$citation <- renderText({
    HTML('<p><strong><span style="font-size: 22px;">FIRST TIME USERS:&nbsp;</span></strong><span style="font-size: 22px;">
         <p><strong>Please cite:&nbsp;<a href="https://pubs.acs.org/doi/full/10.1021/acs.jproteome.0c00857">Degnan et al 2021</a></strong></p> 
         Please enable &quot;Description Mode&quot; under &quot;App Settings&quot; on the Uploads Page.&nbsp;</span></p>')
  })

)
