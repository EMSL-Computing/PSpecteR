## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_10_20

# DESCRIPTION: Contains the welcome GIF, how to use app GIF, and a link to the manual 

list(

  #############################
  ## RENDER IMAGES AND LINKS ##
  #############################
  
  # Add Welcome GIF which is stored in the www folder
  output$WelcomeGIF <- renderImage({
    list(src = file.path("www", "WelcomeToPSpecteR.gif"), contentType = "image/gif",
        width = "204px", height = "100px", align = "left")}, deleteFile = F),

  # Add "How to Use App" PNG which is stored in the www folder
  output$WelcomePNG <-  renderImage({
    list(src = file.path("www", "PSpecteR_Intro_Graphic.png"), contentType = "image/png",
          width = "572px", height = "350px", align = "left")}, deleteFile = F), 

  # Add link to the user manual 
  output$manual <- renderUI({
    tags$a(href = file.path("PSpecteR_Manual_ver1_0_0.html"), 
           target = "blank", "Click here to read the PSpecteR Manual!")
  }),
  
  # Add html output
  output$citation <- renderText({
    HTML("<p><strong>Please cite:&nbsp;</strong>Degnan et al. 2020</p>")
  })

)
