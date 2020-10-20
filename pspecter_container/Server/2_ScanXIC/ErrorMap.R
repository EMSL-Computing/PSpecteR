## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_07_14

# DESCRIPTION: Generates the error map for 2. Scan & Seq

output$ErrorMap <- renderPlotly({
  
  # The Error Heat Map is a dataframe built specifically for this graphic
  req(getErrorHM())
  EHM <- getErrorHM()
  
  # Define specific red to white to blue colorscale
  RWB <- list(list(0, 'rgb(179,0,17)'),
              list(0.05, 'rgb(179,0,17)'), list(0.05, 'rgb(189,23,31)'),
              list(0.10, 'rgb(189,23,31)'), list(0.10, 'rgb(199,51,45'), 
              list(0.15, 'rgb(199,51,45'), list(0.15, 'rgb(213,83,61)'), 
              list(0.20, 'rgb(213,83,61)'), list(0.20, 'rgb(225,111,75)'), 
              list(0.25, 'rgb(225,111,75)'), list(0.25, 'rgb(238,140,89)'), 
              list(0.30, 'rgb(238,140,89)'), list(0.30, 'rgb(246,167,112)'), 
              list(0.35, 'rgb(246,167,112)'), list(0.35, 'rgb(246,184,139)'), 
              list(0.40, 'rgb(246,184,139)'), list(0.40, 'rgb(238,201,175)'), 
              list(0.45, 'rgb(238,201,175)'), list(0.45, 'rgb(220,220,220)'), 
              list(0.50, 'rgb(220,220,220)'), list(0.50, 'rgb(255,255,255)'), 
              list(0.55, 'rgb(255,255,255)'), list(0.55, 'rgb(203,207,230)'), 
              list(0.60, 'rgb(203,207,230)'), list(0.60, 'rgb(185,194,239)'), 
              list(0.65, 'rgb(185,194,239)'), list(0.65, 'rgb(137,161,241)'), 
              list(0.70, 'rgb(137,161,241)'), list(0.70, 'rgb(104,138,250)'), 
              list(0.75, 'rgb(104,138,250)'), list(0.75, 'rgb(87,121,248)'), 
              list(0.80, 'rgb(87,121,248)'), list(0.80, 'rgb(63,95,238)'), 
              list(0.85, 'rgb(63,95,238)'), list(0.85, 'rgb(41,66,197)'), 
              list(0.90, 'rgb(41,66,197)'), list(0.90, 'rgb(29,50,188)'), 
              list(0.95, 'rgb(29,50,188)'), list(0.95, 'rgb(17,36,183)'), 
              list(1, 'rgb(17,36,183)'))
  
  HM <- plot_ly(EHM, x = EHM$Pos, y = EHM$Type, z = EHM$MatchScore, type = "heatmap", 
          colorscale = RWB, reversescale = T, hoverinfo = "text", xgap = 3, ygap = 3,
          hovertext = paste("Amino Acid:", EHM$Let, "<br>Ion Type:", EHM$Type, 
          "<br>PPM Error:", round(as.numeric(EHM$MatchScore), 4))) %>% 
        layout(xaxis = list(title = "Amino Acid", tickvals = EHM$Pos,
          ticktext = EHM$Let), yaxis = list(title = "Ion Type"), 
          plot_bgcolor = "#000000")
  
  plots$currHM <- HM
  
  HM 
  
})