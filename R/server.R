library(shiny)
library(tidyverse)
library(leaflet)


shinyServer(function(input, output) {
   
    
    observeEvent(input$goButton, {
       
      # get the county code for the current selection
       ccode <- countycodes %>% 
          filter(county == input$countySelect) %>% 
          .[[3]]
       
       # get the prob_per_loc for this county
       filename <- paste('../data/', ccode, '_prob_per_loc.csv', sep='')
       prob_per_loc <- read_csv(filename)
       
       # filtering
       prob_per_loc <- drop_effort_cols(prob_per_loc)
       best_H <- select_hotspots(prob_per_loc, 5)
       

       # for testing 
       output$buttonResponse <-renderText({
          paste(input$countySelect, input$stateSelect, sep=", ")
      })
       
       # TODO: output formatting
       output$bestSpots <- renderText({
          best_H
       })
       
    })
   
   
   output$map <- renderLeaflet({
      
      # placeholder?
      leaflet() %>% 
         addTiles() %>% 
         setView(-73, 44, 7)
   })

})
