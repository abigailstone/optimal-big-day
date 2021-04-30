library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
    
    observeEvent(input$goButton, {
        output$buttonResponse <-renderText({"ouch!"})
    })

})
