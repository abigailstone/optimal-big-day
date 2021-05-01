library(shiny)
library(leaflet)

shinyUI(fluidPage(

    titlePanel("Optimal Big Day"),

    # Sidebar 
    sidebarLayout(
        
        sidebarPanel(
            selectInput("stateSelect",
                        label = "Select a state:",
                        choices = unique(countycodes$state)),
            
            selectInput("countySelect", 
                        label = "Select a county:", 
                        choices = unique(countycodes$county)),
            
            actionButton("goButton", "Go!")
        ),
        
        # main layout!
        mainPanel(
            
            leafletOutput("map"),
            
            p(
                "At some point we'll put meaningful things here!"
            ), 
            
            textOutput("buttonResponse"), 
            
            textOutput("bestSpots")
        )
        
    )
))
