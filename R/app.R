library(tidyverse)
library(shiny)
library(leaflet)

<<<<<<< HEAD

countycodes <- read_csv('../data/counties.csv')
hotspots <- read_csv('../data/hotspots.csv')
=======
source('R/find_hotspots.R')

countycodes <- read_csv('data/counties.csv')
hotspots <- read_csv('data/hotspots.csv')
>>>>>>> 00eebf71627f0cca72b26b566975be52e0199270

# Define UI for application that draws a histogram
ui <-tagList(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Courier New';
      }
      h2 {
        font-family: 'Courier New';
      }"))
  ),
  
  navbarPage(
    "Optimal Big Day",
    tabPanel(
      "Home", 
      sidebarLayout(
        sidebarPanel(
          htmlOutput("stateSelect"),
          
          htmlOutput("countySelect"),
          
          numericInput("nHotspots",
                       label = "Number of hotspots to visit:",
                       value = 5,
                       min = 1, 
                       max = 15),
          
          htmlOutput("includeThese"),
          
          actionButton("goButton", "Go!")
        ),
        mainPanel(
          leafletOutput("map"),
          
          textOutput("status"), 
          
          # textOutput("bestSpots")
          htmlOutput("bestSpots")
        )
      )
    ), # end main tab panel 
    tabPanel(
      "About",
      p("hello, world!"),
      p(
      tags$a(href="https://github.com/abigailstone/optimal-big-day", "Source code")
      )
    ) # end about tab panel
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$stateSelect <- renderUI({
        selectInput("stateSelect",
                    label = "Select a state: ", 
                    choices = as.character(unique(countycodes$state)))
    })
    
    
    output$countySelect <- renderUI({
        counties <- countycodes[countycodes$state == input$stateSelect, "county"]
        
        selectInput("countySelect",
                    label = "Select a county:",
                    choices = unique(counties))
    })
    
    output$includeThese <- renderUI({
        ccode <- countycodes %>%
            filter(county == input$countySelect) %>%
            .[[3]]

        hspots <- hotspots[hotspots$county_code == ccode, "locality"]

        selectInput("includeThese",
                    label = "Include these: ",
                    choices = unique(hspots),
                    selected = NULL,
                    multiple = TRUE)
    })
    
    observeEvent(input$goButton, {
        
        # get the county code for the current selection
        ccode <- countycodes %>% 
            filter(county == input$countySelect) %>% 
            .[[3]]
        
        # get the prob_per_loc for this county
        filename <- paste('data/', ccode, '_prob_per_loc.csv', sep='')
        
        prob_per_loc <- read_csv(filename) %>% 
            drop_effort_cols()
        
        # filtering
        visitThese <- hotspots %>%
            filter(locality %in% input$includeThese) %>%
            .[[2]]
        
        # select best hotspots
        bestH <- select_hotspots(prob_per_loc, input$nHotspots, visitThese)
        
        # display the best results
        output$bestSpots <- renderUI({
            HTML(
                paste0("</br> <p> Optimal hotspots in ",
                       input$countySelect, " County, ", 
                       input$stateSelect,
                       ":</br> <ul> <li>", 
                       paste(bestH, collapse="</li> <li>"),
                       "</li><ul></p>")
            )
        })
        
        # get hotspot locations
        pin_locations <- hotspots %>% 
            filter(locality %in% bestH) 
        
        # update pins and map view
        leafletProxy('map') %>% 
            clearMarkers() %>%
            addMarkers(lng = pin_locations$longitude, 
                       lat = pin_locations$latitude,
                       popup = paste("<b>", pin_locations$locality, "</b><br>")) %>% 
            setView(lng = mean(pin_locations$longitude),
                    lat = mean(pin_locations$latitude),
                    9)
            
        
    })
    
    
    output$map <- renderLeaflet({
        
        # TODO: update zoom on county select
        leaflet() %>% 
            addTiles() %>% 
            setView(-73, 44, 7)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
