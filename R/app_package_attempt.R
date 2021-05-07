# library(tidyverse)
# library(shiny)
# library(leaflet)

# source('R/find_hotspots.R')

# countycodes <- read_csv('data/counties.csv')
# hotspots <- read_csv('data/hotspots.csv')

# Define UI for application that draws a histogram

#' @import shiny
ui <- function() {tagList(
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
          leaflet::leafletOutput("map"),
          
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
)}

# Define server logic required to draw a histogram
#' @import shiny 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
server <- function(input, output) {
  
  countycodes <- readr::read_csv('data_local/counties.csv')
  hotspots <- readr::read_csv('data_local/hotspots.csv')
  
    output$stateSelect <- renderUI({
        selectInput("stateSelect",
                    label = "Select a state: ", 
                    choices = as.character(unique(countycodes$state)))
    })
    
    
    output$countySelect <- renderUI({
      req(input$stateSelect)
        counties <- as.list(countycodes[countycodes$state == input$stateSelect, "county"])

        selectInput("countySelect",
                    label = "Select a county:",
                    choices = unique(counties))
    })
    
    output$includeThese <- renderUI({
        req(input$countySelect)
        # ccode <- countycodes %>%
        #     dplyr::filter(.data$county == input$countySelect)[[3]]
        ccode <- countycodes[countycodes$county == input$countySelect, "county_code"]
      
      
        hspots <- hotspots[hotspots$county_code == ccode, "locality"]

        selectInput("includeThese",
                    label = "Include these: ",
                    choices = unique(hspots),
                    selected = NULL,
                    multiple = TRUE)
    })
    
    observeEvent(input$goButton, {

        # get the county code for the current selection
        # ccode <- countycodes %>% 
        #     dplyr::filter(.data$county == input$countySelect)[[3]]
      
        ccode <- countycodes[countycodes$county == input$countySelect, "countySelect"]
        
        # get the prob_per_loc for this county
        filename <- paste('data_local/', ccode, '_prob_per_loc.csv', sep='')
        
        prob_per_loc <- readr::read_csv(filename) %>% 
            drop_effort_cols()
        
        # filtering
        visitThese <- hotspots %>%
            dplyr::filter(.data$locality %in% input$includeThese)[[2]]
        
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
            dplyr::filter(.data$locality %in% bestH) 
        
        # update pins and map view
        leaflet::leafletProxy('map') %>% 
            leaflet::clearMarkers() %>%
            leaflet::addMarkers(lng = pin_locations$longitude, 
                       lat = pin_locations$latitude,
                       popup = paste("<b>", pin_locations$locality, "</b><br>")) %>% 
            leaflet::setView(lng = mean(pin_locations$longitude),
                        lat = mean(pin_locations$latitude),
                        9)
                
        
    })
    
    
    output$map <- leaflet::renderLeaflet({
        
        # TODO: update zoom on county select
        leaflet::leaflet() %>% 
            leaflet::addTiles() %>% 
            leaflet::setView(-73, 44, 7)
    })
    
}

# Run the application 
myApp <- function(ui, server) {
  shiny::shinyApp(ui = ui, server = server)
}
