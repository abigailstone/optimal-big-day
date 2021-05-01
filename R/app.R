library(shiny)

countycodes <- read_csv('../data/counties.csv')
hotspots <- read_csv('../data/hotspots.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
    
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
            
            textOutput("status"), 
            
            # textOutput("bestSpots")
            htmlOutput("bestSpots")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$goButton, {
        
        # get the county code for the current selection
        ccode <- countycodes %>% 
            filter(county == input$countySelect) %>% 
            .[[3]]
        
        # get the prob_per_loc for this county
        filename <- paste('../data/', ccode, '_prob_per_loc.csv', sep='')
        
        prob_per_loc <- read_csv(filename) %>% 
            drop_effort_cols()
        
        # filtering
        bestH <- select_hotspots(prob_per_loc, 5)
        
        # display the best results
        output$bestSpots <- renderUI({
            HTML(
                paste0("Optimal hotspots in ",
                       input$countySelect, ", ", 
                       input$stateSelect,
                       ":</br>", 
                       paste(bestH, collapse="</br>"))
            )
        })
        
        # get hotspot locations
        pin_locations <- hotspots %>% 
            filter(locality %in% bestH) 
        
        # update pins on map
        leafletProxy('map') %>% 
            clearMarkers() %>%
            addMarkers(lng = pin_locations$longitude, 
                       lat = pin_locations$latitude,
                       popup = paste("<b>", pin_locations$locality, "</b><br>"))
            
        
    })
    
    
    output$map <- renderLeaflet({
        
        # placeholder?
        leaflet() %>% 
            addTiles() %>% 
            setView(-73, 44, 7)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
