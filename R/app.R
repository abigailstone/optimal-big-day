library(shiny)

countycodes <- read_csv('../data/counties.csv')

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
            
            p(
                "At some point we'll put meaningful things here!"
            ), 
            
            textOutput("buttonResponse"), 
            
            textOutput("bestSpots")
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
        print(filename)
        
        prob_per_loc <- read_csv(filename) %>% 
            drop_effort_cols()
        
        # filtering
        bestH <- select_hotspots(prob_per_loc, 5)
        
        
        # for testing 
        output$buttonResponse <-renderText({
            paste(input$countySelect, input$stateSelect, sep=", ")
        })
        
        # TODO: output formatting
        output$bestSpots <- renderText({
            bestH
        })
        
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
