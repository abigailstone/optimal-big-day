#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 

  countycodes <- readr::read_csv('data_local/counties.csv', col_types = readr::cols())
  hotspots <- readr::read_csv('data_local/hotspots.csv', col_types = readr::cols())
  
  
  output$stateSelect <- renderUI({
    
    selectInput("stateSelect",
                label = "Select a state: ", 
                choices = as.character(unique(countycodes$state)), 
                selected = "Vermont")
    
  })
  
  output$countySelect <- renderUI({
    
    req(input$stateSelect)
    counties <- countycodes[countycodes$state == input$stateSelect, "county"]
    
    selectInput("countySelect",
                label = "Select a county:",
                choices = unique(counties), 
                selected = "Addison")
  })
  
  output$nHotspots <- renderUI({
    
    req(input$countySelect)
    ccode <- countycodes[countycodes$county == input$countySelect, "county_code"]
    hspots <- hotspots[hotspots$county_code == as.character(ccode), "locality"]
    n_hspots <- nrow(hspots)

    numericInput("nHotspots",
                 label = "Number of hotspots to visit:",
                 value = 5,
                 min = 1, 
                 # limit to number of hotspots in the county
                 max = min(15, n_hspots)) 
    
  })
  
  output$includeThese <- renderUI({
    
    req(input$countySelect)
    ccode <- countycodes[countycodes$county == input$countySelect, "county_code"]
    
    hspots <- hotspots[hotspots$county_code == as.character(ccode), "locality"]
    
    selectizeInput("includeThese",
                label = "Include these hotspots: ",
                choices = hspots,
                selected = NULL,
                multiple = TRUE, 
                # limit to the number of hotspots to visit
                options = list(maxItems = input$nHotspots))
  })
  
  observeEvent(input$goButton, {
    
    # get the county code for the current selection
    ccode <- countycodes[countycodes$county == input$countySelect, "county_code"] 
    
    # get the prob_per_loc for this county
    filename <- paste('data_local/', ccode, '_prob_per_loc.csv', sep='')
    
    # read the prob_per_loc for this county 
    prob_per_loc <- readr::read_csv(filename, col_types = readr::cols()) %>% 
      drop_effort_cols()
    
    # filtering
    visitThese <- (hotspots %>%
                     dplyr::filter(.data$locality %in% input$includeThese))[[2]]
    
    # select best hotspots
    bestH <- select_hotspots(prob_per_loc, input$nHotspots, visitThese)
    
    # get predicted locality totals and the predicted overall total
    predicted_totals <- pred_hotspot_totals(bestH, prob_per_loc)
    summary_total <- pred_total(bestH, prob_per_loc)
    
    # append predicted totals at each hotspot to the hotspot name
    bestH_probs <- paste(bestH, " (", 
                         floor(as.numeric(predicted_totals)), 
                         " species)",
                         sep="")
    
    # display the best results
    output$bestSpots <- renderUI({
      HTML(
        paste0("</br> <p> Optimal hotspots in ",
               input$countySelect, " County, ", 
               input$stateSelect,
               ":</br> <ul> <li>",
               # list formatting
               paste(bestH_probs, collapse="</li> <li>"),
               "</li></ul></p>",
               "<p> Predicted total: ",
               as.character(floor(as.numeric(summary_total))),
               " species",
               "</p>")
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
                          popup = paste0(
                            '<a href="', 'https://ebird.org/hotspot/', 
                            pin_locations$locality_id, '">', 
                            pin_locations$locality, '</a>')) %>% 
      leaflet::setView(lng = mean(pin_locations$longitude),
                       lat = mean(pin_locations$latitude),
                       9)
    
  })
  
  
  output$map <- leaflet::renderLeaflet({
    
    leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::setView(-73, 44, 7)
  })  
}
