#' Shiny app server function
#' 
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
server <- function(input, output) {
   
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
      counties <- as.list(countycodes[countycodes$state == input$stateSelect, "county"])
      
      selectInput("countySelect",
                  label = "Select a county:",
                  choices = unique(counties), 
                  selected = "Addison")
   })
   
   output$includeThese <- renderUI({
      
      req(input$countySelect)
      ccode <- countycodes[countycodes$county == input$countySelect, "county_code"]
      
      hspots <- hotspots[hotspots$county_code == as.character(ccode), "locality"]
      
      selectInput("includeThese",
                  label = "Include these: ",
                  choices = unique(hspots),
                  selected = NULL,
                  multiple = TRUE)
   })
   
   observeEvent(input$goButton, {
      
      # get the county code for the current selection
      ccode <- countycodes[countycodes$county == input$countySelect, "county_code"] 
      
      # get the prob_per_loc for this county
      filename <- paste('data_local/', ccode, '_prob_per_loc.csv', sep='')
      
      prob_per_loc <- readr::read_csv(filename, col_types = readr::cols()) %>% 
         drop_effort_cols()
      
      # filtering
      visitThese <- (hotspots %>%
                        dplyr::filter(.data$locality %in% input$includeThese))[[2]]
      
      # select best hotspots
      bestH <- select_hotspots(prob_per_loc, input$nHotspots, visitThese)
      predicted_totals <- pred_hotspot_total(bestH, prob_per_loc)
      
      # separate the hotspots and the predicted total
      pred_total <- tail(bestH, n=1)
      bestH <- head(bestH, n=input$nHotspots)
      
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
                   as.character(floor(as.numeric(pred_total))),
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
                             popup = paste("<b>", pin_locations$locality, "</b><br>")) %>% 
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