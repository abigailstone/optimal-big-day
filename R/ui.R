#' Shiny app UI object
#' 
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
