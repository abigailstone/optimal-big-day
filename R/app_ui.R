#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      
      tagList(
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
      )
      
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'optimalBigDay'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

