library(shiny)


shinyUI(fluidPage(

    titlePanel("Optimal Big Day"),

    # Sidebar 
    sidebarLayout(
        
        # note: these can't rely on ebird_data going forward 
        # since we won't include that data in the shiny upload 
        # we'll probably need a file/function that matches county codes to names
        sidebarPanel(
            selectInput("state",
                        label = "Select a state:",
                        choices = unique(ebird_data$state)),
            selectInput("county",
                        label = "Select a county:",
                        choices = unique(ebird_data_filtered$county)),
            actionButton("goButton", "Go!")
        ),
        
        # main layout!
        mainPanel(
            p(
                "At some point we'll put meaningful things here!"
            ), 
            
            textOutput("buttonResponse")
        )
        
    )
))
