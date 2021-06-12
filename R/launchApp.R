#' launches the optimalBigDay shiny app
#'
#' @export launchApp
#'
#' @return shiny application object
#' 
launchApp <- function() {
   shiny::shinyApp(ui = ui(), server = server)
}