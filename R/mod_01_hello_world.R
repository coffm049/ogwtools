#' 01_hello_world UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_01_hello_world_ui <- function(id){
  ns <- NS(id)
  tagList(
    # ask for user text input
    textInput(NS(id, "txt"), "Enter some text"),
    plotOutput(NS(id, "plot"))
  )
}
    
#' 01_hello_world Server Functions
#'
#' @noRd 
mod_01_hello_world_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    data <- reactiveFileReader(1000, session, filePath = ns("txt"))
    output$plot <- renderPlot({
               plot(data$x, data$y)
    }, res = 96)
  })
}
    
## To be copied in the UI
# mod_01_hello_world_ui("01_hello_world_1")
    
## To be copied in the server
# mod_01_hello_world_server("01_hello_world_1")
