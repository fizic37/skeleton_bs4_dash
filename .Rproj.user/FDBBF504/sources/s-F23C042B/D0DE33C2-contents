#' server UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_server_ui <- function(id){
  ns <- NS(id)
  tagList(
  DT::dataTableOutput(ns("my_table"))
  )
}
    
#' server Server Functions
#'
#' @noRd 
mod_server_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$my_table <- DT::renderDataTable( DT::datatable(data = mtcars, rownames = FALSE,
          options = list(dom = "tp"),
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
          "UI of server module. Note the server module is being called inside app_server.R as it contains
          server side processing.") ) )
    
  })
}
    
## To be copied in the UI
# mod_server_ui("server_ui_1")
    
## To be copied in the server
# mod_server_server("server_ui_1")
