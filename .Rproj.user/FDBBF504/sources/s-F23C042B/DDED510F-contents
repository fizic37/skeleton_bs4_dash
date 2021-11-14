#' database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_ui <- function(id){
  ns <- NS(id)
  tagList(
  h4("This is UI of the database module. Note that I do not call mod_database_server('database_ui_1') within app_server.R,
  as there is no server side processing." )
  )
}
    
#' database Server Functions
#'
#' @noRd 
mod_database_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_database_ui("database_ui_1")
    
## To be copied in the server
# mod_database_server("database_ui_1")
