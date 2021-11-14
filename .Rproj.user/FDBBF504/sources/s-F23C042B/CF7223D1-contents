#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  sidebar_selected <- c()
  
  vals <- reactiveValues(sidebar_selected = sidebar_selected)
  
  mod_sidebar_server("sidebar_ui_1", vals)
  
  # Below observer handles module calling depending on sidebar selected
  
  observeEvent(vals$sidebar_selected,{
    # Sidebar menu has to be selected only once in order to call the corresponding module
    if (sum("server" == vals$sidebar_selected)==1) {
      
      mod_server_server("server_ui_1")
      
      vals$sidebar_selected <- c(vals$sidebar_selected,"server")
    }
    
  })
 
  
}
