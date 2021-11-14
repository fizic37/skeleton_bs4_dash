#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  
  bs4Dash::sidebarMenuOutput(outputId = ns("sidebar"))
  
}
    
#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    admin_sidebar <- bs4Dash::sidebarMenu(   id = ns("tabs"),
      
      bs4Dash::menuItem( text = "Home",    icon = icon("home"),   tabName = "home"),
      
      bs4Dash::menuItem( text = "Administrator",  icon = icon("tools"),  tabName = "admin_main",
        
        bs4Dash::menuSubItem( text = "Database",  tabName = "database", icon = icon("database") ),
        
        bs4Dash::menuSubItem( text = "Servers", tabName = "server",  icon = icon("server")
        )
      )
    )
    
    output$sidebar <- bs4Dash::renderMenu(admin_sidebar)
    
    observeEvent(input$tabs,{ 
      # I built my sidebar selected vector and use it in the app_server.R in order to call the corresponding modules
        vals$sidebar_selected <- c(vals$sidebar_selected,input$tabs)
      })
    
  })
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_ui_1")
