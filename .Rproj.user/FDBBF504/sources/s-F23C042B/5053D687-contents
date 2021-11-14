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
   
    bs4Dash::dashboardPage(   title = "App Title",
      header = bs4Dash::dashboardHeader(title = "Header Title", status = "#b9f4e3"),
      sidebar = bs4Dash::bs4DashSidebar(mod_sidebar_ui("sidebar_ui_1"), status = "teal",skin = "light"),
      footer = bs4Dash::bs4DashFooter(left = "Developed by Tita Marius", right = "Golem framework"),
      
      body = bs4Dash::bs4DashBody( bs4Dash::tabItems(
        bs4Dash::tabItem(tabName = "database", mod_database_ui("database_ui_1") ),
        bs4Dash::tabItem(tabName = "server", mod_server_ui("server_ui_1") )
        ) )
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
      app_title = 'Bs4Skeleton'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

