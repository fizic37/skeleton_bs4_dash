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
    bs4Dash::dashboardPage(skin = "blue", dark = FALSE,  title = "RiskReporting",
      sidebar = bs4Dash::dashboardSidebar( mod_sidebar_ui("sidebar_ui_1"),skin = "light",collapsed = TRUE),
      body = bs4Dash::dashboardBody( 
        bs4Dash::tabItems(
              bs4Dash::tabItem(tabName = "garantii"),
              bs4Dash::tabItem(tabName = "prudentialitate", mod_prudentialitate_ui("prudentialitate_ui_1")),
              bs4Dash::tabItem(tabName = "lista_banci", mod_rating_database_ui("rating_database_ui_1")),
              bs4Dash::tabItem(tabName = "new_rating",  mod_rating_ui("rating_ui_1")),
              bs4Dash::tabItem(tabName = "admin",  mod_admin_ui("admin_ui_1"))
              ) ),
                    header = bs4Dash::dashboardHeader(title = "Risk Reporting"))
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
      app_title = 'RiskReport'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    gfonts::use_font(css_path = "inst/app/www/montserrat_100.css",id = "montserrat")
  )
}

