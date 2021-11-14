#' balanta_database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_balanta_database_ui <- function(id){
  ns <- NS(id)
  
  fluidPage( br(),
  fluidRow(
    column( width = 3,
            selectInput(inputId = ns("date_baza_balanta"),  label = "Selecteaza data raportului", 
                        choices = c()  ) ), br(),
    
    column(width = 3,   selectInput(ns("tip_sursa"),label = "Selecteaza tipul sursei",
                                    choices =  c()) ),
    
    column(width = 3,   selectInput(ns("tip_plasament"),label = "Selecteaza tipul plasamentului",
                                    choices = c()) ),
    
    column(width = 3,   shinyWidgets::pickerInput(ns("contrapartida"),label = "Selecteaza Contrapartida",
                              choices =  c(),options = list('live-search' = TRUE))  )
  ),
    
  DT::dataTableOutput(ns("baza_date_balanta"))
  )
  
}
    
#' balanta_database Server Functions
#'
#' @noRd 
mod_balanta_database_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    balanta_database <- readRDS("R/reactivedata/balanta/balanta_database.rds")
    
    vals_balanta_database <- reactiveValues(balanta_database = balanta_database)
   
    updateSelectInput(session = session,inputId = 'date_baza_balanta',
                      choices = unique(balanta_database$data_balanta) %>% sort(decreasing = TRUE))
    updateSelectInput(session = session,inputId = "tip_sursa", 
                      choices = c("all",unique(balanta_database$tip_sursa)))
    updateSelectInput(session = session,inputId = "tip_plasament",
                      choices = c("all",unique(balanta_database$tip_plasament)))
    
    tabela_nume_banci <- eventReactive(input$date_baza_balanta, { req(input$date_baza_balanta != "")
      readRDS("R/reactivedata/banci/tabela_nume_banci.rds") %>% 
      dplyr::filter(DataInitiala <= input$date_baza_balanta & DataExpirare >= input$date_baza_balanta) %>% dplyr::select(2:3)
      })
   
      observeEvent(input$date_baza_balanta,{ req(tabela_nume_banci(), input$date_baza_balanta != "")
        
        vals_balanta_database$balanta_database <- balanta_database %>%
            dplyr::left_join(tabela_nume_banci(), by = c("Banca" = "CodFinantator")) %>% 
            dplyr::mutate(DenumireFinantator = ifelse(is.na(DenumireFinantator),Banca,DenumireFinantator)) %>%
            dplyr::filter(data_balanta==input$date_baza_balanta)
        
        shinyWidgets::updatePickerInput(session, "contrapartida", choices =  c("all", vals_balanta_database$balanta_database %>%
                              dplyr::pull(DenumireFinantator) %>% unique()  )  )
        })
        
        
        
    view_surse <- reactive( { req(tabela_nume_banci(), input$contrapartida != "") 
      vals_balanta_database$balanta_database %>% 
        #dplyr::filter(data_balanta == as.Date(input$date_baza_balanta)) %>% 
        dplyr::filter(if (input$tip_sursa=="all") TRUE else tip_sursa==input$tip_sursa) %>%
        dplyr::filter(if (input$tip_plasament=="all") TRUE else tip_plasament==input$tip_plasament) %>%
        dplyr::filter(if (input$contrapartida=="all") TRUE else DenumireFinantator==input$contrapartida) %>%
        dplyr::select(-tip_sursa, -Banca) %>%
        dplyr::group_by(DenumireFinantator, tip_plasament) %>%
        tidyr::pivot_wider(names_from = tip_plasament,
                           values_from =  `Solduri finale|Debit`,values_fill = 0 ) %>% 
        dplyr::select(-`Simbol cont`,- `Denumire cont`,-data_balanta) %>%
        dplyr::summarise_all(.funs = ~sum(.)) %>%
        dplyr::mutate(Expunere_totala = rowSums(dplyr::select_if(.,is.numeric)))
      })
      
     
      output$baza_date_balanta <- DT::renderDataTable({ req(view_surse())
        DT::datatable(data = view_surse() %>%
                        janitor::adorn_totals(where = "row",fill = "-") %>%
                        dplyr::arrange(desc(Expunere_totala)), rownames = FALSE,
                      options = list(dom = "Bt", buttons = c("copy","csv","excel"), pageLength = nrow(view_surse())+1), 
                      extensions = "Buttons", caption = "Expunerea pe contrapartide") %>%
          DT::formatRound(columns = 2:ncol(view_surse()),digits = 0) 
          # It does not work. I am guessing it does not receive a datatable after it was formatted Round
          #DT::formatStyle(table = .,columns = 0,target = "row",fontWeight = DT::styleEqual(1, "bold")
           })
      
    })
    
    
 
  
}
    
## To be copied in the UI
# mod_balanta_database_ui("balanta_database_ui_1")
    
## To be copied in the server
# mod_balanta_database_server("balanta_database_ui_1")
