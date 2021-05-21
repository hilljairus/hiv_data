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
  tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        "MAP is a shiny app that allows easy interaction with summary statistics 
        and plots for Kenya epidiemological data"
            ),
      shiny::mainPanel(
        shiny::tabsetPanel(id=ns("mainpaneltabs"),
                           shiny::tabPanel("Map","This is the map section"),
                           shiny::tabPanel("Table","Table section"),
                           shiny::tabPanel("Summary","Summary section")
                          )
            )
          )
  )
}
    
#' sidebar Server Function
#'
#' @noRd 
mod_sidebar_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")
 
