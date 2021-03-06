#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

light<-bslib::bs_theme(version = 4, bootswatch = "cerulean")
dark<-bslib:: bs_theme(bg = "#101010", 
                       fg = "#FDF7F7", 
                       primary = "purple")
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage("Health Data by county", theme = light ,
               
               tabPanel("Application",
                        mod_sidebar_ui("sb")),
               tabPanel("Settings", icon = icon("gear"),
                        shinyWidgets::materialSwitch("dark_mode", "Dark Mode", status = "primary")
                        ),
               navbarMenu("More",
                          tabPanel("About",
                                   tags$h3("Source"),
                                   tags$a(href="https://nacc.or.ke/quarter-4-score-card-report/",
                                          icon("external-link"),"NACC")
                                   ))
      
    )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @import shinydashboard
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
 
  tags$head(
    
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'MAP'
    ),
    # Add here other external resources
    #shinybusy::add_busy_spinner(color="#F4F6F7",spin = "fading-circle"),
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    
    
  )
}

