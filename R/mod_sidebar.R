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
  ns <- shiny::NS(id)
  tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        "MAP is a shiny app that allows easy interaction with summary statistics 
        and plots for Kenya epidiemological data",
        shiny::tags$br(),
        shiny::tags$br(),
        
        shiny::selectizeInput(ns("county"), shiny::tags$b("County"),
                              NULL,options = list(create = TRUE)), #counties in fct_data.R
        
        shinyBS::bsButton(ns("generate"), label = "Generate")
            ),
      shiny::mainPanel(
        shiny::tabsetPanel(id=ns("mainpaneltabs"),
                           shiny::tabPanel("Map",
                                  shiny::fluidRow(
                                    shiny::column(3,
                                        shiny::selectInput(ns("indicator"),"Indicator",choices=indicator),
                                                                
                                        shiny::selectInput(ns("age"),"Age",NULL)
                                        ),
                                    shiny::column(9,
                                             leaflet::leafletOutput(ns("map"), height = "600") 
                                      )
                                    )
                                           ),
                           shiny::tabPanel("Table", DT::dataTableOutput(ns("table")))
                           
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
  ##---------Render Map
  mapOutput<-shiny::reactive({ 
    shiny::req(input$age)
    map(hiv_estimate,input$age,input$indicator)})
  m1<-shiny::reactive({map(hiv_estimate,input$age,input$indicator)})
  output$map<-leaflet::renderLeaflet({
   mapOutput()
  })
  #Dynamically change select input based on first chioce
  selectIndicatorData<-reactive({
    dplyr::filter(hiv_estimate,Indicator==input$indicator)
  })
  shiny::observeEvent(selectIndicatorData(),{
    choices<-unique(selectIndicatorData()$Age)
    shiny::freezeReactiveValue(input, "age")
    shiny::updateSelectInput(session,"age",choices = choices,selected = NULL)
  })
  ##Update select inpluts
  shiny::updateSelectizeInput(session, 'county', choices = counties, server = TRUE)
  
  ##----Table data
  hiv_data<-reactive({
    hiv_estimate %>% dplyr::filter(County ==input$county)
  })
 
  #-------Dynamically add report download tab
  rv_gen <- shiny::reactiveValues(generate_flag = 0) #reactive value to check if genrate button has been clicked
  shiny::observeEvent(input$generate, {
     rv_gen$generate_flag<- rv_gen$generate_flag + 1
    if(rv_gen$generate_flag == 1){
    shiny::insertTab(inputId = "mainpaneltabs",
                     shiny::tabPanel("Summary",
                                     shiny::fluidRow(shiny::downloadButton(ns("downloadData"),"Download")),
                                     shiny::fluidRow(DT::dataTableOutput(ns("table2"),"75%")),
                                       
                                       shiny::tags$br(),
                                       shiny::tags$br(),
                                     shiny::fluidRow(leaflet::leafletOutput(ns("map2"),"75%","600"))
                                     ),
                     target = "Table",
                     position = "after",
                     select = TRUE
    )
     
    } else{
      shiny::showNotification("Data already generated!!", type ="warning")
      shiny::updateTabsetPanel(session, "mainpaneltabs", selected="Summary")
    }
  })
  
  ##-------Generate report
 
  
  output$table2<-DT::renderDataTable({
   hiv_data()
  })
  output$map2<-leaflet::renderLeaflet({
    mapOutput()
  }) 
  output$downloadData <- shiny::downloadHandler(

    filename = paste0("Report_", Sys.Date(), ".html"),

    content = content <- function(file) {

      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(county=input$county,mapOutput = mapOutput(),
                     age=input$age, indicator = input$indicator)
      
      id <- shiny::showNotification(
        "Rendering report...", 
        duration = NULL, 
        closeButton = FALSE,
        type = "message"
      )
      
      on.exit(shiny::removeNotification(id), add = TRUE)
      rmarkdown::render(tempReport,
                        output_file = file,
                        output_format = "html_document",
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    },
    contentType = "text/html"
    )
  ##----------Render Table
  
  output$table<-DT::renderDataTable({
    DT::datatable(hiv_data())
  })
                     
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# callModule(mod_sidebar_server, "sidebar_ui_1")
 
