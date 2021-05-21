.onLoad <- function(...) { 
  
  # Create link to javascript and css files for shinyBs 
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS")) 
  
} 


  