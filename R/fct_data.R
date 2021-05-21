`%>%` <- magrittr::`%>%`

hiv_estimate<-utils::read.csv("Data/hiv_estimates.csv",stringsAsFactors = FALSE)
counties<-unique(hiv_estimate$County)
age<-unique(hiv_estimate$Age)
indicator<-unique(hiv_estimate$Indicator)
county_geodata<-rgdal::readOGR("Data/cnty/County.shp")


###Build a reactive map
county_geodata@data[["Name"]][21]<-"Murang'a" #Correct typo

map<-function(data,age,indicator){
  selected<-data[(data[["Age"]]==age & data[["Indicator"]]==indicator),]
  #init<-county_geodata@data
  county_geodata@data<-dplyr::left_join(county_geodata@data,selected, 
                                        by=c("Name"="County"))
  pal<-leaflet::colorNumeric("Blues",county_geodata@data$Value)
  mytext <- paste(
    "County: ", county_geodata@data$Name,"<br/>", 
    "Value: ", county_geodata@data$Value, "<br/>", 
    sep="") %>%
    lapply(shiny::HTML)
  county_map<-leaflet::leaflet(county_geodata) %>% 
    leaflet::addPolygons(
      fillColor = ~pal(Value),
      weight = 2,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = leaflet::highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = mytext,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>% 
    leaflet::addLegend(position = 'topright', pal=pal, values = ~Value)
                        
                    
        
  
  county_map
   
}

# hivDataFilter<-function(section){
#   dplyr::filter(hiv_estimate, hiv_estimate$County==section & !is.na(hiv_estimate$Value))
#   
# }



