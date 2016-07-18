############
# The Data Incubator Challenge
# Author: JR Mihaljevic
# Date: 18 July 2016
# Language: R
############

####################################
# Shiny App 1
####################################

#-------------------------------------------------
#-------------------------------------------------

library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(geojsonio)
#-------------------------------------------------
#-------------------------------------------------

####################################
# Parks data set
####################################
parks_geo <- geojson_read("parks.geojson", method="local", what="sp")
parks_latlong <- read_csv("parks_latlong.csv")


####################################
# Crimes data set
####################################

crime <- read_csv(file="crimes.csv")
crime <- data.frame(crime)
crime <- na.omit(crime)

crime <- sample_frac(crime, size=0.5)

####################################
# The Server
####################################

server <- function(input, output, session){
  
  output$crime_select <- renderUI({
    selectInput("crime.select", "Which crimes?",
                choices=c("Select All",unique(crime$PRIMARY.DESCRIPTION)), multiple=T,
                selected = "Select All")
  })
  
  output$park_select <- renderUI({
    selectInput("park.select", "Zoom to a particular park?",
                choices=c("Select All",unique(parks_latlong$PARK)), multiple=F,
                selected="Select All")
  })
  

  
  output$map <- renderLeaflet({
    leaflet(crime) %>%
    setView(lat=41.8781, lng=-87.6298, zoom=10) %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addCircleMarkers(
      lat = ~LATITUDE,
      lng = ~LONGITUDE,
      clusterOptions = markerClusterOptions(),
      popup = ~PRIMARY.DESCRIPTION,
      stroke=F,
      fillColor = "red",
      fillOpacity = 0.7,
      radius=7
    ) %>%
    addPolygons(
      data=parks_geo,
      fillColor = "green",
      fillOpacity = .2,
      stroke=FALSE,
      popup = ~park) 
  })
  
  filter_crime <- reactive({

    if(c("Select All") %in% input$crime.select | is.null(input$crime.select)){
      crime
    }else{
      crime %>%
        filter(PRIMARY.DESCRIPTION %in% input$crime.select)
    }

  })
  
  filter_park <- reactive({

    if(c("Select All") %in% input$park.select | is.null(input$park.select)){
      c(41.8781, -87.6298)
    }else{
      park_selected <- parks_latlong %>%
        filter(PARK == input$park.select) %>%
        select(X_COORD, Y_COORD)

      c(park_selected$Y_COORD[1], park_selected$X_COORD[1])
    }

  })
  
  filter_park_zoom <- reactive({
    
    if(c("Select All") %in% input$park.select | is.null(input$park.select)){
      c(10)
    }else{
      c(15)
    }
    
  })

  observe({

    leafletProxy("map", data = filter_crime())  %>%
      clearMarkerClusters() %>%
      addCircleMarkers(
        lat = ~LATITUDE,
        lng = ~LONGITUDE,
        clusterOptions = markerClusterOptions(),
        popup = ~PRIMARY.DESCRIPTION,
        stroke=F,
        fillColor = "red",
        fillOpacity = 0.7,
        radius=7
        )
      

  })
  
  observe({
    
    view.latlong <- filter_park()
    
    leafletProxy("map")  %>%
      setView(lat = view.latlong[1], lng = view.latlong[2], zoom = filter_park_zoom())
    
  })

}