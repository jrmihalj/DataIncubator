############
# The Data Incubator Challenge
# Author: JR Mihaljevic
# Date: 18 July 2016
# Language: R
############

####################################
# Shiny App 2
####################################

#-------------------------------------------------
#-------------------------------------------------

library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(geojsonio)
library(plotly)
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


####################################
# The Server
####################################

server <- function(input, output, session){
  
  
  lng_range <- c(-87.84, -87.52)
  lat_range <- c(41.65, 42.02)
  
  data_func <- reactive({
    
    N_iter <- input$N_iter
    
    lat_rand <- runif(N_iter, lat_range[1], lat_range[2])
    lng_rand <- runif(N_iter, lng_range[1], lng_range[2])
    
    # Create square buffer around each point:
    buffer <- input$buffer
    
    lng_up <- lng_rand + buffer
    lng_down <- lng_rand - buffer
    lat_right <- lat_rand + buffer
    lat_left <- lat_rand - buffer
    
    # Determine which parks reside in each buffer
    # Determine which crimes in each buffer
    
    num_parks <- NULL
    num_crimes <- NULL
    for(i in 1:N_iter){
      
      num_parks[i] <- 
        nrow(parks_latlong %>%
               filter(Y_COORD > lat_left[i], Y_COORD < lat_right[i]) %>%
               filter(X_COORD > lng_down[i], X_COORD < lng_up[i]) %>%
               distinct(PARK))
      
      num_crimes[i] <- 
        nrow(crime %>% 
               filter(LATITUDE > lat_left[i], LATITUDE < lat_right[i]) %>%
               filter(LONGITUDE > lng_down[i], LONGITUDE < lng_up[i])) 
      
    }
    
    df <- data.frame(num_parks, num_crimes)
    
    filter(df, num_parks > 0)
    
  })
  
  
  
  output$plot <- renderPlotly({
    
    p <- ggplot(data_func(), aes(x=num_parks, y=log(num_crimes)))+
      theme_classic()+
      geom_point(shape=19, color="red")+
      labs(x="Number of Parks", y="ln(Number of Crimes)")
    
    gg <- ggplotly(p)
    gg
    
  })
  
  
  
  
  
}