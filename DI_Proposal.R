############
# The Data Incubator Challenge
# Author: JR Mihaljevic
# Date: 18 July 2016
# Language: R
############

####################################
# Data Import and Cleaning
####################################

#-------------------------------------------------
#-------------------------------------------------
library(readr)
library(dplyr)
library(leaflet)
library(geojsonio)
library(plotly)

####################################
# Parks data set
####################################
parks_url <- "https://data.cityofchicago.org/api/geospatial/ej32-qgdr?method=export&format=GeoJSON"
parks_geo <- geojson_read(parks_url, method="local", what="sp")

parks_latlong <- read_csv("https://data.cityofchicago.org/api/views/eix4-gf83/rows.csv?accessType=DOWNLOAD")
parks_latlong <- select(parks_latlong, PARK, X_COORD, Y_COORD)
write_csv(parks_latlong, path="./shiny_app1/parks_latlong.csv")

####################################
# Crimes data set
####################################

crime <- read_csv(file="https://data.cityofchicago.org/api/views/x2n5-8w5q/rows.csv?accessType=DOWNLOAD")
crime <- data.frame(crime)
crime <- na.omit(crime)

crime <- crime %>%
  select(PRIMARY.DESCRIPTION, LATITUDE, LONGITUDE)

write_csv(crime, path="crimes.csv")

crime_sub <- na.omit(sample_n(crime, 1000))

crime_map <- leaflet(crime) %>%
  setView(lat=41.8781, lng=-87.6298, zoom=10) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addCircleMarkers(
    lat = ~LATITUDE,
    lng = ~LONGITUDE,
    clusterOptions = markerClusterOptions(),
    popup = ~PRIMARY.DESCRIPTION
  ) %>%
  addPolygons(
    data=parks_geo,
    fillColor = "green",
    fillOpacity = .2,
    stroke=FALSE,
    popup = ~park) 
crime_map

####################################
# Look at # parks in an area and # crimes
####################################

# Need to create a random number of locations in chicago:

lng_range <- c(-87.84, -87.52)
lat_range <- c(41.65, 42.02)

N_iter <- 1000;

lat_rand <- runif(N_iter, lat_range[1], lat_range[2])
lng_rand <- runif(N_iter, lng_range[1], lng_range[2])

# Create square buffer around each point:
buffer <- .02
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


plot(log(num_crimes) ~ num_parks)

df <- data.frame(num_parks, num_crimes)
df <- filter(df, num_parks > 0)

p <- ggplot(df, aes(x=num_parks, y=log(num_crimes)))+
  theme_classic()+
  geom_point(shape=19, color="red")+
  labs(x="Number of Parks", y="ln(Number of Crimes)")

gg <- ggplotly(p)
gg


test_map <- leaflet(df) %>%
  addTiles() %>%
  addCircles(
    lat = lat_rand,
    lng = lng_rand
  )
test_map