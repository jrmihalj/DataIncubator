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


shinyUI(fluidPage(
  titlePanel("Chicago Parks and Crime"),
  
  fluidRow(
    column(3,
           h3("What would you like to see?"),
           
           
           uiOutput("crime_select"),
           uiOutput("park_select")
           
           
    ),
    
    column(9,
           leafletOutput("map"))
    
  )
))