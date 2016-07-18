############
# The Data Incubator Challenge
# Author: JR Mihaljevic
# Date: 18 July 2016
# Language: R
############

####################################
# Shiny App 2
####################################

library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(geojsonio)
library(plotly)
#-------------------------------------------------
#-------------------------------------------------



shinyUI(fluidPage(
  titlePanel("Chicago Parks and Crime"),
  
  fluidRow(
    column(3,
           
           sliderInput("N_iter", "Number of random coordinates",
                       min=10, max=1000, value=50, step=5),
           
           sliderInput("buffer", "How much buffer around each point?",
                       min=.001, max=.1, value=.05, step=.0005)
          
           
           
    ),
    
    column(9,
           plotlyOutput("plot"))
    
  )
))