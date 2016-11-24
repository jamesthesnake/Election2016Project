
library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(ggplot2)
ui <-shinyUI(pageWithSidebar(
  headerPanel("Election 2016!"),
  sidebarPanel(
    actionButton("goButton", "Run Statistics"),
    selectInput("chooseStates","Choose state to view", selected = "North Carolina", c("North Carolina","Florida","Ohio","Iowa","Colorado","Arizona","Virgina","Michigan","Nevada","Pennslyvania","Wisconsin","Connecticut","Minnesota"))
),mainPanel(

tabsetPanel(

  tabPanel("Instructions", textOutput("text1") ),
  tabPanel("graph", plotOutput("graphTwo")),
  tabPanel("Heatmap", leafletOutput("myMapper")),
  tabPanel("the end",plotOutput("graphThree"))
)

)


))
