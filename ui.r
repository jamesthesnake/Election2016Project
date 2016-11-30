
library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(ggplot2)
ui <-shinyUI(pageWithSidebar(
  headerPanel(h1(a(img(src = "logo.jpg", align = "left", width = "100%"), href = "http://www.politico.com/"), "Election 2016")),
  sidebarPanel(
    actionButton("goButton", "Run Statistics"),
    selectInput("chooseStates","Choose state to view", selected = "North Carolina", c("Alabama","Alaska","Arkansas","Arizona","Connecticut","Colorado","California","Delaware","DC", "Florida","Georgia","Hawaii","Idaho","Illinois", "Indiana","Iowa", "Kansas","Kentucky","Lousianna","Maine","Maryland", "Massachusets", "Michigan", "Minnesota", "Mississippi","Missouri","Montanna","Neberaska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio","Oklahoma","Oregon","Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin",
                                                                                      "Wyoming")),
    selectInput("whatData","Choose the data for state",c("drugs","2016Results")),
    downloadButton("downloadMap","download Map")
    ),mainPanel(

tabsetPanel(

  tabPanel("Instructions", textOutput("text1") ,textOutput("text2")),
  tabPanel("graph", plotOutput("graphTwo")),
  tabPanel("Heatmap", leafletOutput("myMapper")),
  tabPanel("custom heatmap", leafletOutput("drugMap")),
  tabPanel("the end",plotOutput("graphThree"))
)

)


))
