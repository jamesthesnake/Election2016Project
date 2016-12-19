
library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(ggplot2)
ui <-shinyUI(pageWithSidebar(
  headerPanel(h1( a( "Election 2016",style = "font-family: 'Cyborg', cursive; font-weight: 500; line-height: 1.1; color: #FF0000;")),
  sidebarPanel()),
  sidebarPanel(
    
    selectInput("chooseColor", "Choose Color Scheme:", c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys", "Greens", "GnBu", "BuPu", "BuGn", "Blues")),
    checkboxInput("individualState","See Individual state"),
    
    selectInput("chooseStates","Choose state to view", selected = "North Carolina", c("Alabama","Alaska","Arkansas","Arizona","Connecticut","Colorado","California","Delaware","DC", "Florida","Georgia","Hawaii","Idaho","Illinois", "Indiana","Iowa", 
                                                                                      "Kansas","Kentucky","Louisiana","Maine","Maryland", "Massachusets", "Michigan", "Minnesota", "Mississippi","Missouri","Montanna","Neberaska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio","Oklahoma","Oregon","Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin",
                                                                                      "Wyoming")),
    selectInput("whatData","Choose the data for state",c("drugs","2016Results","RomTrump","blackPop","PopDensity")),
    selectInput("whatFormat","Chooseformat to download map", c(".png",".pdf",".html")),
    checkboxInput("labelYes", "put Labels on the map"),
    checkboxInput("legendYes", "put a Legend on the map"),
    downloadButton("downloadMap","download Map"),
    downloadButton("downloadOutput","download csv")
    ),mainPanel(

tabsetPanel(

  tabPanel("Instructions", textOutput("text1") ,textOutput("text2"),textOutput("text3"),textOutput("text4")),
  tabPanel("Graph", plotOutput("graphTwo")),
#  tabPanel(" Label Heatmap", leafletOutput("genMap")),
  tabPanel("State Analysis", leafletOutput("drugMap")),
  tabPanel("Congressional Analysis", leafletOutput("CongMap")),
  tabPanel("Plot with Drug Rates",plotOutput("graphThree")),
  tabPanel("About Authors",textOutput("text6"),h1(a(img(src = "ben.jpg", align = "left", width = "50%",length="50%"))),textOutput("text7"))
)

)


))
