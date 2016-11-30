rm(list = ls())

library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(ggplot2)
library(webshot)
server<-shinyServer(function(input, output){
  
  #IMPORT DATA
  output$text1<-renderText({ "research the 2016 election using custom metrics and color schemes"})
  output$text2<-renderText({ "for the customt tab , select your area for data and see the heatmap on custom sdie wrok"})
  
  #IMPORT ELECTION DATA 2016
 

  
  #IMPORT ELECTION DATA 2012

  st_fips <- read.csv("st_fips.csv")

  #IMPORT AND MERGE DRUG DATA
   

  
  drugDeathBin <- function(x){
    if(x == "0-2") return(1)
    if(x == "2.1-4") return(3)
    if(x == "4.1-6") return(5)
    if(x == "6.1-8") return(7)
    if(x == "8.1-10") return(9)
    if(x == "10.1-12") return(11)
    if(x == "12.1-14") return(13)
    if(x == "14.1-16") return(15)
    if(x == "16.1-18") return(17)
    if(x == "18.1-20") return(19)
    if(x == ">20") return(21)
  }
  
  getStates<-reactive({
  number<-st_fips[st_fips$State== input$chooseStates,]$FIPS
    
  states <- readOGR(dsn="cb_2015_us_county_20m",layer="cb_2015_us_county_20m")
  states <- states[states$STATEFP == number,]
  })
  getData<-reactive({
  data <- read.csv("data.csv", header = T, sep = ",")
  
  states<-getStates()
  data <- data[order(order(as.numeric(as.character(states$COUNTYFP )))),]
  ncounty <- length(states$COUNTYFP)

  m1 <- lm(gop_margin_2016 ~ DrugDeathRate, data)
  data$m1.residuals <- resid(m1)
  
  m2 <- lm(gop_margin_2016 ~ gop_margin_2012, data)
  data$m2.residuals <- resid(m2)
  
  data$rnorm <- rnorm(ncounty)
  m3 <- lm(gop_margin_2016 ~ rnorm, data)
  data$m3.residuals <- resid(m3)
  data$winner <- "Hillary"
  data$winner[data$TrumpWin==1] <- "Trump"
  data<-data
  })
  #MAKE SHAPES
  shape_file <- "cb_2015_us_county_20m/cb_2015_us_county_20m.shp"
  
  
  
  
  ##Winners
  output$myMapper<-renderLeaflet({
    data<-getData()
    states<-getStates()
    
    data <- data[order(order(as.numeric(as.character(states$COUNTYFP )))),]
    ncounty <- length(states$COUNTYFP)
  color <- rep('blue',ncounty)
  color[data$TrumpWin == 1]<- 'red'
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = color
    )
  })

   # color <- rep('blue',ncounty)
    #color[data$TrumpWin == 1]<- 'red'
    
    #leaflet(states) %>%
    #addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
     #            weight = .5, fill = T, fillColor = color
    #)
    
  

  
  ##Drug death rates
  output$drugMap<-renderLeaflet({
    data<-getData()
    states<-getStates()
    data <- data[order(order(as.numeric(as.character(states$COUNTYFP )))),]
    ncounty <- length(states$COUNTYFP)
    if(input$whatData=="drugs"){
      
  color <- colorBin("YlOrRd", data$DrugDeathRate , bins = 8)(data$DrugDeathRate)
    }
    else if(input$whatData=="2016Results"){
      color <- rep('blue',ncounty)
      color[data$TrumpWin == 1]<- 'red'
    }
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color)
    
  })


  getMap<-function()({
    data<-getData()
    states<-getStates()
    data <- data[order(order(as.numeric(as.character(states$COUNTYFP )))),]
    ncounty <- length(states$COUNTYFP)
    color <- colorBin("YlOrRd", data$DrugDeathRate , bins = 8)(data$DrugDeathRate)
   
    leaflet(states) %>%
      addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                   weight = .5, fill = T, fillColor = color
      )
    
    
  })
  
  #Correlation of drug death rates & trump victory margin
  output$drugsTrump<-renderLeaflet({
    data<-getData()
    states<-getStates()
    
    data <- data[order(order(as.numeric(as.character(states$COUNTYFP )))),]
    ncounty <- length(states$COUNTYFP)
  color <- colorBin("YlOrRd", data$m1.residuals^2 , bins = 8)(data$m1.residuals^2)
  
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color
    )
  })
  output$mapRomney<-renderLeaflet({
  
  #Correlation of romney victory margin & trump victory margin
  color <- colorBin("YlOrRd", data$m2.residuals^2 , bins = 5)(data$m2.residuals^2)
  
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color
    )
})
  output$mapRandom<-renderLeaflet({
    
  ##Correlation w/ Random Number
  color <- colorBin("YlOrRd", data$m3.residuals^2 , bins = 5)(data$m3.residuals^2)
  
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color
    )
  })
  output$mapRandomer<-renderLeaflet({
    color <- colorBin("YlOrRd", data$m3.residuals^2 , bins = 5)(data$m3.residuals^2)
    
    leaflet(states) %>%
      addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                   weight = .5, fill = T, fillColor = ~color
      )
  })
  
  ##BOXPLOTS
  output$graphTwo<-renderPlot({
    
  bp <- ggplot(data = data, aes(x=data$DrugDeathRate, y=gop_margin_2016), order(as.numeric(data$DrugDeathRate))) + geom_boxplot(aes(fill=DrugDeathRate) ) 
  
  bp <- bp + xlab( "Age-Adjusted drug deaths per 100,000 people") +
    ylab("Trump Victory Margin")       
  bp <- bp + scale_fill_discrete(breaks=c("6.1-8","8.1-10","10.1-12","12.1-14","14.1-16","16.1-18","18.1-20",">20"))
  bp + ggtitle("Trump victory margin in North Carolina counties, by county drug overdose rate ")
  print(bp)
  })
  
  ##BAR GRAPH
  output$graphThree <-renderPlot({
   # data$winner16 <- factor(data$winner16)
    #data$winner16 <- factor(data$winner16, levels = rev(levels(data$winner16)))
    data<-getData()
    bp2 <- ggplot(data, aes(DrugDeathRateCategory, fill = winner, order = as.numeric(DrugDeathRateCategory))) +
      geom_bar()
    bp2 <- bp2 + xlab( "Age-Adjusted drug deaths per 100,000 people") +
      ylab("Number of Counties")       
    bp2 + ggtitle("2016 Election victor in State counties by county drug overdose rate")
  })
  ##REGRESSIONS
  getSummary<-renderText({
  summary(lm(TrumpPctVictory ~ RomneyPctVictory + DDR, data)) 
 # summary(glm(TrumpWin ~ RomneyWin + DDR,data,family="DDRomial"))  
  cor(data$TrumpPctVictory, data$DDR)
  summary(lm(TrumpPctVictory ~ DDR, data[data$RomneyWin == F,])) ##effect of drug death on obama counties
})
  output$downloadMap <- downloadHandler(
    filename = function() { paste(input$chooseStates, '.png', sep='') },
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
     # owd <- setwd(tempdir())
      #on.exit(setwd(owd))
      
      saveWidget(getMap(), "temp.html", selfcontained = FALSE)
      webshot("temp.html", file = file, cliprect = "viewport")
    }
  )
    
})
  