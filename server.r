rm(list = ls())
library(rmarkdown)
library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(ggplot2)
library(webshot)
library(mapview)
server<-shinyServer(function(input, output){
  
  #IMPORT DATA
  output$text1<-renderText({ "research the 2016 election using custom metrics and color schemes"})
  output$text2<-renderText({ "for the customt tab , select your area for data and see the heatmap on custom sdie wrok"})
  output$text3<-renderText({ "labels will be added in the label heatmap"})
  output$text4<-renderText({ "By James Hennessy and Benjamin Berger"})

  
  
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
  getCongress<-reactive({
    cong<-readOGR(dsn="cb_2014_us_cd114_20m",layer="cb_2014_us_cd114_20m")
    
    
  })
  getStates<-reactive({
  states <- readOGR(dsn="cb_2015_us_county_20m",layer="cb_2015_us_county_20m")
  states<-states[states$STATEFP!=72,]
  states<-states[as.character(states$STATEFP)!="02",]
  states<-states[states$NAME!="Kalawao",]
  states$FULLFP<-paste0(states$STATEFP,states$COUNTYFP)
  
    if(input$individualState){
    number<-st_fips[st_fips$State== input$chooseStates,]$FIPS
    if(number<10){
      number<-as.character(number)
      number<-paste0("0",number)
      states <- states[states$STATEFP == number,]
      
    }
    else{
    states <- states[states$STATEFP == number,]
    }
    }
  states<-states
  })
  getData<-reactive({
  data <- read.csv("data.csv", header = T, sep = ",")
  states<-getStates()

  if(input$individualState){
    
    num<-as.numeric(as.character(states$STATEFP))
    if(num<10){
      data<- data[data$StateFIPS==num,]
    }
    else{
    data <- data[data$StateFIPS==states$STATEFP,]
    }
    data<-data
  }
  ncounty <- length(states$COUNTYFP)
  m1 <- lm(gop_margin_2016 ~ DrugDeathRate, data)
  data$m1.residuals <- resid(m1)
  
  m2 <- lm(gop_margin_2016 ~ gop_margin_2012, data)
  data$m2.residuals <- resid(m2)
  data$rnorm <- rnorm(ncounty)
  m3 <- lm(gop_margin_2016 ~ rnorm, data)
  data$m3.residuals <- resid(m3)
  m4<-lm(gop_margin_2016~BlackShare,data)
  data$m4.residuals<- resid(m4)
  data$winner <- "Hillary"
  data$winner[data$TrumpWin==1] <- "Trump"
  
  data<-data
  
  })

  output$CongMap<- renderLeaflet({
    cong<-getCongress()
    data<-getData()
    data <- data[order(order(as.numeric(as.character(cong$GEOID)))),]
    color <- colorBin(input$chooseColor, data$m4.residuals^2 , bins = 5)(data$m4.residuals^2)
    leaflet(cong) %>%
        addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                     weight = .5, fill = T, fillColor = ~color)
    
  })

  
  ##Drug death rates
  output$drugMap<-renderLeaflet({
    finalMap<-finalMap()
  })
  finalMap<-reactive({
    data<-getData()
    
    states<-getStates()
   
    data <- data[order(order(as.numeric(as.character(states$GEOID)))),]
    if(input$individualState){
      
      num<-as.numeric(as.character(states$STATEFP))
      if(num<10){
        data<- data[data$StateFIPS==num,]
      }
      else{
        data <- data[data$StateFIPS==states$STATEFP,]
      }
      data<-data
    }
    ncounty <- length(states$COUNTYFP)

    if(input$whatData=="drugs"){
  color <- colorBin(input$chooseColor, data$DrugDeathRate , bins = 8)(data$DrugDeathRate)
    }
    else if(input$whatData=="2016Results"){
      color <- rep('blue',ncounty)
      color[data$TrumpWin == 1]<- 'red'
    }
    else if (input$whatData == "RomTrump"){
      color <- colorBin(input$chooseColor, data$m2.residuals^2 , bins = 5)(data$m2.residuals^2)
      
    }
    else if(input$whatData=="blackPop"){
      color <- colorBin(input$chooseColor, data$BlackShare , bins = 8)(data$BlackShare)
      
      
    }
    if(input$whatData=="2016Results"){
      color <- rep('blue',ncounty)
      color[data$TrumpWin == 1]<- 'red'
    }  
  map<-{
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color)
  }
  if(input$labelYes){
    longLat<-read.csv("us_cty_area.csv")
    abbStates<-read.csv("states.csv")
    if(input$individualState){
      intial<-abbStates$Abbreviation[which(abbStates$State==input$chooseStates)]
      
      long<-data$long[data$StateCode==toString(intial)]
      lat<-data$lat[data$StateCode==toString(intial)]      
    }
    else{

      long<-data$long
      lat<-data$lat
    }
    map<-map%>%
      addLabelOnlyMarkers(~lat, ~long, label =  ~as.character(states$NAME), 
                          labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
  }
  if(input$legendYes){
    if(input$whatData=="2016Results"){
      pal<- colorFactor(c("blue","red"),
      domain= c("Hillary","Trump"))
      value<-c("Hillary","Trump")
    }
    else{
    pal <- colorNumeric(
    
      palette   = input$chooseColor,
      domain = data$DrugDeathRate
    )
    value<-data$DrugDeathRate
    }
    value<-value
    pal<-pal
    map<-map%>%
      addLegend("bottomright", pal = pal, values = value,
                title = input$whatData,
                opacity = 1
      )
    
  }
  else{
    map<-map
  }
  map<-map
  })


  getMap<-function()({
    data<-getData()
    states<-getStates()
    if(input$individualState){
      # data<-data[data$StateFIPS==states$STATEFP,]
    }
    ncounty <- length(states$COUNTYFP)
    
    color <- colorBin(input$chooseColor, data$DrugDeathRate , bins = 8)(data$DrugDeathRate)
    
    leaflet(states) %>%
      addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                   weight = .5, fill = T, fillColor = ~color
      )
    

    
    
  })
  
  #Correlation of drug death rates & trump victory margin
  

  output$genMap<-renderLeaflet({
    map<-getGenMap()
    states<-getStates()
    data<-getData()
    if(input$individualState){
     # data<-data[data$StateFIPS==states$STATEFP,]
    }
    abbStates<-read.csv("states.csv")
    if(input$individualState){
    intial<-abbStates$Abbreviation[which(abbStates$State==input$chooseStates)]
    
    long<-data$long[data$StateCode==toString(intial)]
    lat<-data$lat[data$StateCode==toString(intial)]    
    }
    else{
      
      long<-data$long
      lat<-data$lat
    }
 
    map%>%
          addLabelOnlyMarkers(~lat, ~long, label =  ~as.character(states$NAME), 
                                                       labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
    
    
  })
  getGenMap<-reactive({
    data<-getData()
    states<-getStates()
    if(input$individualState){
     # data<-data[data$StateFIPS==states$STATEFP,]
    }
    data <- data[order(order(as.numeric(as.character(states$GEOID )))),]
    ncounty <- length(states$COUNTYFP)
  ##Correlation w/ Random Number
  color <- colorBin(input$chooseColor, data$m3.residuals^2 , bins = 5)(data$m3.residuals^2)
  
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color
    )
  })
  output$mapRandomer<-renderLeaflet({
    color <- colorBin(input$chooseColor, data$m3.residuals^2 , bins = 5)(data$m3.residuals^2)
    
    leaflet(states) %>%
      addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                   weight = .5, fill = T, fillColor = ~color
      )
  })
  
  ##BOXPLOTS
  output$graphTwo<-renderPlot({
  data<-getData()
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
  saveFile<-function(){
    here<-finalMap()
    saveWidget(here, file="temp.html", selfcontained = T) 
    webshot("temp.html", file = "Rplot.png",
            cliprect = "viewport")
  }
  observeEvent(input$saveButton,{
    saveFile()
    
    
  })
  require(webshot)
  output$downloadMap <- downloadHandler(
    filename = 'temp.html',
    
    content = function(file) {
#      src <- normalizePath('report.Rmd')
      
      here<-finalMap()
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(here, file="temp.html", selfcontained = T) 
      
      webshot("temp.html", file = "Rplot.png",
             cliprect = "viewport")
      #file.copy(src, 'Rplot.png', overwrite = TRUE)
      
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      file.copy("temp.html",file,overwrite = TRUE)
    }
    )
  output$downloadMapTwo <- downloadHandler(
    filename = 'Rplot.png',
    
    content = function(file) {
      #      src <- normalizePath('report.Rmd')
      
      here<-finalMap()
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(here, file="temp.html", selfcontained = T) 
      
      webshot("temp.html", file = "Rplot.png",
              cliprect = "viewport")
      #file.copy(src, 'Rplot.png', overwrite = TRUE)
      
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      file.copy("Rplot.png",file,overwrite = TRUE)
    }
  )
  output$downloadData <- downloadHandler({
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$chooseStates, ".csv", sep = ".")
    }
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      sep <- ","
      
      # Write to a file specified by the 'file' argument
      write.table("data.csv", file, sep = sep,
                  row.names = FALSE)
    }
    
  })
})
  