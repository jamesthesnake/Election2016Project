rm(list = ls())

library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(ggplot2)
server<-shinyServer(function(input, output){
  
  #IMPORT DATA
  output$text1<-renderText({ "research the 2016 election using cool tab"})

  #IMPORT ELECTION DATA 2016
  get2016<-reactive({
    data <- read.csv("nc_cty_votes_16.csv", header = T, sep = ",")
    data$FIPS_county <- sprintf("%03d", data$FIPS_county)
    data <- rename(data, COUNTYFP = FIPS_county)
    
    data$TrumpWin <- data$Trump > data$Clinton
    data$TrumpWinRatio <- data$Trump/data$Clinton
    data$nVotes <- data$Trump + data$Clinton
    data$TrumpPct <- data$Trump/data$nVotes * 100
    data$ClintonPct <- data$Clinton/data$nVotes * 100
    data$TrumpPctVictory <- data$TrumpPct - data$ClintonPct
    
    data$winner <- "Hillary"
    data$winner[data$TrumpWin==1] <- "Trump"
    data<-data
  })
  
  
  #IMPORT ELECTION DATA 2012
get2012<-reactive({
    data<-get2016()
    data12 <- read.csv(file = "US_elect_county.csv")
    data12 <- filter(data12, State.Postal == "NC")
    data12$FIPS <- sprintf("%03d", data12$FIPS - 37000)
    data12 <- data12[2:nrow(data12),]
    data12$RomneyPct <- as.numeric(data12$RomneyPct)
    data12$ObamaPct <- as.numeric(data12$ObamaPct)
    data12 <- rename(data12, COUNTYFP = FIPS)
    
    data12$RomneyWin <- data12$Romney > data12$Obama
    data12$RomneyPctVictory <- data12$RomneyPct - data12$ObamaPct 
    data12[,1:2] <- NULL
    data12<<-data12
    data <- merge(data,data12)
})
  st_fips <- read.csv("st_fips.csv")

  #IMPORT AND MERGE DRUG DATA
    drug <- read.csv(file = "cty_drug.csv", header = T, sep = ",")
    getDrugs<-reactive({
    data<-get2012()
    drug <- drug[drug$State == input$chooseStates & drug$Year == 2014,]
    names(drug)[8] <- "AdjDrugDeathRate"
    drug <- rename(drug, COUNTYFP = FIPS)
    drug$FIPS <- drug$COUNTYFP - 37000
    drug$FIPS <- sprintf("%03d", drug$COUNTYFP)
    drug <- select(drug, FIPS, AdjDrugDeathRate)
    data <- merge(data, drug)
    })

  
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
  data<-getDrugs()
  states<-getStates()
  data <- data[order(order(as.numeric(as.character(states$COUNTYFP )))),]
  ncounty <<- length(states$COUNTYFP)
  data$DDR <- mapply(drugDeathBin, data$AdjDrugDeathRate)
  
  m1 <- lm(TrumpPctVictory ~ DDR, data)
  data$m1.residuals <- resid(m1)
  
  m2 <- lm(TrumpPctVictory ~ RomneyPctVictory, data)
  data$m2.residuals <- resid(m2)
  
  data$rnorm <- rnorm(ncounty)
  m3 <- lm(TrumpPctVictory ~ rnorm, data)
  data$m3.residuals <- resid(m3)
  data<<-data
  })
  #MAKE SHAPES
  shape_file <- "cb_2015_us_county_20m/cb_2015_us_county_20m.shp"
  
  
  
  
  ##Winners
  output$myMapper<-renderLeaflet({
    data<-getData()
    states<-getStates()
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
  color <- colorBin("YlOrRd", data$DDR , bins = 8)(data$DDR)
  
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color
    )
  
  
  #Correlation of drug death rates & trump victory margin
  color <- colorBin("YlOrRd", data$m1.residuals^2 , bins = 8)(data$m1.residuals^2)
  
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color
    )
  
  #Correlation of romney victory margin & trump victory margin
  color <- colorBin("YlOrRd", data$m2.residuals^2 , bins = 5)(data$m2.residuals^2)
  
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color
    )

  ##Correlation w/ Random Number
  color <- colorBin("YlOrRd", data$m3.residuals^2 , bins = 5)(data$m3.residuals^2)
  
  leaflet(states) %>%
    addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                 weight = .5, fill = T, fillColor = ~color
    )
  
    color <- colorBin("YlOrRd", data$m3.residuals^2 , bins = 5)(data$m3.residuals^2)
    
    leaflet(states) %>%
      addPolygons( stroke = T, fillOpacity =.7, smoothFactor = 0, color = "black",
                   weight = .5, fill = T, fillColor = ~color
      )
    
  
  ##BOXPLOTS
  output$graphTwo<-renderPlot({
    
  bp <- ggplot(data = data, aes(x=data$DDR, y=TrumpPctVictory), order(as.numeric(data$DDR))) + geom_boxplot(aes(fill=AdjDrugDeathRate) ) 
  
  bp <- bp + xlab( "Age-Adjusted drug deaths per 100,000 people") +
    ylab("Trump Victory Margin")       
  bp <- bp + scale_fill_discrete(breaks=c("6.1-8","8.1-10","10.1-12","12.1-14","14.1-16","16.1-18","18.1-20",">20"))
  bp + ggtitle("Trump victory margin in North Carolina counties, by county drug overdose rate ")
  print(bp)
  })
  
  ##BAR GRAPH
  output$graphThree <-renderPlot({
    data$winner <- factor(data$winner)
    data$winner <- factor(data$winner, levels = rev(levels(data$winner)))
    bp2 <- ggplot(data, aes(DDR, fill = winner, order = as.numeric(DDR))) +
      geom_bar()
    bp2 <<- bp2 + xlab( "Age-Adjusted drug deaths per 100,000 people") +
      ylab("Number of Counties")       
    bp2 + ggtitle("2016 Election victor in North Carolina counties by county drug overdose rate")
  })
  ##REGRESSIONS
  getSummary<-renderText({
  summary(lm(TrumpPctVictory ~ RomneyPctVictory + DDR, data)) 
 # summary(glm(TrumpWin ~ RomneyWin + DDR,data,family="DDRomial"))  
  cor(data$TrumpPctVictory, data$DDR)
  summary(lm(TrumpPctVictory ~ DDR, data[data$RomneyWin == F,])) ##effect of drug death on obama counties
})
})
  