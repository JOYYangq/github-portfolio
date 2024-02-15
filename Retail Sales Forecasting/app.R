#########################################################
#  Economic forecasting and analysis
#  Qiaochu Yang
#  R Shiny Web Forecasting App Project
##########################################################
#1. import library
library(shiny)
library(fpp2)
library(ggplot2)
library(seasonal)
library(quantmod)
library(shinythemes)
library(urca)
##2. clear memory
rm(list=ls())

#3. extract data from FRED

symbols <- c( "MRTSSM4453USN","MRTSSM442USN","MRTSSM448USN" )
m = length(symbols)

getSymbols(symbols,src="FRED")


# 4.Define UI 
ui <- fluidPage(theme = shinytheme("journal"),pageWithSidebar(
  # Application title
  headerPanel("Retail Sales Forecasting for Liquor, Furniture and Clothing stores"),
  
  # Sidebar with controls to select the dataset and forecast ahead duration
  sidebarPanel(
    # Select variable
    h6(selectInput("variable", "Variable:",
                list("Liquor Store Sales" = "Liquor Store Sales", 
                     "Furniture Store Sales" = "Furniture Store Sales",
                     "Clothing Store Sales" = "Clothing Store Sales"))),
    h6(textOutput("text1")),
    br(),
    h6(numericInput("ahead", "Month to Forecast Ahead:", 12)),
    h6(numericInput("start", "Starting year:", 2010)),
    checkboxInput("cleand","clean data", FALSE),
    submitButton("Update View"),
    br(),
    h6(p("ECON 6210 R Shiny Web Forecasting App Project")),
    h6(p("App Created by: Qiaochu Yang")),
    br(),
    
    img(src='logo.png', align = "left"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  ),
  
  
  
  # Show the caption and forecast plots
  mainPanel(
    h3(textOutput("caption")),
    
    tabsetPanel(
      tabPanel("Timeseries plot", plotOutput("tsPlot")),
      tabPanel("ACF plot", plotOutput("acfPlot")),
      tabPanel("Timeseries Decomposition", plotOutput("dcompPlot")),
      tabPanel("Arima Forecast", plotOutput("arimaForecastPlot"), verbatimTextOutput("arimaForecastTable")),
      tabPanel("Exponential Smoothing (ETS) Forecast", plotOutput("etsForecastPlot"), verbatimTextOutput("etsForecastTable")),
      tabPanel("TBATS Forecast", plotOutput("tbatsForecastPlot"), verbatimTextOutput("tbatsForecastTable")),
      tabPanel("Holt winters Forecast", plotOutput("holtswForecastPlot"), verbatimTextOutput("holtswForecastTable")),
      tabPanel("STL Forecast", plotOutput("stlForecastPlot"), verbatimTextOutput("stlForecastTable")),
      tabPanel("Average (arima, ets, TBATS, Holt Winters, STL) forecasts", verbatimTextOutput("averageForecastTable"))
    )
  )
))


server <- (function(input, output) {
  
   getDataset <- reactive({
     if (input$variable=="Liquor Store Sales")
     {
       return(MRTSSM4453USN)
     }
     else if (input$variable=="Furniture Store Sales")
     {
       return(MRTSSM442USN)
     }
     else
     {
       return(MRTSSM448USN)
     }
   })
  
   output$caption <- renderText({
     paste("Dataset: ", input$variable)
   })
  
   output$tsPlot <- renderPlot({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     autoplot(y,size = 1, colour = "blue") + 
       ylab(input$variable) + xlab("Year") +
       theme(plot.title  = element_text(size=18, face = "bold") ,
             axis.text.x = element_text(size=18, face = "bold"),
             axis.text.y = element_text(size=18, face = "bold"),
             axis.title  = element_text(size=18, face = "italic")) 
   })
  
   output$acfPlot <- renderPlot({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     ggAcf(y)
   })
  
   output$dcompPlot <- renderPlot({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     f <- decompose(y)
     plot(f)
   })
  

   # Arima 
   
   
   
   output$arimaForecastPlot <- renderPlot({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     fit <- auto.arima(y)
     plot(forecast(fit, h=input$ahead))
   })
  
   output$arimaForecastTable <- renderPrint({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
      
      # clean data
      if(isTRUE(input$cleand)){
        y <- tsclean(y)
      }
      # If clean is FALSE
      if(!isTRUE(input$cleand)){
        y <- y
      }
      
      fit <- auto.arima(y)
      forecast(fit, h=input$ahead)
   })
  
   output$etsForecastPlot <- renderPlot({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     fit <- ets(y)
     plot(forecast(fit, h=input$ahead))
   })
  
   output$etsForecastTable <- renderPrint({
      y <- getDataset()
      date.start = input$start
      y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
      
      # clean data
      if(isTRUE(input$cleand)){
        y <- tsclean(y)
      }
      # If clean is FALSE
      if(!isTRUE(input$cleand)){
        y <- y
      }
      
      
      fit <- ets(y)
      forecast(fit, h=input$ahead)
   })
  
   output$tbatsForecastPlot <- renderPlot({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     fit <- tbats(y)
     plot(forecast(fit, h=input$ahead))
   })
  
   output$tbatsForecastTable <- renderPrint({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     fit <- tbats(y)
     forecast(fit, h=input$ahead)
    
   })
  
   output$holtswForecastPlot <- renderPlot({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     fit <- hw(y)
     plot(forecast(fit, h=input$ahead))
   })
  
   output$holtswForecastTable <- renderPrint({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     fit <- hw(y)
     forecast(fit, h=input$ahead)
    
   })
  
   
   #STL PAGE
   output$stlForecastPlot <- renderPlot({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     fit <- stlf(y)
     plot(forecast(fit, h=input$ahead))
   })
  
   output$stlForecastTable <- renderPrint({
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
     
     fit <- stlf(y)
     forecast(fit, h=input$ahead)
    
   })
  
   #Average Page
   output$averageForecastTable <- renderPrint({
    
     y <- getDataset()
     date.start = input$start
     y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12)
     
     # clean data
     if(isTRUE(input$cleand)){
       y <- tsclean(y)
     }
     # If clean is FALSE
     if(!isTRUE(input$cleand)){
       y <- y
     }
    
     fit1 = auto.arima(y)
     fc1 = forecast(fit1, h=input$ahead)
    
     fit2 <- ets(y)
     fc2 = forecast(fit2, h=input$ahead)
    
     fit3 = tbats(y)
     fc3= forecast(fit3, h=input$ahead)
    
     fit4 = hw(y)
     fc4 = forecast(fit4, h=input$ahead)
    
     fit5 = stlf(y)
     fc5 = forecast(fit5, h=input$ahead)
    
     fc = (fc1$mean + fc2$mean + fc3$mean + fc4$mean + fc5$mean)/5
     fc
   })
   
   output$text1 <- renderText({
  #switch for different variables  
     switch(input$variable,
            "Liquor Store Sales" = "Retail Sales: Beer, Wine, and Liquor Stores", 
            "Furniture Store Sales" = "Retail Sales: Furniture and Home Furnishings Stores",
            "Clothing Store Sales" = "Retail Sales: Clothing and Clothing Accessory Stores")
   })
  
})

# Run the application 
shinyApp(ui = ui, server = server)
