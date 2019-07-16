library(shiny)
library(ggplot2)
library(mdsr)
library(ggthemes)
library(scales)
library(date)
library(lubridate)
library(tidyverse)
?hr
?shiny
#1) User Interface Object
ui <- fluidPage(
  #App title
  titlePanel("Dow Price Comparison "),
  #Sidebar layout with input and output definitions
  sidebarLayout(
      sidebarPanel(
       selectInput(inputId = "ticker1", label = "Ticker 1", choices = dowIndex$symbol,  tableOutput("")),
       selectInput(inputId = "ticker2", label = "Ticker 2", choices = dowIndex$symbol,  tableOutput("")),
       selectInput(inputId = "metric", label = "Select a Metric to Compare",  choices = names(dowTotal), tableOutput("")),
       sliderInput(inputId = "year", label = "Years to view", 2010, 2016, value = c(2010, 2016), sep = ""),
       hr(),
       helpText("Select a stock to plot its price over time")),
      mainPanel(
        plotOutput(width = "100%", height = "400px", "stockPlot"),
        plotOutput(width = "100%", height = "400px", "fundamentalsPlot")
      )
  )
)
#2) A server function

server <- function(input, output) {
  output$stockPlot <- renderPlot({
    dowIndividual <- dowIndex %>% filter(symbol %in% c(input$ticker1, input$ticker2)) %>% filter(year >= input$year[1], year <= input$year[2])
    dowIndividual$date <- as.POSIXct(gsub("-", "/", dowIndividual$date))
    ggplot(data = dowIndividual, aes(x = date, y = dailyPrice, color = symbol)) + geom_point()  + geom_smooth() +
        theme_economist() + scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("3 months")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Date") + ylab("Price")
  })
  output$fundamentalsPlot <- renderPlot({
    #metric <- gsub(" ", ".", input$metric)
    dataInput <- reactive({dowTotal[, input$metric]})
    dowTotal <- dowTotal %>% filter(Ticker.Symbol %in% c(input$ticker1, input$ticker2)) 
    ggplot(data = dowTotal, aes(x = For.Year, y = dataInput(), fill = Ticker.Symbol)) +
      geom_bar(stat = "identity", position = position_dodge()) + ylab("Selected Metric")
  })
}
shinyApp(ui = ui, server = server)

