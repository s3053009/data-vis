# install.packages("shiny")

library(shiny)
library(ggmap)
library(ggplot2)
library(tidyverse)

# setwd("C:/Users/ttan/Documents/COSC2644")
df <- read.csv("data/GlobalPowerPlantDatabaseWithContinents.csv", header = T, stringsAsFactors = F)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Data Visualisation"),
  img(src = "dataVisualization.jpg", height = 140, width = 260),
  fluidRow(
    column(3,
          selectInput("select", h3("Select plot"), 
                       choices = list("Plot 1 - Country Power Generations by Fuel Type [Jordan]" = 1,
                                      "Plot 2 - Power Stations by Continent [David]" = 2,
                                      "Plot 3 - Fuel Type by Estimated Power Generation [Tim]" = 3), selected = 1)),
          tags$head(tags$style(HTML(".selectize-input {height: 20px; width: 500px; font-size: 14px;}")))
  ),

  plotOutput(outputId = "distPlot")
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
      if(input$select == 1){
        
        ggplot(data = df, mapping = aes(x = df$primary_fuel, y = df$estimated_generation_gwh)) +
          geom_boxplot(alpha = 0) +
          geom_jitter(alpha = 0.3, color = "tomato")
    }
    else if(input$select == 2){
      
      ggplot(data=df, aes(x=Continent_Name)) +
        geom_bar(fill="steelblue") +
        theme_minimal() + ggtitle("Power Stations by Continent") +
        xlab("\n Continent") + ylab("Number of Power Stations \n") +
        theme(plot.title = element_text(hjust = 0.5))
    }
    else if(input$select == 3){
      
      ggplot(data = df, aes(x=df$primary_fuel, y=df$capacity_mw)) +
        geom_bar(stat="identity", fill="#008080", alpha = 0.6) +
        xlab("Primary fuel") +
        ylab("Capacity (MW)") +
        scale_y_continuous(labels = function(l) {l = l / 1000000; paste0(l, "M")}) +
        stat_summary(aes(label = round(..y..)), fun.y = 'sum', geom = 'text', col = 'black', vjust = 1.0, size = 3) +
        theme_minimal()
    }
    
    
    
  })
  
}

shinyApp(ui = ui, server = server)
