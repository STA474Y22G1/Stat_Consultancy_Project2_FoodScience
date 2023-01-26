## app.R ##

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)

oilData <- read_csv("Tidy Data.csv")

#oilData$`Palm olein concentration(C)` <- as.factor(oilData$`Palm olein concentration(C)`)
oilData$`Replicate No` <- as.factor(oilData$`Replicate No`)
oilData <- rename(oilData, Concentration = `Palm olein concentration(C)`, 
                  Replicate = `Replicate No`)


ui <- dashboardPage( 
  dashboardHeader(title= h3("Spectrum Viz")),
  dashboardSidebar(
    selectInput("Series", label = h4("Select Series"), 
                choices = unique(oilData$Series)),
    selectInput("Concentration", label = h4("Select Palm Olein Concentration"), 
                choices = sort(unique(oilData$Concentration))),
    selectInput("Replicate", label = h4("Select Replicate"), 
                choices = unique(oilData$Replicate))
  ),
  dashboardBody(
    fluidRow(
           box(plotlyOutput("plot", height = 570), width = 12)
    )
    
  )
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    plot <- oilData %>% filter(Series == input$Series) %>% 
      filter(Concentration == input$Concentration) %>%
      filter(Replicate == input$Replicate) %>%
      ggplot(aes(x = `Wave Number (cm-1)(W)`, y = `Absorption (A)`)) + 
      geom_line() + 
      theme_bw() +
      labs(x = "Wave Number (cm-1)", y = "Absorption", title = paste("Spectrum of", input$Series, 
                                                                     "For Replicate", input$Replicate))
    ggplotly(plot)
    
  })
}


shinyApp(ui, server)
