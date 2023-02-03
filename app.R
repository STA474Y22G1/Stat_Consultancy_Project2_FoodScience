## app.R ##

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)

oilData <- read_csv("Tidy Data.csv")

#oilData$`Palm olein concentration(C)` <- as.factor(oilData$`Palm olein concentration(C)`)
# oilData$`Replicate No` <- as.factor(oilData$`Replicate No`)
oilData <- rename(oilData, Concentration = `Palm olein concentration(C)`, 
                  Replicate = `Replicate No`)


ui <- dashboardPage( 
  dashboardHeader(title= h3("Spectrum Viz")),
  dashboardSidebar(
    selectInput("Series", label = h4("Select Series"), 
                choices = unique(oilData$Series), multiple = TRUE),
    selectInput("Concentration", label = h4("Select Palm Olein Concentration"), 
                choices = NULL),
    selectInput("Replicate", label = h4("Select Replicate"), 
                choices = NULL)
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("plot", height = 570), width = 12)
    )
    
  )
)

server <- function(input, output, session) {
  
  # updating filters
  series <- reactive({
    req(input$Series)
    filter(oilData, Series == input$Series)
  })
  
  concentration <- reactive({
    req(input$Concentration)
    filter(series(), Concentration == input$Concentration)
  })
  
  replicate <- reactive({
    req(input$Replicate)
    filter(concentration(), Replicate == input$Replicate)
  })
  
  # observing event to update next filter
  observeEvent(series(), {
    updateSelectInput(session, "Concentration", 
                      choices = sort(unique(series()$Concentration)), 
                      selected = c("Pure Palm Oil","Pure VCO"))
  })
  
  observeEvent(concentration(), {
    updateSelectInput(session, "Replicate", 
                      choices = unique(concentration()$Replicate), selected = 1)
  })
  
  
  
  
  output$plot <- renderPlotly({
    plot <- replicate() %>%
      ggplot(aes(x = `Wave Number (cm-1)(W)`, y = `Absorption (A)`)) + 
      geom_line() + 
      theme_bw() +
      labs(x = "Wave Number (cm-1)", y = "Absorption", title = paste("Spectrum of", input$Series, 
                                                                     "For Replicate", input$Replicate))
    ggplotly(plot)
    
  })
}


shinyApp(ui, server)