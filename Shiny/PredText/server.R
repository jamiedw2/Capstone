library(shiny)
library(tm)
source("~/Coursera/Capstone/code/PredTextModel2.R")

shinyServer(function(input, output) {
    
    output$txtOut <- renderTable({
        PredTextModel2(input$txtIn, input$numWords)
    }, colnames=FALSE)
    
  })