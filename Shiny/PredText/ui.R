library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Predictive text model"), 
    sidebarLayout(
        sidebarPanel(
            h3("How to use"),
            "Simply enter the text into the field below...",
            textInput("txtIn", "Input text", value="one of"),
            sliderInput("numWords", "Number of predicted words", min=0, max=10, value=3)
    ),
    
    mainPanel(
        h3("Next word"),
        tableOutput("txtOut")
    )
  )
))
