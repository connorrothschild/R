#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  textInput("word", "word", "Data Summary"),
  verbatimTextOutput("word")
)
server <- function(input, output) {
  output$word <- renderText({ words$lastwordsfull[words$word %in% input$word] })
}
shinyApp(ui, server)

input <- "Jesus"
