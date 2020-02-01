library(tm)
library(data.table)
library(shiny)
library(ggplot2)

shinyServer(function(input, output){
  
  observeEvent(input$update), {
    
    output$printText <- reactive({
      
      #paste together correct output text
      textDiff <- paste0("The word or phrase you entered was ", 
                         tolower(input$word), 
      
      #assign final output text               
      printText
      )     
    })
  })
})