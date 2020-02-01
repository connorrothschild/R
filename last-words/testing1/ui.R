shinyUI(pageWithSidebar(
  headerPanel('2016 US Presidential Debates Analyser'),
  sidebarPanel(
    h3("Select a word to find out which candidate said it more, and to find associated words."),
    p("Input explanation text for the user here"),
    h4("Type the word you're interested in below: "),
    textInput("word", label="", value = ""),
    actionButton("update", "Find word!")
  ),
  mainPanel(
    h2("Results appear here: "),
    p(textOutput('printText')), 
    h2("Associated words appear here: "),
    h3(textOutput('trumpGraphText')), 
    plotOutput('trumpGraph'), 
    h3(textOutput('clintonGraphText')), 
    plotOutput('clintonGraph')
    #   tableOutput('table1')
  )
  
  #fluidRow(column(3, verbatimTextOutput("value")))
  
))