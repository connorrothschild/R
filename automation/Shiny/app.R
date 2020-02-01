library(shiny)

# Define UI for application 
ui <- pageWithSidebar(
  headerPanel("Automation"),
  sidebarPanel(
    wellPanel(
      h4("Filter"),
      sliderInput("TOT_EMP", "Number of Workers",
                  0, 4450000, 4450000, step = 10000),
      sliderInput("A_MEDIAN", "Median Income", 
                  0, 185150, 185150, step = 1000),
      sliderInput("probability", "Probability of Automation",
                  0, 1, 1, step = .1),
      # sliderInput("boxoffice", "Dollars at Box Office (millions)",
      #             0, 800, c(0, 800), step = 1),
      selectInput("typicaled", "Education Level",
                  c("All", "Bachelor's degree", "High school diploma or equivalent", "Associate's degree", "Postsecondary nondegree award",
                    "No formal educational credential", "Master's degree", "Doctoral or professional degree", "Some college, no degree")
      ))
      #textInput("occupation", "Occupation Name"))
  ),
  mainPanel(
    plotOutput("plot")
  )
)

server <- function(input, output) {
  
  # defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
  # series <- structure(
  #   lapply(defaultColors, function(color) { list(color=color) }),
  #   names = levels(data$typicaled)
  # )
  
  
  dfInput <- reactive({
    if (input$typicaled!="All") {
    data %>% filter(TOT_EMP <= input$TOT_EMP,
                    A_MEDIAN <= input$A_MEDIAN,
                    probability <= input$probability,
                    typicaled %in% input$typicaled)
    #occupation == input$occupation)
      } else { 
    data %>% filter(TOT_EMP <= input$TOT_EMP,
                    A_MEDIAN <= input$A_MEDIAN,
                    probability <= input$probability)
      }
    })

  
  output$plot <- renderPlot({
    
    data1 <- dfInput()
    
    ggplot(data1) +
      geom_point(mapping = aes(x = A_MEDIAN, y = probability, size = TOT_EMP, alpha=0.05, col = typicaled)) +
      # #geom_smooth(aes(x=A_MEDIAN, y=probability), method="lm", se=FALSE) +
      scale_size_area(max_size = 20) +
      scale_alpha(guide = 'none') +
      guides(size = "none") +
      theme(legend.position = "bottom") +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      ylim(-.05,1.05) +
      xlim(25000,200000) +
      xlab("Median Income") +
      ylab("Probability of Automation") +
      ggtitle("Likelihood of Job Automation vs Median Income") +
      labs(size="Total Employment", col="Education Level")
  })
}

shinyApp(ui = ui, server = server)