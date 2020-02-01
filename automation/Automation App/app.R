library(shiny)
library(ggvis)

source("./source_code_for_shiny.R")

# Define UI for application 
ui <- pageWithSidebar(
  headerPanel("Automation and Its Impact on Jobs"),
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
    ggvisOutput("plot")
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
  
  
  vis <- reactive({
    
    data1 <- dfInput()
    
    data1 %>% 
    ggvis(~A_MEDIAN, ~probability, opacity := 0.3, key := ~occupation) %>% 
      scale_numeric('size',domain = c(100000,500000),range=c(100,500)) %>%
      layer_points(fill = ~typicaled, size = ~TOT_EMP) %>% 
      add_tooltip(function(data1) glue::glue('<span style="font-size:16px;font-weight:bold">{data1$occupation}</span>',
                                       '<b>Number employed:</b> {data1$TOT_EMP}',
                                       '<b>Computerization prob:</b> {data1$probability*100}%',
                                       '<b>Education:</b> {data1$typicaled}',
                                       .sep = "<br />")) %>% 
      add_axis("x", title = "Median Income") %>%
      add_axis("y", title = "Probability of Automation", title_offset = 50) %>% 
      add_legend("fill", title = "Education", properties = legend_props(legend = list(y = 200))) %>%
      add_legend("size", title = "Number of Workers", properties = legend_props(legend = list(y = 50))) %>%
      scale_numeric("x", domain = c(25000, 200000), nice = FALSE) %>%
      scale_numeric("y", domain = c(0, 1), nice = FALSE) %>% 
      set_options(duration = 0, width = 900, height = 500)
      # #geom_smooth(aes(x=A_MEDIAN, y=probability), method="lm", se=FALSE) +
      # scale_size_area(max_size = 20) +
      # scale_alpha(guide = 'none') +
      # guides(size = "none") +
      # theme(legend.position = "bottom") +
      # guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      # ylim(-.05,1.05) +
      # xlim(25000,200000) +
      # xlab("Median Income") +
      # ylab("Probability of Automation") +
      # ggtitle("Likelihood of Job Automation vs Median Income") +
      # labs(size="Total Employment", col="Education Level")
  })
  vis %>% bind_shiny("plot")
}

shinyApp(ui = ui, server = server)
