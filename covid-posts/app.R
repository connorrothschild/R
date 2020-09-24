library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(magrittr)

df <- readxl::read_excel('postsTopicsRoles0920.xlsx') %>% 
    mutate(Data = case_when(Data == "Y" ~ "Yes", 
                            Data == "N" ~ "No",
                            TRUE ~ "Unclear"),
           Code = case_when(Code == "Y" ~ "Yes", 
                            Code == "N" ~ "No",
                            TRUE ~ "Unclear"),
           Math = case_when(Math == "Y" ~ "Yes", 
                            Math == "N" ~ "No",
                            TRUE ~ "Unclear")
    ) %>% 
    select(-Text)

ui <- fluidPage(
fluidRow(
    column(
        3,
        tabPanel(
            "Filter by Role",
            br(),
            selectInput(
                inputId = 'Role',
                label = 'Choose a Role',
                choices = c("All", df$Role[!is.na(df$Role)]),
                selected = "All"
            )
        ),
        tabPanel(
            "Filter by Geography",
            br(),
            selectInput(
                inputId = 'GeoSpecific',
                label = 'Choose a Geography',
                choices = c("All", df$GeoSpecific[!is.na(df$GeoSpecific)]),
                selected = "All"
            )
        ),
        tabPanel(
            "Filter by Topic",
            br(),
            selectInput(
                inputId = 'Topic',
                label = 'Choose a Topic',
                choices = c("All", df$Topic[!is.na(df$Topic)]),
                selected = "All"
            )
        ),
        tabPanel(
            "Has Code?",
            br(),
            selectInput(
                inputId = 'Code',
                label = "Has Code?",
                choices = c("All", df$Code[!is.na(df$Code)]),
                selected = "All"
            )
        ),
        tabPanel(
            "Has Data?",
            br(),
            selectInput(
                inputId = 'Data',
                label = "Has Data?",
                choices = c("All", df$Data[!is.na(df$Data)]),
                selected = "All"
            )
        )
    ), 
    column(
        9,
        conditionalPanel(condition = 'output.nrow != "No Data"',
                         reactable::reactableOutput("table")),
        textOutput("nrow")
    )
))

server <- function(input, output, session) {
    
    chosen_dataset <- reactive({
        if (input$Role != "All") {
            df <- df %>%
                filter(Role == input$Role)
        }
        if (input$GeoSpecific != "All") {
            df <- df %>%
                filter(GeoSpecific == input$GeoSpecific)
        }        
        if (input$Topic != "All") {
            df <- df %>%
                filter(Topic == input$Topic)
        }
        if (input$Data != "All") {
            df <- df %>%
                filter(Data == input$Data)
        }
        if (input$Code != "All") {
            df <- df %>%
                filter(Code == input$Code)
        }
        return(df)
    })
    
    output$table <- reactable::renderReactable({reactable::reactable(chosen_dataset())})
    output$nrow <- renderText({ 
        ifelse(nrow(chosen_dataset()) == 0, "No Data", nrow(chosen_dataset()))
               })
}


shinyApp(ui, server)
