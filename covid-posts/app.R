## FIXME: Author should just be first name last name

library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(magrittr)
library(reactable)

df <- readxl::read_excel('postsTopicsRoles1019.xlsx') %>%
    mutate(Date = lubridate::as_date(Date),
           # consolidate country & geospecific
           Country = ifelse(is.na(Country), GeoSpecific, Country),
           Country = ifelse(Country == 'United States of America', 'USA', Country))

ui <- shinyUI(fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
 
    titlePanel(title = 'COVID-19 Blog Post Directory',
               windowTitle = 'COVID-19 Blog Post Directory'),
    
    fluidRow(
        column(
            4,
            div(h4(
                'A collection of posts by the #rstats community'
            )),
            br(),
            br(),
            selectInput(
                inputId = 'Role',
                label = 'Filter by Job Title',
                choices = c("All", df$Role[!is.na(df$Role)]),
                selected = "All"
            ),
            selectInput(
                inputId = 'Country',
                label = 'Filter by Geography',
                choices = c("All", df$Country[!is.na(df$Country)]),
                selected = "All"
            ),
            selectInput(
                inputId = 'Topic',
                label = 'Filter by Topic',
                choices = c("All", df$Topic[!is.na(df$Topic)]),
                selected = "All"
            ),
            dateRangeInput(
                "dateRange",
                "Filter by Date",
                start = min(df$Date) - 1,
                end   = max(df$Date) + 1,
                min   = min(df$Date) - 1,
                max   = max(df$Date) + 1
            ),
            
            htmlOutput('matchText'),
            
        ),
        column(
            8,
            conditionalPanel(condition = 'output.nrow != "0"',
                             reactableOutput("table")),
        )
    ),
    br(),
    hr(),
    fluidRow(
    column(12, 
    HTML(paste0(
    "<strong>Note:</strong> Although the #rstats community is intelligent, driven, and knowledgable,
                  not every member of the community is an expert when it comes to COVID-19.",
                br(),
                  "Engaging with these blog posts should be an opportunity to learn more and give feedback,
                  but we advise against using the content of blog posts to inform your approach to or
                  understanding of COVID-19.",
                  br(),
                  "For the most up-to-date and accurate information related to the Coronavirus, visit ",
                  tags$a(href = 'https://www.cdc.gov/', target = "_blank", 'cdc.gov'), "."
    ))))
))

server <- function(input, output, session) {
    chosen_dataset <- reactive({
        if (input$Role != "All") {
            df <- df %>%
                filter(Role == input$Role)
        }
        if (input$Country != "All") {
            df <- df %>%
                filter(Country == input$Country)
        }
        if (input$Topic != "All") {
            df <- df %>%
                filter(Topic == input$Topic)
        }
        
        df <- df %>%
            filter(Date > format(input$dateRange[1]) &
                       Date < format(input$dateRange[2]))
        
        df <- df %>%
            select(-c(
                Text,
                LastName,
                Week,
                WeekYear,
                PostName,
                Region,
                Geo,
                GeoFocus
            )) %>%
            select(Title, Author, Date, everything())
        return(df)
    })
    
    url_title_lookup <- df %>%
        select(URL, Title)
    
    sticky_style <-
        list(
            position = "sticky",
            left = 0,
            background = "#f5f5f5",
            zIndex = 1,
            borderRight = '1px solid white'
        )
    sticky_style_header <-
        list(
            position = "sticky",
            left = 0,
            background = "white",
            zIndex = 1,
            fontSize = '14px',
            borderRight = '1px solid #f5f5f5'
        )
    
    output$table <- renderReactable({
        reactable(
            chosen_dataset(),
            bordered = TRUE,
            pagination = FALSE,
            height = 500,
            searchable = TRUE,
            showSortIcon = FALSE,
            highlight = TRUE,
            compact = FALSE,
            theme = reactableTheme(
                backgroundColor = '#f5f5f5'
            ),
            language = reactableLang(
                searchPlaceholder = "Search for a post, author, or topic",
                noData = "No entries found",
                pageInfo = "{rowStart} to {rowEnd} of {rows} entries",
                pagePrevious = "\u276e",
                pageNext = "\u276f",
                
                # Accessible labels for assistive technologies such as screen readers.
                # These are already set by default, but don't forget to update them when
                # changing visible text.
                pagePreviousLabel = "Previous page",
                pageNextLabel = "Next page"
            ),
            defaultColDef = colDef(headerClass = "header",
                                   na = "–"),
            columns = list(
                URL = colDef(show = FALSE),
                Email = colDef(show = FALSE),
                GeoSpecific = colDef(show = FALSE),
                Country = colDef(name = 'Location'),
                Date = colDef(format = colFormat(date = TRUE)),
                Data = colDef(name = "Has Data?"),
                Code = colDef(name = "Has Code?"),
                Math = colDef(name = "Has Math?"),
                Author = colDef(minWidth = 200),
                Role = colDef(name = 'Job Title'),
                CoreRole = colDef(name = "Employment", minWidth = 120),
                Title = colDef(
                    name = "Post Title",
                    style = sticky_style,
                    headerStyle = sticky_style_header,
                    minWidth = 250,
                    cell = function(value, index) {
                        htmltools::tags$a(href = url_title_lookup[url_title_lookup$Title == value,]$URL,
                                          target = "_blank",
                                          value)
                    }
                )
            )
        )
    })
    
    # numMatch <- nrow(chosen_dataset())
    output$nrow <- renderText({nrow(chosen_dataset())})
    output$matchText <- renderUI({
        HTML(paste0("There are <strong>",
               nrow(chosen_dataset()),
               " posts</strong> that match your criteria."))
    })
}


shinyApp(ui, server)
