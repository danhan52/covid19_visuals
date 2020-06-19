library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
options(scipen = 100)
setwd('C:/Users/danny/repos/covid19_visuals/')

# load('data/covid.RData')
load(url('https://raw.githubusercontent.com/danhan52/covid19_visuals/master/data/covid.RData'))

ui <- fluidPage(
    title = paste('COVID-19 Visualizer -', max(covid_long$date)),
    titlePanel(paste('COVID-19 Visualizer -', max(covid_long$date))),
    plotlyOutput("covidPlot", width = '100%', height = '100%'),
    
    hr(),
    
    fluidRow(
        column(4,
               selectInput(
                   inputId = 'loc',
                   label = 'Location (Country, State/Province, County)',
                   choices = sort(unique(covid_long$loc)),
                   selected = c('Minnesota', 'Cyprus', 'Japan', 'Quebec'),
                   multiple = TRUE
               ),
               selectInput(
                   inputId = 'metric', 
                   label = 'Metric (deaths, confirmed)',
                   choices = c('deaths', 'confirmed'),
                   selected = 'deaths',
                   multiple = TRUE
               )
        ),
        column(4,
               sliderInput(
                   inputId = 'st_date',
                   label = 'Start Date',
                   min = min(covid_long$date),
                   max = max(covid_long$date),
                   value = as.Date('2020-03-15')
               ),
               checkboxInput(
                   inputId = 'percap',
                   label = 'Per 1000 People'
               ),
               checkboxInput(
                   inputId = 'total',
                   label = 'Show total line',
                   value = TRUE
               ),
               checkboxInput(
                   inputId = 'change',
                   label = 'Show daily change',
                   value = FALSE
               )
        ),
        column(4,
               numericInput(
                   inputId = 'w',
                   label = 'Plot width',
                   value = 1200
               ),
               numericInput(
                   inputId = 'h',
                   label = 'Plot height',
                   value = 700
               ),
               actionButton(
                   inputId = 'refresh',
                   'Refresh Data (>30 seconds)'
               )
        )
    ),
    p('Data from a variety of sources and can be found on my github: github.com/danhan52')
)

server <- function(input, output) {
    output$covidPlot <- renderPlotly({
        plt_df <- 
            covid_long %>%
            filter(
                loc %in% input$loc,
                metric %in% input$metric,
                date >= input$st_date
            ) %>%
            group_by(loc, loc_type, metric) %>%
            mutate(
                delta = people - lag(people, order_by = date, default = 0),
                percap = input$percap,
                people = if_else(percap, people / pop * 1000, people),
                delta = if_else(percap, delta / pop * 1000, delta)
            )
        
        
        line_plt <-
            plt_df %>%
            ggplot(aes(x = date, color = loc, linetype = metric))
        if (input$total) {
            line_plt <-
                line_plt +
                geom_line(aes(y = people))
        }
        if (input$change) {
            line_plt <-
                line_plt +
                geom_bar(aes(y = delta), stat = 'identity')
        }
        
        ggplotly(line_plt, width = input$w, height = input$h)
    })
    
    new_data <- observeEvent(input$refresh, {
        print(paste('refreshing', input$refresh))
        source('covid_downloaders.R')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
