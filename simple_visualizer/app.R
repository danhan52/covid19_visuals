library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
setwd('C:/Users/danny/repos/covid19_visuals/')

covid_global <- 
    c(confirmed = 'confirmed', deaths = 'deaths', recovered = 'recovered') %>%
    map(
        function(x) {
            paste0(
                'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
                'master/csse_covid_19_data/csse_covid_19_time_series/',
                'time_series_covid19_',
                x,
                '_global.csv'
            )
        }
    ) %>%
    map_df(function(x) read_csv(url(x)), .id = 'metric') %>%
    pivot_longer(cols = `1/22/20`:last_col(),
                 names_to = 'date',
                 values_to = 'people') %>%
    mutate(county = NA) %>%
    select(
        country_region = `Country/Region`,
        province_state = `Province/State`,
        county,
        date,
        metric,
        people,
        lat = Lat,
        long = Long
    )


covid_us <- 
    c('confirmed' = 'confirmed', 'deaths' = 'deaths') %>%
    map(
        function(x) {
            paste0(
                'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
                'master/csse_covid_19_data/csse_covid_19_time_series/',
                'time_series_covid19_',
                x,
                '_US.csv'
            )
        }
    ) %>%
    map_df(function(x) read_csv(url(x)), .id = 'metric') %>%
    pivot_longer(cols = `1/22/20`:last_col(),
                 names_to = 'date',
                 values_to = 'people') %>%
    select(
        country_region = `Country_Region`,
        province_state = `Province_State`,
        county = Admin2,
        date,
        metric,
        people,
        lat = Lat,
        long = Long_
    )

covid_us %>%
    left_join

mx_dt <- 
    dir('data') %>%
    .[str_detect(., 'global_covid19_through_')] %>%
    str_replace('global_covid19_through_(.+).csv', '\\1') %>%
    as_date() %>%
    max()
covid <- read_csv(paste0('data/global_covid19_through_', mx_dt, '.csv'), guess_max = 1e6)

if (mx_dt < today() - 1) {
    min_d <- as.Date(mx_dt)
    max_d <- as.Date(today() - 1)
    covid <- bind_rows(
        covid,
        map_df(seq(min_d, max_d, length.out = max_d - min_d + 1),
               download_date, type = 'global')
    )
    write_csv(covid, paste0('global_covid19_through_', max_d, '.csv'))
    rm(min_d, max_d)
}


us_pop <-
    read_csv('data/us_pop.csv') %>%
    rename_all(tolower) %>%
    select(stname, ctyname, popestimate2019) %>%
    mutate(ctyname = str_replace(ctyname, ' County', ''))
canada_pop <-
    read_csv('data/canada_pop.csv') %>%
    rename_all(tolower) %>%
    filter(ref_date == '2019-01') %>%
    select(geo, province_pop = value)
world_pop <-
    read_csv('data/world_pop.csv') %>%
    mutate(world_pop = pop2019 * 1000) %>%
    select(name, world_pop)

library(tictoc)
tic()
covid %>%
    filter(confirmed > 10 | deaths > 1, !str_detect(province_state, ',')) %>%
    mutate(
        country_region = str_replace(country_region, 'US', 'United States'),
        country_region = str_replace(country_region, 'UK', 'United Kingdom'),
        country_region = str_replace(country_region, 'Mainland China', 'China'),
        country_region = str_replace(country_region, 'Republic of Korea', 'North Korea'),
        country_region = str_replace(country_region, 'Korea, South', 'South Korea'),
        country_region = str_replace(country_region, 'Iran \\(Islamic Republic of\\)', 'Iran'),
        country_region = str_replace(country_region, 'Holy See', 'Vatican City'),
        country_region = str_replace(country_region, 'Russian Federation', 'Russia'),
        country_region = str_replace(country_region, 'Bahamas, The', 'Bahamas'),
        country_region = str_replace(country_region, 'The Bahamas', 'Bahamas'),
        country_region = str_replace(country_region, 'Czechia', 'Czech Republic')
        # country_region = str_replace(country_region, '', '')
    ) 
toc()
tic()
covid %>%
    filter(confirmed > 10 | deaths > 1, !str_detect(province_state, ',')) %>%
    mutate(
        country_region = str_replace(
            country_region, 
            c('US', 'UK', 'Mainland China', 'Republic of Korea', 'Korea, South', 'Iran \\(Islamic Republic of\\)',
              'Holy See', 'Russian Federation', 'Bahamas, The', 'The Bahamas', 'Czechia'), 
            c('United States', 'United Kingdom', 'China', 'North Korea', 'South Korea', 'Iran', 'Vatican City',
              'Russia', 'Bahamas', 'Bahamas', 'Czech Republic'))
    ) %>%
    filter(country_region %in% c('United States', 'United Kingdom', 'China', 'North Korea', 'South Korea', 'Iran', 'Vatican City',
                                 'Russia', 'Bahamas', 'Bahamas', 'Czech Republic'))
toc()

covid_pop <-
    covid %>%
    filter(confirmed > 10 | deaths > 1, !str_detect(province_state, ',')) %>%
    mutate(
        country_region = str_replace(country_region, 'US', 'United States'),
        country_region = str_replace(country_region, 'UK', 'United Kingdom'),
        # country_region = str_replace(country_region, 'Mainland China', 'China'),
        # country_region = str_replace(country_region, 'Republic of Korea', 'North Korea'),
        # country_region = str_replace(country_region, 'Korea, South', 'South Korea'),
        # country_region = str_replace(country_region, 'Iran \\(Islamic Republic of\\)', 'Iran'),
        # country_region = str_replace(country_region, 'Holy See', 'Vatican City'),
        # country_region = str_replace(country_region, 'Russian Federation', 'Russia'),
        # country_region = str_replace(country_region, 'Bahamas, The', 'Bahamas'),
        # country_region = str_replace(country_region, 'The Bahamas', 'Bahamas'),
        # country_region = str_replace(country_region, 'Czechia', 'Czech Republic')
        # country_region = str_replace(country_region, '', '')
    ) %>%
    left_join(world_pop, by = c('country_region'='name')) %>%
    left_join(canada_pop, by = c('province_state'='geo'))

covid_pop %>% filter(is.na(world_bank_pop)) %>%
    group_by(province_state) %>% summarize(dt = sum(deaths)) %>% arrange(desc(dt))

covid_pop %>% filter(country_region == 'Canada', is.na(province_pop)) %>%
    group_by(province_state) %>% summarize(dt = sum(deaths)) %>% arrange(desc(dt))

plot_state <- function(df, state, by_county = FALSE, metric = 'deaths', h = 600, w = 800) {
    df_plt <-
        df %>%
        filter(province_state %in% state) %>%
        mutate(state = province_state, county = admin2)
    if (!by_county) {
        df_plt <-
            df_plt %>%
            mutate(county = state)
    }
    df_plt <-
        df_plt %>%
        group_by(state, dwnld_date, county) %>%
        summarize(
            confirmed = sum(confirmed, na.rm = T),
            deaths = sum(deaths, na.rm = T),
            recovered = sum(recovered, na.rm = T)
        ) %>%
        ungroup() %>%
        group_by(state, county) %>%
        mutate(
            confirmed_day = confirmed - lag(confirmed, order_by = dwnld_date, default = 0),
            deaths_day = deaths - lag(deaths, order_by = dwnld_date, default = 0),
            recovered_day = recovered - lag(recovered, order_by = dwnld_date, default = 0)
        ) %>%
        ungroup()
    
    p <-
        df_plt %>%
        ggplot(aes(x = dwnld_date, group = county, color = state))
    if (metric == 'deaths') {
        p <- p + 
            geom_line(aes(y = deaths)) +
            geom_bar(aes(y = deaths_day), stat = 'identity')
    } else if (metric == 'confirmed') {
        p <- p + 
            geom_line(aes(y = confirmed)) +
            geom_bar(aes(y = confirmed_day), stat = 'identity')
    }
    ggplotly(p + geom_hline(yintercept = 100), height = h, width = w)
}

ui <- fluidPage(
    titlePanel("State Visualizer"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = 'state',
                label = 'State',
                choices = sort(unique(covid$province_state)),
                selected = 'Minnesota',
                multiple = TRUE
            ),
            selectInput(
                inputId = 'metric', 
                label = 'Metric',
                choices = c('deaths', 'confirmed')
            ),
            checkboxInput(
                inputId = 'by_county',
                label =  'Group by county',
                value = FALSE
            ),
            numericInput(
                inputId = 'h',
                label = 'Plot height',
                value = 600
            ),
            numericInput(
                inputId = 'w',
                label = 'Plot width',
                value = 800
            ),
            actionButton(
                inputId = 'refresh',
                label = 'Refresh COVID data'
            )
        ),
        mainPanel(
           plotlyOutput("covidPlot", width = '100%', height = '100%')
        )
    )
)

server <- function(input, output) {
    output$covidPlot <- renderPlotly({
        plot_state(
            covid,
            state = input$state,
            by_county = input$by_county,
            metric = input$metric,
            h = input$h,
            w = input$w
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
