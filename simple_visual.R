library(tidyverse)
library(lubridate)
library(plotly)
setwd('C:/Users/danny/repos/covid19_visuals/')

covid <- read_csv(paste0('global_covid19_through_', today()-1, '.csv'), guess_max = 1e6)
pop <- read_csv('pop_est_2019.csv') %>%
  rename_all(tolower)
names(pop) <- tolower(names(pop))

us_covid <- 
  covid %>%
  filter(country_region == 'US')

us_covid %>%
  filter(province_state == 'Minnesota') %>%
  group_by(dwnld_date) %>%
  summarize(deaths = sum(deaths, na.rm = T)) %>%
  ggplot(aes(x = dwnld_date, y = deaths)) +
  geom_line()



plot_state <- function(df, state, by_county = F, metric = 'deaths') {
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
  } else {
    p <- p + 
      geom_line(aes(y = recovered)) +
      geom_bar(aes(y = recovered_day), stat = 'identity')
  }
  ggplotly(p + geom_hline(yintercept = 100))
}

plot_state(us_covid, 'Minnesota', F)
plot_state(us_covid, 'Michigan', F)

plot_state(us_covid, c('Minnesota', 'Michigan'), F, 'confirmed')

plot_state(us_covid, c('Minnesota', 'Wisconsin'), F, 'confirmed')


