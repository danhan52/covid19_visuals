library(tidyverse)
library(lubridate)
library(rvest)
setwd('C:/Users/danny/repos/covid19_visuals/')

# US COVID-19 data ####
# https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/
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
  mutate(
    fipstr = str_pad(FIPS, 5, 'left', '0'),
    date = as.Date(date, format = '%m/%d/%y')
  )

# https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902
us_pop <-
  read_csv(url(paste0('https://www2.census.gov/programs-surveys/popest/',
                      'datasets/2010-2019/counties/totals/',
                      'co-est2019-alldata.csv'))) %>%
  rename_all(tolower) %>%
  select(stname, ctyname, popestimate2019, state, county) %>%
  mutate(
    ctyname = str_replace(ctyname, ' County', ''),
    fipstr = paste0(state, county)
  )

covid_us <-
  covid_us %>%
  left_join(us_pop, by = 'fipstr')


# Global COVID-19 data ####
# https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/
covid_global <- 
  c(confirmed = 'confirmed', deaths = 'deaths') %>%
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
  mutate(date = as.Date(date, format = '%m/%d/%y'))

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901
canada_pop <-
  read_csv(url(paste0(
    'https://www150.statcan.gc.ca/t1/tbl1/en/dtl!downloadDbLoadingData',
    '-nonTraduit.action?pid=1710000901&latestN=5&startDate=&endDate=',
    '&csvLocale=en&selectedMembers=%5B%5B2%2C8%2C1%2C5%2C3%2C14%2C10%2C9',
    '%2C7%2C11%2C12%2C4%2C6%2C15%5D%5D'
  ))) %>%
  rename_all(tolower) %>%
  filter(ref_date == '2019-04') %>%
  select(province = geo, pop = value) %>%
  mutate(country = 'Canada')

# https://www.worldatlas.com/articles/the-largest-states-and-territories-of-australia.html
australia_pop <-
  read_html(
    'https://www.worldatlas.com/articles/the-largest-states-and-territories-of-australia.html'
  ) %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble() %>%
  mutate(
    country = 'Australia',
    province = `<U+FEFF>State/Territory`,
    pop = as.numeric(str_remove_all(Population, ','))
  ) %>%
  select(province, pop, country)

# https://www.worldatlas.com/articles/chinese-provinces-by-population.html
china_pop <-
  read_html('https://www.worldatlas.com/articles/chinese-provinces-by-population.html') %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble()
names(china_pop) <- c('rank', 'province', 'pop')
china_pop <-
  china_pop %>%
  mutate(
    country = 'China',
    pop = as.numeric(str_remove_all(pop, ','))
  ) %>%
  select(province, pop, country)

# china, canada, australia
covid_global <-
  covid_global %>%
  left_join(
    bind_rows(china_pop, canada_pop, australia_pop) %>%
      mutate(province = str_remove(province, '[^[:alnum:]|[:blank:]]')), 
    by = c('Province/State'='province', 'Country/Region'='country')
  )

# join and add world population ####
# https://worldpopulationreview.com/
world_pop <-
  read_html('https://worldpopulationreview.com/') %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble() %>%
  mutate(
    country = Country,
    cntry_pop = as.numeric(str_remove_all(`2019 Population`, ','))
  ) %>%
  select(country, cntry_pop)

covid <-
  bind_rows(
    covid_us %>%
      mutate(Province_State = str_replace(Province_State, 'Virgin Islands',
                                          'United States Virgin Islands')) %>%
      select(
        country = Country_Region, state = Province_State, county = Admin2,
        lat = Lat, long = Long_, date, metric, people, pop = popestimate2019
      ),
    covid_global %>%
      filter(!`Country/Region` %in% c('US')) %>%
      mutate(county = NA) %>%
      select(
        country = `Country/Region`, state = `Province/State`, county,
        lat = Lat, long = Long, date, metric, people, pop
      )
  ) %>%
  left_join(world_pop, by = c('state' = 'country')) %>%
  rename(state_pop = cntry_pop) %>%
  left_join(world_pop, by = 'country') %>%
  mutate(
    actl_pop = case_when(
      country == 'US' ~ coalesce(pop, state_pop),
      country %in% c('Canada', 'Australia', 'China') ~ pop,
      country %in% c('Netherlands', 'UK', 'France', 'Denmark') ~ coalesce(state_pop, cntry_pop),
      T ~ cntry_pop
    )
    # state = coalesce(state, country),
    # county = coalesce(county, state, country)
  ) %>%
  select(-c(pop, state_pop, cntry_pop))

covid_long <-
  covid %>%
  mutate(county = if_else(!is.na(county), paste0(county, ' County, ', state), as.character(NA))) %>%
  pivot_longer(cols = country:county, 
               names_to = 'loc_type',
               values_to = 'loc',
               values_drop_na = TRUE) %>%
  group_by(date, metric, loc_type, loc) %>%
  summarize(
    lat = mean(lat, na.rm = T),
    long = mean(long, na.rm = T),
    people = sum(people, na.rm = T),
    pop = sum(actl_pop, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(!is.na(date))

rm(list = c('covid', 'covid_global', 'covid_us', 'world_pop', 'us_pop', 
            'china_pop', 'canada_pop', 'australia_pop'))


# write_csv(covid_long, 'data/covid_long.csv')
# save(covid_long, file = 'data/covid.RData')
