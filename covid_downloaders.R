library(tidyverse)
library(lubridate)
# setwd('C:/Users/danny/repos/covid19_visuals/')

download_date <- function(dte = today() - 1, type = c('global', 'us')) {
  # from https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
  print(dte)
  type <- match.arg(type)
  url_base <-
    switch (type,
            'global' = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/',
            'us' = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/'
            # 'ts' = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/'
    )
  
  dt <- format(dte, '%m-%d-%Y')
  df <- 
    url(paste0(url_base, dt, '.csv')) %>%
    read_csv()
  
  names(df) <- str_replace_all(names(df), '\\/|\\ ', '_')
  if ('FIPS' %in% names(df)) {
    df <- df %>% mutate(FIPS = as.numeric(FIPS))
  }
  
  df <- 
    df %>%
    mutate(
      dwnld_date = dte,
      Last_Update = as.POSIXct(
        Last_Update,
        tryFormats = c("%Y-%m-%d %H:%M:%OS",
                       "%Y/%m/%d %H:%M:%OS",
                       "%Y-%m-%d %H:%M",
                       "%Y/%m/%d %H:%M",
                       "%Y-%m-%d",
                       "%Y/%m/%d",
                       "%m/%d/%Y %H:%M",
                       "%m/%d/%y %H:%M"))
    )
  df
}

# min_d <- date('2020-01-22')
# max_d <- today() - 1
# df <- map_df(seq(min_d, max_d, length.out = max_d - min_d),
#              download_date, type = 'global')
# names(df) <- tolower(names(df))
# write_csv(df, paste0('data/global_covid19_through_', max_d, '.csv'))
