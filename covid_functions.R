

covid_rates <- function(covid_data, population){
  earliest <- min(covid_data$date)
  latest <- max(covid_data$date)
  dates <- as.data.frame(as_date(earliest:latest))
  dates <- dates %>%
    rename(date = `as_date(earliest:latest)`)
  date_matrix <- crossing(population, dates)
  covid_matrix <- left_join(date_matrix, covid_data, by = c("fips", "date"))
  covid_matrix[, c("cases", "deaths")] <- na_replace(covid_matrix[,c("cases", "deaths")],0)

  
  covid_matrix$cases = covid_matrix$cases/covid_matrix$population * 100000
  covid_matrix$deaths = covid_matrix$deaths/covid_matrix$population * 100000

  return(covid_matrix)
  }

us_rates <- function(state_covid, us_population){
  earliest <- min(state_covid$date)
  latest <- max(state_covid$date)
  dates <- as.data.frame(as_date(earliest:latest))
  dates <- dates %>%
    rename(date = `as_date(earliest:latest)`)
  date_matrix <- crossing(us_population, dates)
  us_covid <- state_covid %>%
    group_by(date) %>%
    summarise(cases = sum(cases), 
              deaths = sum(cases))
  
  covid_matrix <- left_join(date_matrix, us_covid, by = c("date"))
  covid_matrix[,c("cases", "deaths")] <- na_replace(covid_matrix[,c("cases", "deaths")], 0)
  
  covid_matrix$cases = covid_matrix$cases/covid_matrix$population * 100000
  
  covid_matrix$deaths = covid_matrix$deaths/covid_matrix$population * 100000
  
  return(covid_matrix)
}


cumulative_county <- function(county_covid, county_pop) {
  covid <- county_covid %>%
    filter(date == max(date))
  
  covid_matrix <- left_join(covid, county_pop, by = "fips")
  
  covid_matrix$cases = covid_matrix$cases/covid_matrix$population * 100000
  covid_matrix$deaths = covid_matrix$deaths/covid_matrix$population * 100000
  
  covid_matrix <- drop_na(covid_matrix)
  
  return(covid_matrix)
  
}


cumulative_state <- function(state_covid, state_pop) {
  covid <- state_covid %>% 
    filter(date == max(date))

  covid_matrix <- left_join(covid, state_pop, by = "fips")
  
  covid_matrix$cases = covid_matrix$cases/covid_matrix$population * 100000
  covid_matrix$deaths = covid_matrix$deaths/covid_matrix$population * 100000
  return(covid_matrix)
}


cumulative_us <- function(state_covid, us_pop) {
  covid <- state_covid %>%
    filter(date == max(date)) %>%
    summarise(cases = sum(cases), 
              deaths = sum(deaths))
  
  covid_matrix <- crossing(covid, us_pop)
  
  covid_matrix$caseRate <- covid_matrix$cases/covid_matrix$population * 100000
  covid_matrix$deathRate <- covid_matrix$deaths/covid_matrix$population * 100000
  
  return(covid_matrix)
}
