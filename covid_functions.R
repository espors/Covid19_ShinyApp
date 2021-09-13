

covid_rates <- function(covid_data, population){
  earliest <- min(covid_data$date)
  latest <- max(covid_data$date)
  dates <- as.data.frame(as_date(earliest:latest))
  dates <- dates %>%
    rename(date = `as_date(earliest:latest)`)
  date_matrix <- crossing(population, dates)
  covid_matrix <- left_join(date_matrix, covid_data, by = c("state", "county", "date"))
  covid_matrix <- covid_matrix %>%
    group_by(county) %>%
    arrange(date) %>%
    mutate(cumulativeCases = cumsum(cases)) %>%
    mutate(cumulativeDeaths) = cumsum(deaths) 
  covid_matrix$cases = covid_matrix$cases/population * 100000
  covid_matrix$deaths = covid_matrix$deaths/population * 100000
  covid_matrix$cumulativeCases = covid_matrix$cumulativeCases * 100000
  covid_matrix$cumulativeDeaths = covid_matrix$cumulativeDeaths * 100000
  return(covid_matrix)
}