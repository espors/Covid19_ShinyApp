

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

us_rates <- function(county_covid, us_population){
  county_covid <- county_covid %>%
    select(-c(fips))
  county_covid <- na_replace(county_covid,0)
  us_covid <- county_covid %>%
    summarise(cases = sum(cases), deaths = sum(deaths))
  
  us_covid <- crossing(us_covid, population)
  us_covid <- us_covid %>%
    mutate(rateCases = cases/population*100000, 
           rateDeaths = deaths/population*100000)
}

