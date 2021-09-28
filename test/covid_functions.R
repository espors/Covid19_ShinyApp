

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

sir_counties <- function(cumu_county, cumu_state){
  sir_df <- inner_join(cumu_county, cumu_state, by = 'state.x')
  
  a = 0.05 
  m = length(cumu_county$cases)
  
  sir_df$sir <- sir_df$cases.x/sir_df$cases.y
  sir_df$lsir <- (qchisq(a/(2*m), df = (2*sir_df$cases.x)))/(2*sir_df$cases.y)
  sir_df$usir <- (qchisq(a/(2*m), df = (2*sir_df$cases.x),lower.tail = FALSE))/(2*sir_df$cases.y)
  
  sir_df$sdr <- sir_df$deaths.x/sir_df$deaths.y
  sir_df$lsdr <- (qchisq(a/(2*m), df = (2*sir_df$deaths.x)))/(2*sir_df$deaths.y)
  sir_df$usdr <- (qchisq(a/(2*m), df = (2*sir_df$deaths.x),lower.tail = FALSE))/(2*sir_df$deaths.y)
  
  sir_df <- sir_df %>%
    mutate(typeC = case_when(sir > 1 ~ "More", 
                             sir <= 1 ~ "Less"), 
           typeD = case_when(sdr > 1 ~ "More", 
                             sdr <= 1 ~ "Less"), 
           significantC = case_when(usir > 1 & lsir < 1 ~ 0, 
                                    TRUE ~ 1),
           significantD = case_when(usdr > 1 & lsir < 1 ~ 0, 
                                    TRUE ~ 1))
  return(sir_df)
}


sir_states <- function(cumu_state, cumu_us){
  cumu_us <- cumu_us %>%
    select(caseRate, deathRate)
  
  sir_df <- crossing(cumu_state, cumu_us)
  
  sir_df <- drop_na(sir_df)
  
  a = 0.05 
  m = length(cumu_state$cases)
  
  sir_df$sir <- sir_df$cases/sir_df$caseRate
  sir_df$lsir <- (qchisq(a/(2*m), df = (2*sir_df$cases)))/(2*sir_df$caseRate)
  sir_df$usir <- (qchisq(a/(2*m), df = (2*sir_df$cases),lower.tail = FALSE))/(2*sir_df$caseRate)

  sir_df$sdr <- sir_df$deaths/sir_df$deathRate
  sir_df$lsdr <- (qchisq(a/(2*m), df = (2*sir_df$deaths)))/(2*sir_df$deathRate)
  sir_df$usdr <- (qchisq(a/(2*m), df = (2*sir_df$deaths),lower.tail = FALSE))/(2*sir_df$deathRate)

  sir_df <- sir_df %>%
    mutate(typeC = case_when(sir > 1 ~ "More",
                             sir <= 1 ~ "Less"),
           typeD = case_when(sdr > 1 ~ "More",
                             sdr <= 1 ~ "Less"),
           significantC = case_when(usir > 1 & lsir < 1 ~ 0,
                                    TRUE ~ 1),
           significantD = case_when(usdr > 1 & lsir < 1 ~ 0,
                                    TRUE ~ 1))
  return(sir_df)
}
