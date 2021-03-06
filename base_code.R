# Author: Emma Spors 
# Date: August 2021 
# Purpose: Write code to clean, understand, and develop 
#   tests/graphs to be used in shiny app 
----------------------------------
  
# Necessary Libraries ----------

library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(imputeTS)
source("covid_functions.R")

# Import Data ---------

covid_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

county_population <- read_csv("county_population.csv")
state_population <- read_csv("state_population.csv")
united_states_pop <- read_csv("united_states_pop")



# Data Preparation -------

# dataframe for every date from the earliest
# to most current date 
earliest <- min(covid_counties$date)
latest <- max(covid_counties$date)
dates <- as.data.frame(as_date(earliest:latest))

# all unique county FIPS codes 
unique_fips <- as.data.frame(unique(covid_counties$fips))

county_pop <- county %>%
  select(SUMLEV, REGION, DIVISION, STATE, COUNTY, STNAME,
         CTYNAME, POPESTIMATE2020) %>%
  unite("FIPS", STATE:COUNTY, sep = "")


test <- crossing(county_population, dates)

county_rates <- covid_rates(covid_counties, county_population)

covid_data <- covid_counties
population <- county_population

earliest <- min(covid_data$date)
latest <- max(covid_data$date)
dates <- as.data.frame(as_date(earliest:latest))
dates <- dates %>%
  rename(date = `as_date(earliest:latest)`)
date_matrix <- crossing(population, dates)
covid_matrix <- left_join(date_matrix, covid_data, by = c("state", "county", "date"))
covid_matrix <- na_replace(covid_matrix,0)

covid_matrix <- covid_matrix %>%
  group_by(county) %>%
  arrange(date) %>%
  mutate(cumulativeCases = cumsum(cases)) %>%
  mutate(cumulativeDeaths = cumsum(deaths)) 

covid_matrix$cases = covid_matrix$cases/covid_matrix$population * 100000
covid_matrix$deaths = covid_matrix$deaths/covid_matrix$population * 100000
covid_matrix$cumulativeCases = covid_matrix$cumulativeCases/covid_matrix$population * 100000
covid_matrix$cumulativeDeaths = covid_matrix$cumulativeDeaths/covid_matrix$population * 100000

