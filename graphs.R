library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
library(imputeTS)
source("covid_functions.R")

covid_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

county_population <- read_csv("county_population.csv")
state_population <- read_csv("state_population.csv")
united_states_pop <- read_csv("united_states_pop.csv")

county_rates <- covid_rates(covid_counties, county_population)
