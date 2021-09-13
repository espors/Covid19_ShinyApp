library(readr)
library(tidyverse)

source("covid_functions.R")

county <- read_csv("co-est2020-alldata.csv")

county_population <- county %>% 
  filter(SUMLEV == '050') %>%
  separate(CTYNAME, c("CTYNAME", "Other", sep = " Cou")) %>%
  select(STNAME, CTYNAME, POPESTIMATE2020) %>%
  rename(state = STNAME, 
         county = CTYNAME, 
         population = POPESTIMATE2020)
  

write.csv(county_population, "county_population.csv")

state_population <- county %>%
  filter(SUMLEV == '040') %>%
  select(STNAME, CTYNAME, POPESTIMATE2020) %>%
  rename(state = STNAME, 
         county = CTYNAME, 
         population = POPESTIMATE2020)

write.csv(state_population, "state_population.csv")

united_states_pop <- states %>%
  summarise(state= 'United States', 
            county = 'United States',
            population = sum(POPESTIMATE2020))



write.csv(united_states_pop, "united_states_pop.csv")

