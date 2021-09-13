library(readr)
library(tidyverse)

county <- read_csv("co-est2020-alldata.csv")

county_population <- county %>% 
  filter(SUMLEV == '050') %>%
  separate(CTYNAME, c("CTYNAME", "Other", sep = " Cou")) %>%
  select(STNAME, CTYNAME, POPESTIMATE2020)
  

write.csv(county_population, "county_population.csv")

state_population <- county %>%
  filter(SUMLEV == '040') %>%
  select(STNAME, CTYNAME, POPESTIMATE2020)

write.csv(state_population, "state_population.csv")

united_states_pop <- states %>%
  summarise(STNAME= 'United States', 
            CTYNAME = 'United States',
            POPESTIMATE2020 = sum(POPESTIMATE2020))



write.csv(united_states_pop, "united_states_pop.csv")

