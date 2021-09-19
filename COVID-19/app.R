
library(shiny)
library(tidyverse)
library(lubridate)
library(imputeTS)
library(car)
source("covid_functions.r")

#read in data 
covid_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

county_population <- read_csv("county_population.csv")
state_population <- read_csv("state_population.csv")
united_states_pop <- read_csv("united_states_pop.csv")

#create new data sets 
covidCounties <- covid_rates(covid_counties, county_population)
covidStates <- covid_rates(covid_states, state_population)
covidUs <- us_rates(covid_states, united_states_pop)
cumulativeCounties <- cumulative_county(covid_counties, county_population)
cumulativeStates <- cumulative_state(covid_states, state_population)
cumulativeUS <- cumulative_us(covid_states, united_states_pop)

# Define UI for application that draws a histogram


ui <- fluidPage(
    titlePanel("COVID-19: An Overview in the United States"),
    tabsetPanel(
        tabPanel("United States",
                 fluidRow(
                     column(6,
                            ), 
                     column(6,
                            ),
                 ), 
                 fluidRow(
                     column(3, verbatimTextOutput("rawCases")
                            ), 
                     column(3, verbatimTextOutput("cumuCases")
                            ), 
                     column(3, verbatimTextOutput("rawDeaths")
                            ), 
                     column(3, verbatimTextOutput("cumuDeaths")
                            )
                 )
    
    
        ), 
        tabPanel("States"), 
        tabPanel("Counties"), 
        tabPanel("Data and Methodology")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$rawCases <- renderText({cumulativeUS$cases})
    output$cumuCases <- renderText({cumulativeUS$caseRate})
    output$rawDeaths <- renderText({cumulativeUS$deaths})
    output$cumuDeaths <- renderText({cumulativeUS$deathRate})

}

# Run the application 
shinyApp(ui = ui, server = server)
