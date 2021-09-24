
library(shiny)
library(tidyverse)
library(lubridate)
library(imputeTS)
library(car)
library(plotly)
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

#define map information 
mapDataStates <- cumulativeStates
state <- map_data("state")

mapDataStates$state.x <- tolower(mapDataStates$state.x)
colnames(mapDataStates)[colnames(mapDataStates) == "state.x"] <- "region"
states <- inner_join(state, mapDataStates, by = "region")

mapBaseStates <- ggplot(data = state, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(color = "black", fill = "gray")  + 
    ylab('') + 
    xlab('') + 
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(), 
          legend.position = 'none')

mapStatesCases <- mapBaseStates + 
    geom_polygon(data = states, aes(fill = cases), color = "white") + 
    scale_fill_gradient(low = "white", high = "darkolivegreen4")

mapStatesCases <- ggplotly(mapStatesCases)

mapStatesDeaths <- mapBaseStates + 
    geom_polygon(data = states, aes(fill = deaths), color = "white") + 
    scale_fill_gradient(low = "white", high = "darkblue")

mapStatesDeaths <- ggplotly(mapStatesDeaths)


dodge <- position_dodge(width = 0.9)
custom <- c("#335C67", "#E09F3E")

# Define UI for application that draws a histogram

ui <- fluidPage(
    titlePanel("COVID-19: An Overview in the United States"),
    tabsetPanel(
        tabPanel("United States",
                 fluidRow(
                     h3("Maps are worth a 1,000 words..."),
                     column(6,
                            h5("Cumulative Cases per 100,000 in the United States", align = 'center'), 
                            plotlyOutput("UsCaseMap")
                            ), 
                     column(6,
                            h5("Cumulative Deaths per 100,000 in the United State", align = 'center'), 
                            plotlyOutput("UsDeathMap")
                            ),
                 ), 
                 fluidRow(
                     h3("By the numbers..."),
                     column(3,
                            h5("Cumulative Cases in the US (Raw Counts)", align = 'center'),
                            h4(verbatimTextOutput("rawCases"), align = 'right')
                     ), 
                     column(3,
                            h5("Cumulative Cases\nPer 100,000 in the US", align = 'center'),
                            h4(verbatimTextOutput("cumuCases"), align = 'right')
                     ),
                     column(3,
                            h5("Cumulative Deaths\n(Raw Counts) in the US", align = 'center'),
                            h4(verbatimTextOutput("rawDeaths"), align = 'right')
                     ), 
                     column(3, 
                            h5("Cumulative Deaths\nPer 100,000 in the US", align = 'center'),
                            h4(verbatimTextOutput("cumuDeaths"), align = 'right')
                     )
                 )
    
    
        ), 
        tabPanel("States", 
                 fluidRow(
                     column(3, 
                            selectInput("state", "Select State(s)", cumulativeStates$state.x, multiple = TRUE, selected = "Arizona"),
                            selectInput("cases_deaths", "Select Outcome", choices = c("Cases" = "cases", "Deaths" = "deaths", selected = "Cases"))
                            ), 
                     column(9, 
                            plotlyOutput("mapCumulativeState")
                            )),
                 fluidRow(
                     plotlyOutput("sirCumulativeState")
                 )
                 ), 
        tabPanel("Counties",
                 fluidRow(
                     column(3, 
                            selectInput("state", "Select State(s)", cumulativeStates$state.x, multiple = FALSE, selected = "Arizona"),
                            selectInput("county", "Select Counties(s)", selectedState$county.x, multiple = TRUE),
                            selectInput("cases_deaths", "Select Outcome", choices = c("Cases" = "cases", "Deaths" = "deaths", selected = "Cases"))
                     ), 
                     column(9, 
                            plotlyOutput("mapCumulativeState")
                     ))), 
        tabPanel("Data and Methodology")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$rawCases <- renderText({cumulativeUS$cases})
    output$cumuCases <- renderText({round(cumulativeUS$caseRate,0)})
    output$rawDeaths <- renderText({cumulativeUS$deaths})
    output$cumuDeaths <- renderText({round(cumulativeUS$deathRate,0)})
    output$UsCaseMap <- renderPlotly(mapStatesCases)
    output$UsDeathMap <- renderPlotly(mapStatesDeaths)
    
    covidState <- reactive(covidStates %>%
        filter(state.x == input$state))
    
   cumulativeState <-  reactive({cumulativeStates %>%
        filter(state.x == input$state)})
   
   sirStates <- sir_states(cumulativeStates, cumulativeUS)
   
   #sirStatesR <- reactive({sirStates %>%
           #filter(state.x == input$state)})
   
   output$mapCumulativeState <- renderPlotly(ggplotly(ggplot(data = covidState(), aes(x = date)) + 
                                                          geom_line(aes(y = cases, color = state.x), size = 0.75) + 
                                                          theme_minimal()))
   
  
   output$sirCumulativeState <- renderPlotly(ggplotly(ggplot(data = sirStates, aes(y = (sir-1), x = reorder(state.x, sir), fill = typeC)) + 
                                                          geom_bar(stat = "identity", position = dodge) + 
                                                          geom_errorbar(aes(ymax = (usir - 1), ymin = (lsir - 1)), position = dodge, width = 0.25) + 
                                                          xlab("State") + 
                                                          ylab("SIR for cases") + 
                                                          labs(fill = "Type") + 
                                                          theme_minimal() + 
                                                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
                                                          scale_fill_manual(values = custom) + 
                                                          theme(legend.position = "none")))
}

# Run the application 
shinyApp(ui = ui, server = server)
