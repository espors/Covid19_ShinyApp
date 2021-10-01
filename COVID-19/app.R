
library(shiny)
library(tidyverse)
library(lubridate)
library(imputeTS)
library(car)
library(plotly)
source("covid_functions.r")
reactiveConsole(TRUE)

#read in data -- static 
#covid_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
#covid_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

covid_counties <- read_csv("covid_counties.csv")
covid_states <- read_csv("covid_states.csv")

county_population <- read_csv("county_population.csv")
state_population <- read_csv("state_population.csv")
united_states_pop <- read_csv("united_states_pop.csv")

#create new data sets -- static 
covidCounties <- covid_rates(covid_counties, county_population)
covidStates <- covid_rates(covid_states, state_population)
covidUs <- us_rates(covid_states, united_states_pop)
cumulativeCounties <- cumulative_county(covid_counties, county_population)
cumulativeStates <- cumulative_state(covid_states, state_population)
cumulativeUS <- cumulative_us(covid_states, united_states_pop)

#define map information -- static 
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

#formatting settings -- static 
dodge <- position_dodge(width = 0.9)
custom <- c("lightslategrey", "darkslategrey")


ui <- fluidPage(
    titlePanel("COVID-19: An Overview in the United States"),
    tabsetPanel(
        
            #information on the US -- cases and deaths choropleth  map, total cases and deaths, total cases and deaths per 100,000
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
                            )
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
        
            #information on states, select states, select outcomes, time series map, sir graph 
        tabPanel("States", 
                 fluidRow(
                     column(3, 
                                #get input for time series map 
                            selectInput(inputId = "state", 
                                        label = "Select State(s)", 
                                        choices = cumulativeStates$state.x, 
                                        multiple = TRUE, 
                                        selected = "South Dakota"),
                                #get input for outcome type 
                            selectInput(inputId = "cases_deaths", 
                                        label = "Select Outcome", 
                                        choices = c("Cases" = 1, "Deaths" = 0), 
                                        selected = "Cases")
                            ), 
                     column(9, 
                            plotlyOutput("mapTsState")
                            )),
                 fluidRow(
                     column(3, 
                                #?? better way to do this ??
                            h4("What are Standardized incidence ratios?"),
                            tags$br(), 
                            h5("Standarized incidence ratios compared the observed value to the expected
                               value, based on a reference population. For this, the observed value was
                               the cumulative rate per 100,000 and the expected value was the cumulative 
                               rate per 100,000 in the United States. Values that are greater than one 
                               indicate that that state saw a higher rate of a particular outcome than expected. 
                               Values that are less than one indicate the that state saw a lower rate
                               of a particular out than expected. For ease of intpretation, the values are 
                               presented as 1-SIR. If a confidence interval contains zero, then that 
                               state is not significantly different than expected.")
                            
                            ), 
                     column(9, 
                            plotlyOutput("sirCumulativeState")
                            )
                     
                 )
                 ), 
        
            #information on counties, select one state, select countites, select outcome, time series plot, sir graph 
        tabPanel("Counties",
                 fluidRow(
                     column(3, 
                             selectInput(inputId = "state4Counties", 
                                         label = "Select State", 
                                         choices = cumulativeStates$state.x, multiple = FALSE, 
                                         selected = "Arizona"),
                            uiOutput("counties"), 
                            selectInput(inputId = "c_d", 
                                        label = "Select Outcome", 
                                        choices = c("Cases" = 1, "Deaths" = 0), 
                                        selected = "Cases")
                     ), 
                     column(9
                            #plotlyOutput("mapCumulativeState")
                     )),
                 fluidRow(
                     column(3, 
                            #?? better way to do this ??
                            h4("What are Standardized incidence ratios?"),
                            tags$br(), 
                            h5("Standarized incidence ratios compared the observed value to the expected
                               value, based on a reference population. For this, the observed value was
                               the cumulative rate per 100,000 and the expected value was the cumulative 
                               rate per 100,000 in the United States. Values that are greater than one 
                               indicate that that state saw a higher rate of a particular outcome than expected. 
                               Values that are less than one indicate the that state saw a lower rate
                               of a particular out than expected. For ease of intpretation, the values are 
                               presented as 1-SIR. If a confidence interval contains zero, then that 
                               state is not significantly different than expected.")
                            
                     ), 
                     column(9, 
                            plotlyOutput("sirCumulativeCounty")
                     )
                     
                 )), 
        
        tabPanel("Data and Methodology")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #----------TAB: United States------------------------------------
    output$rawCases <- renderText({cumulativeUS$cases})
    output$cumuCases <- renderText({round(cumulativeUS$caseRate,0)})
    output$rawDeaths <- renderText({cumulativeUS$deaths})
    output$cumuDeaths <- renderText({round(cumulativeUS$deathRate,0)})
    output$UsCaseMap <- renderPlotly(mapStatesCases)
    output$UsDeathMap <- renderPlotly(mapStatesDeaths)
     
    
    #--------TAB: States--------------------------------------------
    
        #filter daily counts for time series plot 
    covidState <- reactive(covidStates %>%
         filter(state.x == input$state))
 
        #filter cumulative counts for SIR values 
    cumulativeState <-  reactive({cumulativeStates %>%
         filter(state.x == input$state)})
        
        #create SIR values from all states 
    sirStates <- sir_states(cumulativeStates, cumulativeUS)
    
        #---------~PLOT: State time series-------------------------
    ts_plot <- reactive({
        if (input$cases_deaths == 1) return(ggplotly(ggplot(data = covidState(),
                                                            aes(x = date)) +
                                                         geom_line(aes(y = cases, color = state.x), size = 0.75) +
                                                         labs(title = "Cumulative cases per 100,000 by selected state", color = "State")+
                                                         xlab("Date") + 
                                                         ylab("Cumulative cases per 100,000") + 
                                                         theme_minimal() 
                                                     
        ))
        if (input$cases_deaths == 0) return(ggplotly(ggplot(data = covidState(),
                                                            aes(x = date)) +
                                                         geom_line(aes(y = deaths, color = state.x), size = 0.75) +
                                                         labs(title = "Cumulative deaths per 100,000 by selected state", color = "State") + 
                                                         xlab("Date") + 
                                                         ylab("Cumulative deaths per 100,000") + 
                                                         theme_minimal()))
    })
    
    output$mapTsState <- renderPlotly({
        dataplots = ts_plot()
        print(dataplots)
    })
    

        #--------~PLOT: State sir values----------------------
    sir_plot <- reactive({
        if (input$cases_deaths == 1) return(ggplotly(ggplot(data = sirStates, aes(y = (sir-1), x = reorder(state.x, sir), fill = typeC)) + 
                                                         geom_bar(stat = "identity", position = dodge) +
                                                         geom_errorbar(aes(ymax = (usir - 1), ymin = (lsir - 1)), position = dodge, width = 0.25) +
                                                         xlab("State") +
                                                         ylab("1-SIR for cases") +
                                                         labs(fill = "Type") +
                                                         theme_minimal() +
                                                         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                                         scale_fill_manual(values = custom) +
                                                         theme(legend.position = "none")))
        if (input$cases_deaths == 0) return(ggplotly(ggplot(data = sirStates, aes(y = (sdr-1), x = reorder(state.x, sdr), fill = typeD)) + 
                                                         geom_bar(stat = "identity", position = dodge) +
                                                         geom_errorbar(aes(ymax = (usdr - 1), ymin = (lsdr - 1)), position = dodge, width = 0.25) +
                                                         xlab("State") +
                                                         ylab("1-SIR for deaths") +
                                                         labs(fill = "Type") +
                                                         theme_minimal() +
                                                         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                                         scale_fill_manual(values = custom) +
                                                         theme(legend.position = "none")))
    })

   
    output$sirCumulativeState <- renderPlotly({
        dataplots = sir_plot()
        print(dataplots)
        })
    
    #-------TAB: Counties-----------------------------
    
        #create select input that is based off of previous select input 
    output$counties <- renderUI({
        selectInput(inputId = "choose_counties", 
                    label = "Select Counties", 
                    choices = cumulativeCounties[cumulativeCounties$state.x == input$state4Counties, "county.x"], 
                    multiple = TRUE)
                    
    })
    
    
    sirCounties <- reactive({sir_counties(cumulativeCounties[cumulativeCounties$state.x == input$state4Counties,], 
                                  cumulativeStates[cumulativeStates$state.x == input$state4Counties,])})
        
        #--------~PLOT: County sir values----------------
    sirC_plot <- reactive({
        if (input$c_d == 1) return(ggplotly(ggplot(data = sirCounties(), aes(y = (sir-1), x = reorder(county.x, sir), fill = typeC)) + 
                                                             geom_bar(stat = "identity", position = dodge) +
                                                             geom_errorbar(aes(ymax = (usir - 1), ymin = (lsir - 1)), position = dodge, width = 0.25) +
                                                             xlab("County") +
                                                             ylab("1-SIR for cases") +
                                                             labs(fill = "Type") +
                                                             theme_minimal() +
                                                             theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                                             scale_fill_manual(values = custom) +
                                                             theme(legend.position = "none")))
        if (input$c_d == 0) return(ggplotly(ggplot(data = sirCounties(), aes(y = (sdr-1), x = reorder(county.x, sdr), fill = typeD)) + 
                                                             geom_bar(stat = "identity", position = dodge) +
                                                             geom_errorbar(aes(ymax = (usdr - 1), ymin = (lsdr - 1)), position = dodge, width = 0.25) +
                                                             xlab("County") +
                                                             ylab("1-SIR for deaths") +
                                                             labs(fill = "Type") +
                                                             theme_minimal() +
                                                             theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                                             scale_fill_manual(values = custom) +
                                                             theme(legend.position = "none")))
    })
        
    output$sirCumulativeCounty <- renderPlotly({
            dataplots = sirC_plot()
            print(dataplots)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
