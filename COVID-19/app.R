
library(shiny)
library(tidyverse)
library(lubridate)
library(imputeTS)
library(car)
library(plotly)
library(RSelenium)
library(shinysky)
source("covid_functions.r")
reactiveConsole(TRUE)


#--------------Static Information-----------------
#read in data -- static 
#covid_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
#covid_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

covid_counties <- read_csv("covid_counties.csv")
covid_states <- read_csv("covid_states.csv")

covid_states <- covid_states %>%
    filter(state != 'American Samoa' & 
               state != 'Northern Mariana Islands' & 
               state != 'Virgin Islands' & 
               state != 'Puerto Rico' & 
               state != 'Guam')



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
    titlePanel(wellPanel(tags$h1("COVID-19: An Overview in the United States"), style = "background: #aec3b0")),
    busyIndicator(text = "Please wait ... ", wait = 0),
    includeCSS("www/colorTheme.css"),
    
    tabsetPanel(
            
            #----------TAB: United States-----------------
            #information on the US -- cases and deaths choropleth  map, total cases and deaths, total cases and deaths per 100,000
        tabPanel(h3("United States"),
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
                 ), 
                 fluidRow(
                     wellPanel(style = "background: #aec3b0",
                     h4("For accurate information on the COVID-19 pandemic, please visit the website of your local health department or government resource,
                        such the CDC.", align = "center"),
                     tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/index.html", tags$h4(tags$u("CDC.gov: COVID-19"), style = "color: blue"), align = "center"),
                     h4("Use the following link to find the nearest COVID-19 vaccination provider.", align = "center"),
                     tags$a(href = "https://www.vaccines.gov/", tags$h4(tags$u("Vaccines.gov"), style = "color: blue"), align = "center")
                 ))
    
    
        ), 
        
            #------------TAB: States------------------------------------
            #information on states, select states, select outcomes, time series map, sir graph 
        tabPanel(h3("States"), 
                 fluidRow(
                     column(3,
                                #get input for time series map 
                            selectInput(inputId = "state", 
                                        label = tags$h5("Select State(s)"), 
                                        choices = cumulativeStates$state.x, 
                                        multiple = TRUE, 
                                        selected = "South Dakota"),
                                #get input for outcome type 
                            selectInput(inputId = "cases_deaths", 
                                        label = tags$h5("Select Outcome"), 
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
                            tags$p("Standarized incidence ratios compared the observed value to the expected
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
        
            #---------TAB: Counties------------------------------------
            #information on counties, select one state, select countites, select outcome, time series plot, sir graph 
        tabPanel(h3("Counties"),
                 fluidRow(
                     column(3, 
                             selectInput(inputId = "state4Counties", 
                                         label = tags$h5("Select State"), 
                                         choices = cumulativeStates$state.x, multiple = FALSE, 
                                         selected = "South Dakota"),
                            uiOutput("counties"), 
                            selectInput(inputId = "c_d", 
                                        label = tags$h5("Select Outcome"), 
                                        choices = c("Cases" = 1, "Deaths" = 0), 
                                        selected = "Cases")
                     ), 
                     column(9,
                            plotlyOutput("mapTsCounty") 
                           
                     )),
                 fluidRow(
                     column(3, 
                            #?? better way to do this ??
                            h4("What are Standardized incidence ratios?"),
                            tags$br(), 
                            tags$p("Standarized incidence ratios compared the observed value to the expected
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
        
            #--------TAB: Data and Methods--------------------
        tabPanel(h3("Data and Methodology"),
                 fluidRow(
                     column(6,
                            h2("Data Sources", align = "center"),
                            h4("COVID-19 Data"),
                            tags$p("The COVID-19 data was taken from The New York Times Git-Hub page, https://github.com/nytimes/covid-19-data. 
                               The data included the daily cumulative number of cases and deaths reported in each county and state as reported by local health agencies
                               since the beginning of the pandemic in the United States, January 21, 2020. The counts are updated each day and relect the final numbers as 
                               reported from the previous day."), 
                            h4("Population Data"), 
                            h6("The population data by states and counties was provided by the United State Census Bureau based on 2019 estimates.")
                            ), 
                     column(6,
                            h2("Methodology", align = "center"),
                            h4("Age-adjustment"),
                            h4("SIR Values")
                            
                            )
                 ))
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
         filter(state.x %in% input$state))
 
        #filter cumulative counts for SIR values 
    cumulativeState <-  reactive({cumulativeStates %>%
         filter(state.x %in% input$state)})
        
        #create SIR values from all states 
    sirStates <- sir_states(cumulativeStates, cumulativeUS)
    
        #---------~PLOT: State time series-------------------------
    ts_plot <- reactive({
        if (input$cases_deaths == 1) return(ggplotly(ggplot(data = covidState(),
                                                            aes(x = date)) +
                                                         geom_line(aes(y = cases, color = state.x), size = 0.75) +
                                                         labs(title = tags$h3("Cumulative cases per 100,000 by selected state"), color = "State")+
                                                         xlab("Date") + 
                                                         ylab("Cumulative cases per 100,000") + 
                                                         theme_minimal() + 
                                                         scale_color_brewer(palette = "Spectral") 
                                                     
        ))
        if (input$cases_deaths == 0) return(ggplotly(ggplot(data = covidState(),
                                                            aes(x = date)) +
                                                         geom_line(aes(y = deaths, color = state.x), size = 0.75) +
                                                         labs(title = "Cumulative deaths per 100,000 by selected state", color = "State") + 
                                                         xlab("Date") + 
                                                         ylab("Cumulative deaths per 100,000") + 
                                                         theme_minimal() + 
                                                         scale_color_brewer(palette = "Spectral")))
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
                    choices = cumulativeCounties[cumulativeCounties$state.x == input$state4Counties, "county.y"], 
                    multiple = TRUE, 
                    selected = "Brookings County")
                    
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
    
    
        #--------~PLOT: County time series--------------------
    
    #filter daily counts for time series plot 
    # tsCounty <- reactive({covidCounties %>%
    #         dplyr::filter(county.x %in% input$choose_counties)})
    
    tsCounty <- reactive({covidCounties %>%
            dplyr::filter(state.x %in% input$state4Counties) %>%
            dplyr::filter(county.x %in% input$choose_counties)})
    
    
    tsC_plot <- reactive({
        if (input$c_d == 1) return(ggplotly(ggplot(data = tsCounty(),
                                                            aes(x = date)) +
                                                geom_line(aes(y = cases, color = county.x), size = 0.75) +
                                                         labs(title = "Cumulative cases per 100,000 by selected counties", color = "County")+
                                                         xlab("Date") +
                                                         ylab("Cumulative cases per 100,000") +
                                                         theme_minimal() + 
                                                scale_color_brewer(palette = "Spectral")
                                                     
        ))
        if (input$c_d == 0) return(ggplotly(ggplot(data = tsCounty(),
                                                            aes(x = date)) +
                                                         geom_line(aes(y = deaths, color = county.x), size = 0.75) +
                                                         labs(title = "Cumulative deaths per 100,000 by selected counties", color = "County") + 
                                                         xlab("Date") + 
                                                         ylab("Cumulative deaths per 100,000") + 
                                                         theme_minimal()+ 
                                                scale_color_brewer(palette = "Spectral")))
    })
    
    output$mapTsCounty <- renderPlotly({
        dataplots = tsC_plot()
        print(dataplots)
    })
    
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
