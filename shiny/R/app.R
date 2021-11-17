# Load packages
library(shiny)
library(bslib)
library(RPostgreSQL)
library(DBI)
library(magrittr)
library(dplyr)
library(ggplot2)
library(thematic)

#setwd('Documents/GitHub/swan.canning.dashboard/shiny/R/')



# Define UI
ui <- navbarPage(
    theme = bs_theme(bootswatch = "solar"),
    title = "SCEVO",
    sidebarPanel(
        selectInput(
            "site",
            "Select site:",
            c("Matilda")
        ),
        dateRangeInput('dateRange',
                   label = 'Filter by date',
                   start = as.Date("2021-10-15") , end = as.Date("2021-10-15")
    ),
        sliderInput("dateInput",
                "Dates:",
                min=as.Date("2016-08-29","%Y-%m-%d"),
                max=as.Date("2020-12-22","%Y-%m-%d"),
                value=c(as.Date("2016-08-29"), as.Date("2020-12-22")),
                timeFormat="%Y-%m-%d",
                width = '100%',
                animate = animationOptions(1000)
    )
    ),
    mainPanel(
        plotOutput('plot')
    )

)

# Define server function
server <- function(input, output) {
    thematic_shiny(font = "auto")
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(
        drv, host='130.95.204.63', port='5433', dbname='scevo',
        user='dpaw_write', password='dpaw_write'
        )
    sensor_data <-tbl(conn, "sensor_repository_01000") %>% as.data.frame()
    #sensor_data  <- reactive(subset(sensor_data, datestamp> "2021-10-15" & datestamp "2021-10-15"))
   # print(head(sensor_data))
    output$plot  <- renderPlot({
        #req(sensor_data())
        sensor_data <-  dplyr::filter(sensor_data,datestamp >= as.Date(input$dateInput[1]),datestamp <= as.Date(input$dateInput[2]))
        #sensor_data <- sensor_data %>% filter(datestamp >= input$dateRange[1] & datestamp <= input$dateRange[2])
        ggplot(sensor_data, aes(x=datestamp, y=st_value_1)) +
        geom_line() +
        xlab("Time") +
        ylab("Value")
    })
    observeEvent(input$startDate,(
        print(input$startDate)
    ))

}

# Create Shiny object
thematic_shiny()
shinyApp(ui = ui, server = server)

#bootswatch_themes(version = version_default(), full_path = FALSE)
