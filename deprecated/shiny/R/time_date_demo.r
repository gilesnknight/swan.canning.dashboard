library(shiny)
library(RPostgreSQL)
library(DBI)
library(magrittr)
library(dplyr)

ui  <- fluidPage(
    titlePanel("R PostgreSQL"),
    column(
        6,
        dateInput(
            inputId = "startDate",
            label = "Start Date",
            value = "2020-01-01",
            width = '100%'
        ),
        dateInput(
            inputId = "endDate",
            label = "End Date",
            value = "2021-01-01",
            width = '100%'
        ),
        actionButton("fetchData", "Fetch Data", width = '100%'),
        verbatimTextOutput("dateOutput")
    ),
    column(
        6,
        verbatimTextOutput("outputData")

    )
)

server <- function(input, output, session) {

    dateListen <- reactive({
        list(input$startDate,input$endDate)
    })
    observeEvent(dateListen(), {
        if(is.na(input$startDate) && is.na(input$endDate)){
            return()
        } else {
            julian.Date("2020-01-01")
            output$dateOutput <- renderPrint(paste0("Start Date: ", input$startDate, "  ", "End Date: ", input$endDate))
        }
    })


    observeEvent(input$fetchData, {
        drv <- dbDriver("PostgreSQL")
        conn <- dbConnect(
                    drv, host='130.95.204.63', port='5433', dbname='scevo',
                    user='dpaw_write', password='dpaw_write'
                )
        sensorData  <- tbl(conn, "sensor_repository_00748") %>% as.data.frame()

        output$outputData <- renderPrint({
            head(sensorData)
        })

        dbDisconnect(conn)
    })



}

shinyApp(ui = ui, server = server)