library(shiny)
library(RPostgreSQL)
library(DBI)
library(dplyr)

ui  <- fluidPage(
    actionButton(
        inputId = 'connect',
        label = 'Connect!'
    ),
    verbatimTextOutput("dbOutput")
)

server  <- function(input, output, session) {
    drv <- dbDriver("PostgreSQL")
    observeEvent(input$connect, {
        conn <- dbConnect(
            drv, host='130.95.204.63', port='5433', dbname='scevo',
            user='dpaw_write', password='dpaw_write'
        )
        selectedSites  <-  "sensor_repository_00234"
        sensorData  <- as.data.frame(tbl(conn, selectedSites))
        output$dbOutput <- renderPrint({
            head(sensorData)
        })
        dbDisconnect(conn)
    })
}

shinyApp(ui = ui, server = server)