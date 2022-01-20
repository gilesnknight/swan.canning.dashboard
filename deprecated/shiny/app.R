# Load packages
library(shiny)
library(bslib)
library(RPostgreSQL)
library(DBI)
library(magrittr)
library(dplyr)

setwd('Documents/GitHub/swan.canning.dashboard/shiny/R/')

# Define UI
ui <- navbarPage(
    theme = bs_theme(bootswatch = "minty"),
    title = "SCEVO"

)

# Define server function
server <- function(input, output) {
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(
        drv, host='130.95.204.63', port='5433', dbname='scevo',
        user='dpaw_write', password='dpaw_write'
        )
    Sensors<-tbl(conn, "sensors") %>% as.data.frame()
    print(str(Sensors))

}

# Create Shiny object
shinyApp(ui = ui, server = server)

#bootswatch_themes(version = version_default(), full_path = FALSE)
