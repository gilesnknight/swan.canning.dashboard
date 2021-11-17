# Load packages
library(shiny)
library(bslib)
library(RPostgreSQL)
library(DBI)
library(magrittr)
library(dplyr)
library(ggplot2)
library(thematic)
library(patchwork)


# Define UI
ui <- navbarPage(
    theme = bs_theme(bootswatch = "minty"),
    title = "SCEVO",
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "site",
                "Select site:",
                c(
                    "Bickley"="sensor_repository_00038",
                    #"Champion Lakes"="sensor_repository_00143",
                    "Dalwallinu"="sensor_repository_00199",
                    "Dwellingup"="sensor_repository_00220",
                    "Garden Island" = "sensor_repository_00248",
                    "Gingin Airport" = "sensor_repository_00290",
                    "Jandakot" = "sensor_repository_00360",
                    "Lake Grace" = "sensor_repository_00416",
                    "Manjimup" = "sensor_repository_00458",
                    "Perth Airport" = "sensor_repository_00592",
                    "Perth Metro" = "sensor_repository_00620",
                    "Rottnest" = "sensor_repository_00648",
                    "Southern Cross Airport" = "sensor_repository_00662",
                    "Swanbourne" = "sensor_repository_00697",
                    "Wandering" = "sensor_repository_00711",
                    "Chunderin Airport" = "sensor_repository_00178"
                )
            ),
            sliderInput("dateInput",
                    "Dates:",
                    min=as.Date("2020-01-01","%Y-%m-%d"),
                    max=as.Date("2021-12-22","%Y-%m-%d"),
                    value=c(as.Date("2020-01-01"), as.Date("2021-12-22")),
                    timeFormat="%Y-%m-%d",
                    width = '100%',
                    animate = animationOptions(1000)
        )
        ),
        mainPanel(
            plotOutput('plot')
        )
    )

)

# Define server function
server <- function(input, output, session) {
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(
        drv, host='130.95.204.63', port='5433', dbname='scevo',
        user='dpaw_write', password='dpaw_write'
        )

    observeEvent(input$site,{
        sensor_data <- tbl(conn, input$site) %>% as.data.frame()

        sensor_data$datetime  <- as.POSIXlt(sensor_data$st_feed_date_jdn*86400,
                                    origin=structure(-210866760000,
                                                     class=c("POSIXct", "POSIXt"),
                                                     tzone="Australia/Perth"),
                                    tz="Australia/Perth")
        data  <- sensor_data

        updateSliderInput(session, "dateInput", label = "Dates:",
            min = min(as.Date(data$datetime)), max = max(as.Date(data$datetime)),
            value = c(max(as.Date(data$datetime))-7, max(as.Date(data$datetime)))
            )

        output$plot  <- renderPlot({
            data <-  dplyr::filter(data,datetime >= as.POSIXct(input$dateInput[1]),datetime <= as.POSIXct(input$dateInput[2]))
            p1  <- ggplot(data, aes(x=as.POSIXct(datetime), y=st_value_1)) +
            geom_line() +
            xlab("Date") +
            ylab("Air Temperature") +
            theme_light(base_size = 14)
            p2  <- ggplot(data=data,aes(x=1, y=st_value_1))+
                    scale_y_continuous(position = "right") +
                    geom_violin()+
                    stat_summary(fun = "mean", geom = "point",
                                colour = "black")+
                    theme_light(base_size = 14)+
                    theme(
                        axis.text.x = element_blank(),
                        axis.title.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.line.x = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        axis.title.y = element_blank()
                    )
            p1+p2+plot_layout(widths = c(5, 1))
        })
    })

    # observeEvent(input$dateInput,(
    #     print(input$dateInput)
    # ))

}

# Create Shiny object

shinyApp(ui = ui, server = server)


