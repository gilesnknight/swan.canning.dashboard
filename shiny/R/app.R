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
library(bslib)
library(leaflet)
library(leaflet.extras2)
library(lubridate)


# Define UI
ui <-
    navbarPage(
        theme = bs_theme(bootswatch = "litera"),
        title = "SCEVO",
        tabPanel(
            "Home",
            #sidebarPanel(),
            leafletOutput("homeMap", height = 600)
        ),
        tabPanel(
            "Temperature",
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
                    plotOutput('plot',height = "100%")
                )
            )
        ),
        tabPanel(
            "Flow",
            sidebarLayout(
                sidebarPanel(
                    checkboxGroupInput(
                        inputId = "flowSites",
                        label = "Select flow sites:",
                        choices = c(
                            "Walyunga" = "sensor_repository_00748",
                            "Benara Road" = "sensor_repository_00756",
                            "Forrest Highway" = "sensor_repository_00764",
                            "GT Northern Bridge" = "sensor_repository_00768",
                            "Gull Road" = "sensor_repository_00772",
                            "Kielman" = "sensor_repository_00780",
                            "National Park" = "sensor_repository_00784",
                            "Old Bunbury Road" = "sensor_repository_00788",
                            "Pinjarra" = "sensor_repository_00792",
                            "River Road" = "sensor_repository_00796",
                            "Railway Parade" = "sensor_repository_00800",
                            "Seaforth" = "sensor_repository_00804",
                            "Whiteman Road" = "sensor_repository_00812",
                            "Yackaboon" = "sensor_repository_00816"
                        )
                    ),
                    actionButton("plotFlowSites", "Plot sites"),
                    sliderInput("flowDateInput",
                            "Filter dates:",
                            min=as.Date("2019-01-01","%Y-%m-%d"),
                            max=as.Date("2021-12-22","%Y-%m-%d"),
                            value=c(as.Date("2020-01-01"), as.Date("2021-12-22")),
                            timeFormat="%Y-%m-%d",
                            width = '100%',
                            animate = animationOptions(1000)
                )
                ),
                mainPanel(
                    plotOutput('flowPlot',height = "100%")
                )
            )
        ),
        tabPanel(
            "Cumulative",
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = "cumulativeSite",
                        label = "Select a site:",
                        choices = c(
                            "Walyunga" = "sensor_repository_00748",
                            "Benara Road" = "sensor_repository_00756",
                            "Forrest Highway" = "sensor_repository_00764",
                            "GT Northern Bridge" = "sensor_repository_00768",
                            "Gull Road" = "sensor_repository_00772",
                            "Kielman" = "sensor_repository_00780",
                            "National Park" = "sensor_repository_00784",
                            "Old Bunbury Road" = "sensor_repository_00788",
                            "Pinjarra" = "sensor_repository_00792",
                            "River Road" = "sensor_repository_00796",
                            "Railway Parade" = "sensor_repository_00800",
                            "Seaforth" = "sensor_repository_00804",
                            "Whiteman Road" = "sensor_repository_00812",
                            "Yackaboon" = "sensor_repository_00816"
                        )
                    ),
                    actionButton("plotCumulativeSites", "Plot site"),
                    sliderInput("cumulativeDateInput",
                            "Filter dates:",
                            min=as.Date("0000-01-01","%Y-%m-%d"),
                            max=as.Date("0000-12-22","%Y-%m-%d"),
                            value=c(as.Date("0000-01-01"), as.Date("0000-12-22")),
                            timeFormat="%m-%d",
                            width = '100%',
                            animate = animationOptions(1000)
                )
                ),
                mainPanel(
                    plotOutput('cumulativePlot',height = "100%")
                )
            )
        )
    )


# Define server function
server <- function(input, output, session) {
    drv <- dbDriver("PostgreSQL")

    # HOME
    content <- "https://raw.githubusercontent.com/danwild/leaflet-velocity/master/demo/wind-gbr.json"
    output$homeMap <- leaflet::renderLeaflet({
        leaflet::leaflet() %>%
        addTiles(group = "base") %>%
        leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
        setView(145, -20, 4) %>%
        addVelocity(content = content, group = "velo", layerId = "veloid") %>%
        addLayersControl(baseGroups = "base", overlayGroups = "velo")
        # leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
        # attribution = "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community") %>%
        # leaflet::setView(lng = 115.844759,
        #                 lat = -31.99907,
        #                 zoom = 11)
    })

    # TEMPERATURE
    observeEvent(input$site,{
         conn <- dbConnect(
            drv, host='130.95.204.63', port='5433', dbname='scevo',
            user='dpaw_write', password='dpaw_write'
        )
        sensor_data <- tbl(conn, input$site) %>% as.data.frame()
        dbDisconnect(conn)
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

    # FLOW
    observeEvent(input$plotFlowSites,{
        conn <- dbConnect(
            drv, host='130.95.204.63', port='5433', dbname='scevo',
            user='dpaw_write', password='dpaw_write'
        )
        selectedSites  <- input$flowSites
        print(selectedSites)
        allData  <- data.frame(
            "st_sensor_code" = integer(),
            "st_value_1" = double(),
            "datetime" = as.POSIXlt(character())
        )
        for (i in selectedSites) {
            sensorData  <- tbl(conn, i) %>% as.data.frame()

            sensorData$datetime  <- as.POSIXlt(sensorData$st_feed_date_jdn*86400,
                                origin=structure(-210866760000,
                                                    class=c("POSIXct", "POSIXt"),
                                                    tzone="Australia/Perth"),
                                tz="Australia/Perth")
            sensorData  <- sensorData[,c("st_sensor_code","st_value_1","datetime")]
            allData <- rbind(allData, sensorData)
        }
        dbDisconnect(conn)
        # updateSliderInput(session, "flowDateInput", label = "Filter dates::",
        #     min = min(as.Date(allData$datetime)), max = max(as.Date(allData$datetime))
        #     value = c(min(as.Date(allData$datetime)), max(as.Date(allData$datetime)))
        #     )
        output$flowPlot  <- renderPlot({
            allData <-  dplyr::filter(allData,datetime >= as.POSIXct(input$flowDateInput[1]),datetime <= as.POSIXct(input$flowDateInput[2]))
            ggplot(allData, aes(x=as.POSIXct(datetime), y=st_value_1, fill = as.character(st_sensor_code))) +
             geom_area() +
             scale_x_datetime(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0)) +
             xlab("Date") +
             ylab("Flow (m3/s)") +
             theme_light(base_size = 14)
        })
    })

    # CUMULATIVE
    observeEvent(input$cumulativeDateInput,{
        print(input$cumulativeDateInput)
    })
    observeEvent(input$plotCumulativeSites,{
        conn <- dbConnect(
            drv, host='130.95.204.63', port='5433', dbname='scevo',
            user='dpaw_write', password='dpaw_write'
        )
        selectedSite  <- input$cumulativeSite #"sensor_repository_00748"
        print(selectedSite)
        sensorData  <- tbl(conn, selectedSite) %>% as.data.frame()
        dbDisconnect(conn)
        sensorData$datetime  <- as.POSIXlt(sensorData$st_feed_date_jdn*86400,
                                origin=structure(-210866760000,
                                                    class=c("POSIXct", "POSIXt"),
                                                    tzone="Australia/Perth"),
                                tz="Australia/Perth")
        sensorData  <- sensorData[,c("st_sensor_code","st_value_1","datetime")]
        sensorData$year <- format(sensorData$datetime, format="%Y")
        sensorData <- mutate(group_by(sensorData,year), csum=cumsum(st_value_1))
        #sensorData %>% group_by(year) %>% mutate(csum = cumsum(st_value_1))
        sensorData$x  <- sensorData$datetime
        year(sensorData$x)  <- 0
        output$cumulativePlot  <- renderPlot({
            sensorData <-  dplyr::filter(sensorData,as.POSIXct(x) >= ymd(input$cumulativeDateInput[1]),as.POSIXct(x) <= ymd(input$cumulativeDateInput[2]))
            #print(head(sensorData))
            ggplot(sensorData, aes(x=as.POSIXct(x), y=csum, color = as.character(year))) +
             geom_line() +
             scale_x_datetime(expand = c(0,0), date_labels = "%e %b") +
             scale_y_continuous(expand = c(0,0)) +
             xlab("Date") +
             ylab("Cumulative flow (m3/s)") +
             theme_light(base_size = 14)
        })
    })
}

# Create Shiny object
shinyApp(ui = ui, server = server)





