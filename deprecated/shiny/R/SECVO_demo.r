# Load packages
library(bslib)
library(RPostgreSQL)
library(DBI)
library(shiny)
library(dplyr)
library(ggplot2)
library(thematic)
library(patchwork)
library(magrittr)
library(bslib)
library(leaflet)
library(htmltools)
library(lubridate)
library(sf)
library(shinyWidgets)


# Define UI
ui <-
    navbarPage(
        theme = bs_theme(bootswatch = "litera"),
        title = "SCEVO",
        tabPanel(
            "Hydrology",
            icon = icon("water"),
            fluidRow(
                column(4,
                    leafletOutput("hydroMap", height = '100%')
                ),
                column(8,
                    tabsetPanel(type = "tabs",

                        tabPanel(
                            "Flow",
                            plotOutput("hydroFlowPlot", height = "500px"),
                            column(12,
                                fluidRow(
                                    checkboxGroupInput(
                                        inputId = "flowSites",
                                        label = "Select flow sites:",
                                        inline = TRUE,
                                        choiceNames = list(
                                            span(icon("circle", style = "color:#66c2a5;"), " Avon"),
                                            span(icon("circle", style = "color:#fc8d62;"), " Bennett Brook"),
                                            span(icon("circle", style = "color:#8da0cb;"), " Upper Swan"),
                                            span(icon("circle", style = "color:#e78ac3;"), " Jane Brook"),
                                            span(icon("circle", style = "color:#a6d854;"), " Susannah Brook"),
                                            span(icon("circle", style = "color:#ffd92f;"), " Ellen Brook"),
                                            span(icon("circle", style = "color:#e5c494;"), " Canning River"),
                                            span(icon("circle", style = "color:#b3b3b3;"), " Helena River")
                                            # span(icon("circle", style = "color:#66c2a5;"), " Walyunga"),
                                            # span(icon("circle", style = "color:#fc8d62;"), " Benara Road"),
                                            # span(icon("circle", style = "color:#8da0cb;"), " GT Northern Highway"),
                                            # span(icon("circle", style = "color:#e78ac3;"), " National Park"),
                                            # span(icon("circle", style = "color:#a6d854;"), " River Road"),
                                            # span(icon("circle", style = "color:#ffd92f;"), " Railway Parade"),
                                            # span(icon("circle", style = "color:#e5c494;"), " Seaforth"),
                                            # span(icon("circle", style = "color:#b3b3b3;"), " Whiteman Road")
                                        ),
                                        choiceValues = list(
                                            "sensor_repository_00748",
                                            "sensor_repository_00756",
                                            "sensor_repository_00768",
                                            "sensor_repository_00784",
                                            "sensor_repository_00796",
                                            "sensor_repository_00800",
                                            "sensor_repository_00804",
                                            "sensor_repository_00812"
                                        )
                                    )
                                ),
                                fluidRow(
                                    column(
                                        6,
                                        dateInput(
                                            "dateFrom",
                                            label = "From:"
                                        )
                                    ),
                                    column(
                                        6,
                                        dateInput(
                                            "dateTo",
                                            label = "To:"
                                        )
                                    )
                                ),
                                fluidRow(
                                    column(9,
                                        # sliderInput("flowDateInput",
                                        #     "Filter dates:",
                                        #     min=as.Date("2019-01-01","%Y-%m-%d"),
                                        #     max=as.Date("2021-12-22","%Y-%m-%d"),
                                        #     value=c(as.Date("2020-01-01"), as.Date("2021-12-22")),
                                        #     timeFormat="%Y-%m-%d",
                                        #     width = '100%',
                                        #     animate = animationOptions(1000)
                                        # )
                                        uiOutput("flowDateInput")
                                    ),
                                    column(3,
                                        tags$style(
                                            HTML(
                                                '.buttonWrapper {
                                                     margin: 0;
                                                    position: absolute;
                                                    top: 50%;
                                                    -ms-transform: translateY(-50%);
                                                    transform: translateY(-50%);
                                                    }
                                                    '
                                            )
                                        ),
                                        div(
                                            class="buttonWrapper",
                                            actionButton("plotHydroFlow", "Plot sites", width = '100%')
                                        )
                                    )
                                )
                            )
                        ),
                        # tabPanel(
                        #     "Cumulative Flow",
                        #     plotOutput("hydroCumulFlowPlot", height = "500px"),
                        #     column(12,
                        #         fluidRow(
                        #             radioButtons(
                        #                 inputId = "cumulFlowSites",
                        #                 label = "Select a site:",
                        #                 inline = TRUE,
                        #                 choiceNames = list(
                        #                     "Walyunga",
                        #                     "Benara Road",
                        #                     "GT Northern Highway",
                        #                     "National Park",
                        #                     "River Road",
                        #                     "Railway Parade",
                        #                     "Seaforth",
                        #                     "Whiteman Road"
                        #                 ),
                        #                 choiceValues = list(
                        #                     "sensor_repository_00748",
                        #                     "sensor_repository_00756",
                        #                     "sensor_repository_00768",
                        #                     "sensor_repository_00784",
                        #                     "sensor_repository_00796",
                        #                     "sensor_repository_00800",
                        #                     "sensor_repository_00804",
                        #                     "sensor_repository_00812"
                        #                 )
                        #             )
                        #         ),
                        #         fluidRow(
                        #             column(9,
                        #                 sliderInput("cumulFlowDateInput",
                        #                     "Filter dates:",
                        #                     min=as.Date("0000-01-01","%Y-%m-%d"),
                        #                     max=as.Date("0000-12-22","%Y-%m-%d"),
                        #                     value=c(as.Date("0000-01-01"), as.Date("0000-12-22")),
                        #                     timeFormat="%%m-%d",
                        #                     width = '100%',
                        #                     animate = animationOptions(1000)
                        #                 )
                        #             ),
                        #             column(3,
                        #                 tags$style(
                        #                     HTML(
                        #                         '.buttonWrapper {
                        #                              margin: 0;
                        #                             position: absolute;
                        #                             top: 50%;
                        #                             -ms-transform: translateY(-50%);
                        #                             transform: translateY(-50%);
                        #                             }
                        #                             '
                        #                     )
                        #                 ),
                        #                 div(
                        #                     class="buttonWrapper",
                        #                     actionButton("plotCumulFlow", "Plot site", width = '100%')
                        #                 )
                        #             )
                        #         )
                        #     )
                        # ),
                        tabPanel(
                            "Tide",
                            plotOutput("hydroTidePlot", height = "500px"),
                            column(12,
                                fluidRow(
                                    checkboxGroupInput(
                                        inputId = "hydroTideSites",
                                        label = "Select tide sites:",
                                        inline = TRUE,
                                        choiceNames = list(
                                            span(icon("circle", style = "color:#66c2a5;"), " Barrack St Jetty"),
                                            span(icon("circle", style = "color:#66c2a5;"), " Fremantle")
                                        ),
                                        choiceValues = list(
                                            "sensor_repository_00745",
                                            "sensor_repository_00747"
                                        )
                                    )
                                ),
                                fluidRow(
                                    column(
                                        9,
                                        sliderInput(
                                            "hydroTideDateInput",
                                            "Filter dates:",
                                            min=as.Date("2019-01-01","%Y-%m-%d"),
                                            max=as.Date("2021-12-07","%Y-%m-%d"),
                                            value=c(as.Date("2019-01-01"), as.Date("2021-12-07")),
                                            timeFormat="%Y-%m-%d",
                                            width = '100%',
                                            animate = animationOptions(1000)
                                        )
                                    ),
                                    column(
                                        3,
                                        tags$style(
                                            HTML(
                                                '.buttonWrapper {
                                                     margin: 0;
                                                    position: absolute;
                                                    top: 50%;
                                                    -ms-transform: translateY(-50%);
                                                    transform: translateY(-50%);
                                                    }
                                                    '
                                            )
                                        ),
                                        div(
                                            class="buttonWrapper",
                                            actionButton("plotHydroTide", "Plot sites", width = '100%')
                                        )
                                        )
                                    )
                                )
                            )
                        )


                )
            )
        ),
        tabPanel(
            "Weather",
            icon = icon("cloud-sun"),
            fluidRow(
                column(4,
                    leafletOutput("weatherMap", height = '100%')
                ),
                column(8,
                    tabsetPanel(type = "tabs",
                        tabPanel(
                            "Temperature",
                            plotOutput("weatherTempPlot", height = "500px"),
                            column(12,
                                fluidRow(
                                    checkboxGroupInput(
                                        inputId = "weatherTempSites",
                                        label = "Select sites:",
                                        inline = TRUE,
                                        choiceNames = list(
                                            span(icon("circle", style = "color:#66c2a5;"), " Garden Is"),
                                            span(icon("circle", style = "color:#fc8d62;"), " Rottnest"),
                                            #span(icon("circle", style = "color:#8da0cb;"), " Ocean Reef"),
                                            span(icon("circle", style = "color:#e78ac3;"), " Swanbourne"),
                                            #span(icon("circle", style = "color:#a6d854;"), " Melville Water"),
                                            #span(icon("circle", style = "color:#ffd92f;"), " Perth Airport"),
                                            span(icon("circle", style = "color:#e5c494;"), " Perth Metro"),
                                            span(icon("circle", style = "color:#b3b3b3;"), " Jandakot")
                                            #span(icon("circle", style = "color:#80b1d3;"), " Hillarys Boat Harbour")
                                        ),
                                        choiceValues = list(
                                            "sensor_repository_00234",
                                            "sensor_repository_00627",
                                            #"sensor_repository_00556",
                                            "sensor_repository_00676",
                                            #"sensor_repository_00493",
                                            #"sensor_repository_00571",
                                            "sensor_repository_00599",
                                            "sensor_repository_00339"
                                            #"sensor_repository_01027"
                                        )
                                    )
                                ),
                                fluidRow(
                                    column(
                                        9,
                                        sliderInput(
                                            "weatherTempDateInput",
                                            "Filter dates:",
                                            min=as.Date("2021-11-08","%Y-%m-%d"),
                                            max=as.Date("2021-12-07","%Y-%m-%d"),
                                            value=c(as.Date("2021-11-08"), as.Date("2021-12-07")),
                                            timeFormat="%Y-%m-%d",
                                            width = '100%',
                                            animate = animationOptions(1000)
                                        )
                                    ),
                                    column(
                                        3,
                                        tags$style(
                                            HTML(
                                                '.buttonWrapper {
                                                     margin: 0;
                                                    position: absolute;
                                                    top: 50%;
                                                    -ms-transform: translateY(-50%);
                                                    transform: translateY(-50%);
                                                    }
                                                    '
                                            )
                                        ),
                                        div(
                                            class="buttonWrapper",
                                            actionButton("plotWeatherTemp", "Plot sites", width = '100%')
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )



setwd('/Users/gilesknight/Documents/GitHub/swan.canning.dashboard/deprecated/shiny/R/')
#setwd('/Users/gilesknight/Documents/GitHub/swan.canning.dashboard/shiny/R/')
# Define server function
server <- function(input, output, session) {
     drv <- dbDriver("PostgreSQL")

     sensorColours  <- data.frame(
            code = c("sensor_repository_00748","sensor_repository_00756","sensor_repository_00768","sensor_repository_00784","sensor_repository_00796","sensor_repository_00800","sensor_repository_00804","sensor_repository_00812"),
            colour = c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f","#e5c494","#b3b3b3")
        )
    temperatureColours  <- data.frame(
            code = c(
                "sensor_repository_00234",
                "sensor_repository_00627",
                "sensor_repository_00556",
                "sensor_repository_00676",
                "sensor_repository_00493",
                "sensor_repository_00571",
                "sensor_repository_00599",
                "sensor_repository_00339",
                "sensor_repository_01027"
            ),
            colour = c(
                "#66c2a5",
                "#fc8d62",
                "#8da0cb",
                "#e78ac3",
                "#a6d854",
                "#ffd92f",
                "#e5c494",
                "#b3b3b3",
                "#80b1d3"
                )
        )


   hydroSites  <- read.csv("site_list.csv")

   output$hydroMap <- leaflet::renderLeaflet({
        leaflet::leaflet(hydroSites) %>%
        addTiles(group = "base") %>%
        leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
        setView(115.8613, -31.9523, 8)  %>%
        #addMarkers(lng = ~Lon, lat = ~Lat, group = ~Agency, popup = paste0("<b>Station Name: </b>",hydroSites$Station.Name, "<br><b>Agency: </b>", hydroSites$Agency))
        addCircleMarkers(lng = ~Lon, lat = ~Lat, color = "white", radius = 7, weight = 2, fillColor = ~colour, opacity = 1, fillOpacity = 1,
        popup = paste0("<b>Station Name: </b>",hydroSites$Station.Name, "<br><b>Agency: </b>", hydroSites$Agency))

        #addLayersControl(baseGroups = "base", overlayGroups = "velo")
    })
    # current  <- c("D", "D", "B")
    # previous  <- c("D")
    # filter  <- current %in% previous
    # filter
    # current[!filter]

    # # Initialize reactive values
    # rv <- reactiveValues(prevSelection = NULL)

    # # Append new value to previous values when input$flowSites changes
    # observeEvent(input$flowSites, {
    #     if(is.null(rv$prevSelection)){
    #         rv$prevSelection <- c(rv$prevSelection, input$flowSites)
    #         current  <- rv$prevSelection
    #         print(paste0("current: ", current))
    #     } else {
    #         rv$prevSelection <- c(rv$prevSelection, input$flowSites)
    #         print(paste0("rv$prevSelection: ", rv$prevSelection))
    #         filter  <- input$flowSites %in% rv$prevSelection
    #         print(paste0("Filter: ", filter))
    #         current  <- input$flowSites[!filter]
    #         print(paste0("current: ", current))
    #     }
    # })


    previousSelection <- NULL
    observeEvent(input$flowSites, {
        # print(paste0("Selected sites: ", input$flowSites))
        # #print(str(input$flowSites))
        # if(is.null(previousSelection)){
        #     lastSite  <- input$flowSites
        #     #print(lastSite)
        # } else {
        #     selectedSites  <- input$flowSites
        #     filter  <- lastSite %in% previousSelection
        #     lastSite[!filter]
        #     #print(lastSite)
        # }
        lastSite  <- tail(input$flowSites, n =1 )

        selectedHydroSites <- hydroSites[hydroSites$sensorID == lastSite,]
        leafletProxy("hydroMap") %>%
            setView(lng = selectedHydroSites$Lon, lat = selectedHydroSites$Lat, zoom = 12)
        previousSelection  <- input$flowSites
       # print(paste0("Previous sites: ", previousSelection))

    })

    output$flowDateInput  <- renderUI({
        sliderInput(
            "flowDateInput",
            "Filter dates:",
            min=as.Date(input$dateFrom),
            max=as.Date(input$dateTo),
            value=c(as.Date(input$dateFrom), as.Date(input$dateTo)),
            timeFormat="%Y-%m-%d",
            width = '100%',
            animate = animationOptions(1000)
        )
    })

    ### HYDROLOGY
    # FLOW
    observeEvent(input$plotHydroFlow,{
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
            # sensorData  <- tbl(conn, selectedSites)
            # query  <- sensorData  %>%
            #             filter(
            #                 st_feed_date_jdn >= 2458839 &
            #                 st_feed_date_jdn <= 2458840
            #             )
            # #query %>% show_query()
            # sensorData  <-  query %>% dplyr::collect()

            sensorData$datetime  <- as.POSIXlt(sensorData$st_feed_date_jdn*86400,
                                origin=structure(-210866760000,
                                                    class=c("POSIXct", "POSIXt"),
                                                    tzone="Australia/Perth"),
                                tz="Australia/Perth")
            sensorData  <- sensorData[,c("st_sensor_code","st_value_1","datetime")]
            allData <- rbind(allData, sensorData)
        }
        dbDisconnect(conn)

        selectedSites  <- c("sensor_repository_00748", "sensor_repository_00768")
        colourFilter  <-   sensorColours$code %in% selectedSites
        colourFilter  <- sensorColours[colourFilter, "colour"]

        #print(unique(allData$st_sensor_code))
        output$hydroFlowPlot  <- renderPlot({
            allData <-  dplyr::filter(allData,datetime >= as.POSIXct(input$flowDateInput[1]),datetime <= as.POSIXct(input$flowDateInput[2]))
            ggplot(allData, aes(x=as.POSIXct(datetime), y=st_value_1, fill = as.character(st_sensor_code))) +
             geom_area() +
             scale_fill_manual(values=colourFilter) +
             scale_x_datetime(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0)) +
             xlab("Date") +
             ylab("Flow (m3/s)") +
             theme_light(base_size = 14)+
             theme(legend.position = "none")
        })
    })

    # CUMULATIVE FLOW

    # observeEvent(input$plotCumulFlow,{
    #     conn <- dbConnect(
    #         drv, host='130.95.204.63', port='5433', dbname='scevo',
    #         user='dpaw_write', password='dpaw_write'
    #     )
    #     selectedSite  <- input$cumulFlowSites #"sensor_repository_00748"
    #     print(selectedSite)
    #     sensorData  <- tbl(conn, selectedSite) %>% as.data.frame()
    #     dbDisconnect(conn)
    #     sensorData$datetime  <- as.POSIXlt(sensorData$st_feed_date_jdn*86400,
    #                             origin=structure(-210866760000,
    #                                                 class=c("POSIXct", "POSIXt"),
    #                                                 tzone="Australia/Perth"),
    #                             tz="Australia/Perth")
    #     sensorData  <- sensorData[,c("st_sensor_code","st_value_1","datetime")]
    #     sensorData$year <- format(sensorData$datetime, format="%Y")
    #     sensorData <- mutate(group_by(sensorData,year), csum=cumsum(st_value_1))
    #     #sensorData %>% group_by(year) %>% mutate(csum = cumsum(st_value_1))
    #     sensorData$x  <- sensorData$datetime
    #     year(sensorData$x)  <- 0
    #     output$hydroCumulFlowPlot  <- renderPlot({
    #         sensorData <-  dplyr::filter(sensorData,x >= as.POSIXct(input$cumulFlowDateInput[1]), x <= as.POSIXct(input$cumulFlowDateInput[2]))
    #         print(head(sensorData))
    #         ggplot(sensorData, aes(x=as.POSIXct(x), y=csum, color = as.character(year))) +
    #          geom_line() +
    #          scale_x_datetime(expand = c(0,0), date_labels = "%e %b") +
    #          scale_y_continuous(expand = c(0,0)) +
    #          xlab("Date") +
    #          ylab("Cumulative flow (m3/s)") +
    #          theme_light(base_size = 14)
    #     })
    # })

    # HYDROLOGY - TIDE
    observeEvent(input$plotHydroTide,{
        # conn <- dbConnect(
        #     drv, host='130.95.204.63', port='5433', dbname='scevo',
        #     user='dpaw_write', password='dpaw_write'
        # )
        conn <- dbConnect(
            drv, host='130.95.44.39', port='5432', dbname='scevo',
            user='dpaw_write', password='dpaw_write'
        )
        selectedSites  <- "sensor_repository_00934" #input$hydroTideSites
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

        output$hydroTidePlot  <- renderPlot({
            allData <-  dplyr::filter(allData,datetime >= as.POSIXct(input$hydroTideDateInput[1]),datetime <= as.POSIXct(input$hydroTideDateInput[2]))
            ggplot(allData, aes(x=as.POSIXct(datetime), y=st_value_1, fill = as.character(st_sensor_code))) +
             geom_area() +
             #scale_fill_manual(values=colourFilter) +
             scale_x_datetime(expand = c(0,0)) +
             scale_y_continuous(expand = c(0,0)) +
             xlab("Date") +
             ylab("Tide") +
             theme_light(base_size = 14)+
             theme(legend.position = "none")
        })
    })

    # WEATHER - MAP
    weatherSites  <- read.csv("weather_site_list.csv")

   output$weatherMap <- leaflet::renderLeaflet({
        leaflet::leaflet(weatherSites) %>%
        addTiles(group = "base") %>%
        leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
        setView(115.8613, -31.9523, 10)   %>%
        addCircleMarkers(lng = ~Lon, lat = ~Lat, color = "white", radius = 7, weight = 2, fillColor = ~colour, opacity = 1, fillOpacity = 1,
        popup = paste0("<b>Station Name: </b>",weatherSites$Station.Name, "<br><b>Agency: </b>", weatherSites$Agency))
    })

    observeEvent(input$weatherTempSites, {
        lastSite  <- tail(input$weatherTempSites, n =1 )
        selectedWeatherSites <- weatherSites[weatherSites$sensorID == lastSite,]
        leafletProxy("weatherMap") %>%
            setView(lng = selectedWeatherSites$Lon, lat = selectedWeatherSites$Lat, zoom = 12)
        previousSelection  <- input$weatherTempSites

    })

   # WEATHER - TEMPERATURE
   observeEvent(input$plotWeatherTemp,{
        conn <- dbConnect(
            drv, host='130.95.204.63', port='5433', dbname='scevo',
            user='dpaw_write', password='dpaw_write'
        )
        selectedSites  <- input$weatherTempSites #"sensor_repository_00234"
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

        colourFilter  <-   temperatureColours$code %in% selectedSites
        colourFilter  <- temperatureColours[colourFilter, "colour"]

        output$weatherTempPlot  <- renderPlot({
            allData <-  dplyr::filter(allData,datetime >= as.POSIXct(input$weatherTempDateInput[1]),datetime <= as.POSIXct(input$weatherTempDateInput[2]))
            ggplot(allData, aes(x=as.POSIXct(datetime), y=st_value_1, colour = as.character(st_sensor_code))) +
                geom_line() +
                scale_colour_manual(values=colourFilter) +
                xlab("Date") +
                ylab("Temperature (°C)") +
                theme_light(base_size = 14)+
                theme(legend.position = "none")

        })
   })
}

# Create Shiny object
shinyApp(ui = ui, server = server)







