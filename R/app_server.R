#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  print(sensorInfo)
  
  #### WEATHER ####
  
 
  
  #### HYDRO ####
  
  # Leaflet web-map
  output$hydroMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 8) 
  })
  
  # Update slider from calander date inputs
  output$hydroFlowDateSlider <- renderUI({
    sliderInput(
      inputId = "hydroFlowDateSlider",
      "Filter dates:",
      min = as.Date(input$hydroFlowDateFrom),
      max = as.Date(input$hydroFlowDateTo),
      value = c(
        as.Date(input$hydroFlowDateFrom),
        as.Date(input$hydroFlowDateTo)
        ),
      timeFormat="%Y-%m-%d",
      width = '100%',
      animate = animationOptions(1000)
    )
  })
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$hydroFlowFetchData,{ 
    hydroFlowData <- databaseConnect(sensorCodes = input$hydroFlowSiteCheckBox) #c("sensor_repository_00234", "sensor_repository_00627")
    
    hydroFlowSelectedSites <-  input$hydroFlowSiteCheckBox
    hydroFlowDataColours <-  sensorInfo$sensorCode %in% hydroFlowSelectedSites
    hydroFlowDataColours <- sensorInfo[hydroFlowDataColours, "colour"]
    
    # Generate line graph from fetched data and diplay between slider dates
    output$hydroFlowPlot <- renderPlot({
      hydroFlowData <- dplyr::filter(
        hydroFlowData,
        datetime >= as.POSIXct(input$hydroFlowDateSlider[1]),
        datetime <= as.POSIXct(input$hydroFlowDateSlider[2])
      )
      plotLine(
        plotData = hydroFlowData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Flow (m3/s)",
        plotDataColours = hydroFlowDataColours
      )
    })
  })
   
}
