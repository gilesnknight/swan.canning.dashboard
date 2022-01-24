#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  #### WEATHER ####
  
 
  
  #### HYDRO ####
  
  # Filter sensorInfo data to only hydro sensors
  hydroSensorInfo <- sensorInfo[sensorInfo[["group"]]=="hydro",]
  
  # Generates blank hydro tab web-map
  output$hydroMap <- leaflet::renderLeaflet({
    webMap()
  })
  
  # On panel change within the Hydro tabset, update map markers to reflect graphable sensors
  observeEvent(input$hydroTabset,{
    switch(
      input$hydroTabset,
      "Flow" = sensorMapMarkers(
        mapID = "hydroMap", 
        data = hydroSensorInfo, 
        subGroup = input$hydroTabset
        ),
      "test" = sensorMapMarkers(
        mapID = "hydroMap", 
        data = hydroSensorInfo, 
        subGroup = input$hydroTabset
      )
    )
  })
  
  # Update slider from calendar date inputs
  output$hydroFlowDateSlider <- renderUI({
    plotSlider(
      inputID = "hydroFlowDateSlider",
      minDate = input$hydroFlowDateFrom,
      maxDate = input$hydroFlowDateTo
    )
  })
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$hydroFlowFetchData,{ 
    hydroFlowData <- databaseConnect(sensorCodes = input$hydroFlowSiteCheckBox) 
    
    # Get line plot colours for selected sensors
    hydroFlowDataColours <- activeSensorColours(
      checkBoxInputs = input$hydroFlowSiteCheckBox,
      sensorInfo = hydroSensorInfo
    )
    
    # Generate line graph from fetched data and diplay between slider dates
    output$hydroFlowPlot <- renderPlot({
      hydroFlowData <- dplyr::filter(
        hydroFlowData,
        datetime >= as.POSIXct(input$hydroFlowDateSlider[1]),
        datetime <= as.POSIXct(input$hydroFlowDateSlider[2])
      )
    
    # Plot the graph    
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
