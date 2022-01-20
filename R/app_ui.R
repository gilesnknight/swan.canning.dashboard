#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny leaflet
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    navbarPage(
      theme = bslib::bs_theme(bootswatch = "litera"),
      title = "SCEVO",
      tabPanel(
        "Hydrology",
        icon = icon("water"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("hydroMap", height = '100%')
          ),
          column(
            8,
            tabsetPanel(
              type = "tabs",
              tabPanel(
                title = "Flow",
                plotOutput("hydroFlowPlot", height = "500px"),
                sensorCheckBoxUI(namespace = "hydroFlow",sensorInfo = sensorInfo),
                timeseriesUI(namespace = "hydroFlow")
              )
            )
          )
        )
      ),
      tabPanel(
        "Weather",
        icon = icon("cloud-sun"),
        plotOutput("weatherTempPlot", height = "500px"),
        mod_timeseriesFetch_ui("weatherFetch")
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'secvo'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

