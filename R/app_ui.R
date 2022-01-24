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
            leaflet::leafletOutput("hydroMap", height = '700px')
          ),
          column(
            8,
            tabsetPanel(
              id = "hydroTabset",
              type = "tabs",
              tabPanel(
                title = "Flow",
                style = "overflow-y:scroll; max-height: 700px",
                plotOutput("hydroFlowPlot", height = "500px"),
                tags$details(
                  tags$summary(HTML("Select sites <i>(click to expand)</i>:")),
                  sensorCheckBoxUI(namespace = "hydroFlow",sensorInfo = sensorInfo)
                ),
                timeseriesUI(namespace = "hydroFlow")
              ),
              tabPanel(
                title = "test"
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

