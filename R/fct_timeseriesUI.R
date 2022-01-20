#' timeseriesUI 
#'
#' @description Generates the common timeseries UI elements e.g. date inputs, slider. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

timeseriesUI <- function(namespace){
  return(
    tags$div(
      fluidRow(
        column(
          6,
          dateInput(
            inputId = paste0(namespace,"DateFrom"),
            label = "From:"
          )
        ),
        column(
          6,
          dateInput(
            inputId = paste0(namespace,"DateTo"),
            label = "To:"
          )
        )
      ),
      fluidRow(
        column(
          9,
          uiOutput(
            outputId = paste0(namespace,"DateSlider")
          )
        ),
        column(
          3,
          tags$style(
            HTML(
              '.buttonWrapper {margin: 0;position: absolute;top: 50%;-ms-transform: translateY(-50%);transform: translateY(-50%);}'
            )
          ),
          div(
            class = "buttonWrapper",
            actionButton(
              inputId = paste0(namespace,"FetchData"),
              label = "Plot"
            )
          )
        )
      )
    )
  )
}
