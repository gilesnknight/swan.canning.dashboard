#' sensorCheckBoxUI 
#'
#' @description Generates UI checkbox elements from a provided @param sensorInfo df.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

sensorCheckBoxUI <- function(namespace,sensorInfo){
  
  choiceNames <- lapply(1:nrow(sensorInfo), function(i) {
    span(
      icon(
        sensorInfo[,"icon"][i], 
        style = paste0("color:",sensorInfo[,"colour"][i],";")
        ), 
      paste0(" ",sensorInfo[,"label"][i]))
  })
  
  return(
    tags$div(
      column(
        12,
        fluidRow(
          checkboxGroupInput(
            inputId = paste0(namespace,"SiteCheckBox"),
            label = "Select sites:",
            inline = TRUE,
            choiceNames = choiceNames,
            choiceValues =  sensorInfo[,"sensorCode"]
          )
        )
      )
    )
  )
  
  
  
}
