#' 4_quality_2_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_2_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 4_quality_2_report Server Functions
#'
#' @noRd 
mod_4_quality_2_report_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_4_quality_2_report_ui("4_quality_2_report_1")
    
## To be copied in the server
# mod_4_quality_2_report_server("4_quality_2_report_1")
