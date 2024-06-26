#' 3_complete_2_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_2_report_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

  )
}
    
#' 3_complete_2_report Server Functions
#'
#' @noRd 
mod_3_complete_2_report_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
    
## To be copied in the UI
# mod_3_complete_2_report_ui("3_complete_2_report_1")
    
## To be copied in the server
# mod_3_complete_2_report_server("3_complete_2_report_1")
