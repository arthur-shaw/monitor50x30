#' 4_quality_1_setup_1_tables_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_1_tables_details_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(

  )
}

#' 4_quality_1_setup_1_tables_details Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_1_tables_details_server <- function(
  id
) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns


  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_1_tables_details_ui("4_quality_1_setup_1_tables_details_1")
    
## To be copied in the server
# mod_4_quality_1_setup_1_tables_details_server("4_quality_1_setup_1_tables_details_1")
