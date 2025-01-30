#' 4_quality_1_setup_1_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_1_tables_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(

  )
}

#' 4_quality_1_setup_1_tables Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_1_tables_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_1_tables_ui("4_quality_1_setup_1_tables_1")
    
## To be copied in the server
# mod_4_quality_1_setup_1_tables_server("4_quality_1_setup_1_tables_1")
