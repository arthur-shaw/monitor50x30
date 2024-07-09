#' 3_complete_1_setup_2_clusters_3_manager_id UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_3_manager_id_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
 
  )
}
    
#' 3_complete_1_setup_2_clusters_3_manager_id Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_3_manager_id_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_3_manager_id_ui("3_complete_1_setup_2_clusters_3_manager_id_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_3_manager_id_server("3_complete_1_setup_2_clusters_3_manager_id_1")
