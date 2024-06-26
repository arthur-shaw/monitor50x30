#' 3_complete_1_setup_2_clusters_3_human_id_1_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_3_human_id_1_select_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 3_complete_1_setup_2_clusters_3_human_id_1_select Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_3_human_id_1_select_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_3_human_id_1_select_ui("3_complete_1_setup_2_clusters_3_human_id_1_select_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_3_human_id_1_select_server("3_complete_1_setup_2_clusters_3_human_id_1_select_1")
