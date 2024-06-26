#' 3_complete_1_setup_2_clusters_3_human_id_2_order UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_3_human_id_2_order_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
 
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 3_complete_1_setup_2_clusters_3_human_id_2_order Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_3_human_id_2_order_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_3_human_id_2_order_ui("3_complete_1_setup_2_clusters_3_human_id_2_order_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_3_human_id_2_order_server("3_complete_1_setup_2_clusters_3_human_id_2_order_1")
