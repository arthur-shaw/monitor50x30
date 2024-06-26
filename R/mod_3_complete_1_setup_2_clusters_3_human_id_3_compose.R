#' 3_complete_1_setup_2_clusters_3_human_id_3_compose UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_3_human_id_3_compose_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
 
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 3_complete_1_setup_2_clusters_3_human_id_3_compose Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_3_human_id_3_compose_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_3_human_id_3_compose_ui("3_complete_1_setup_2_clusters_3_human_id_3_compose_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_3_human_id_3_compose_server("3_complete_1_setup_2_clusters_3_human_id_3_compose_1")
