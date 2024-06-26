#' 3_complete_1_setup_2_clusters_1_quantify UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_1_quantify_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )
 
  )
}
    
#' 3_complete_1_setup_2_clusters_1_quantify Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_1_quantify_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$save, {

      # signal that quantitative information saved
      gargoyle::trigger("save_quantify_clusters")

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_1_quantify_ui("3_complete_1_setup_2_clusters_1_quantify_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_1_quantify_server("3_complete_1_setup_2_clusters_1_quantify_1")
