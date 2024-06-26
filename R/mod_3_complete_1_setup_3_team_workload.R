#' 3_complete_1_setup_3_team_workload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_3_team_workload_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
 
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 3_complete_1_setup_3_team_workload Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_3_team_workload_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$save, {

      # signal that workload by team has been set
      gargoyle::trigger("save_workloads")

    })
 
  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_3_team_workload_ui("3_complete_1_setup_3_team_workload_1")
    
## To be copied in the server
# mod_3_complete_1_setup_3_team_workload_server("3_complete_1_setup_3_team_workload_1")
