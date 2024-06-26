#' 3_complete_1_setup_1_domains UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_1_domains_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
 
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 3_complete_1_setup_1_domains Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_1_domains_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      gargoyle::trigger("save_domains")

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_1_domains_ui("3_complete_1_setup_1_domains_1")
    
## To be copied in the server
# mod_3_complete_1_setup_1_domains_server("3_complete_1_setup_1_domains_1")
