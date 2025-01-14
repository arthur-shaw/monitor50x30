#' 3_complete_1_setup_4_save UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_4_save_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 3_complete_1_setup_4_save Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_4_save_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$save, {

      # write settings status to R6
      r6$completeness_settings_saved <- TRUE

      # write R6 to disk
      r6$write()

      # signal that settings have been saved
      gargoyle::trigger("save_completeness_setup")

    })
  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_4_save_ui("3_complete_1_setup_4_save_1")
    
## To be copied in the server
# mod_3_complete_1_setup_4_save_server("3_complete_1_setup_4_save_1")
