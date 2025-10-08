#' 4_quality_1_setup_3_interviews UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_3_interviews_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectizeInput(
      inputId = ns("interview_statuses"),
      label = paste(
        "Select the Survey Solutions status(es) of interviews to include."
      ),
      choices = c(
        "Completed" = 100,
        "RejectedBySupervisor" = 65,
        "ApprovedBySupervisor" = 120,
        "RejectedByHeadquarters" = 125,
        "ApprovedByHeadquarters" = 130
      ),
      multiple = TRUE,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_quality_1_setup_3_interviews Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_3_interviews_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # restore previously saved selections
    if (!is.null(r6$interviews_selected)) {

      shiny::updateSelectInput(
        inputId = "interview_statuses",
        selected = r6$interview_statuses
      )

    }

    # ==========================================================================
    # react to saving
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # interview statuses selected
      r6$interview_statuses <- input$interview_statuses

      # save action
      r6$interviews_selected <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_interviews")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_3_interviews_ui("4_quality_1_setup_3_interviews_1")
    
## To be copied in the server
# mod_4_quality_1_setup_3_interviews_server("4_quality_1_setup_3_interviews_1")
