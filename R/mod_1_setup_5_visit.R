#' 1_setup_5_visit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_setup_5_visit_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
 
    shiny::selectizeInput(
      inputId = ns("svy_num_visits"),
      label = "How many visits to the household?",
      choices = c(
        "1",
        "2"
        # "3"
      ),
      selected = NULL,
      multiple = FALSE
    ),
    shiny::selectizeInput(
      inputId = ns("svy_current_visit"),
      label = label_tooltip(
        lbl = "What is the current visit to the household?",
        desc = paste(
          "The app needs to know the visit number.",
          "If the survey has a single visit to the household",
          "then choose 1.",
          "If the survey has multiple visits,",
          "choose the visit number (e.g., 2 for second visit)."
        )
      ),
      choices = c(
        "Single visit",
        "Post-planting",
        "Post-harvest"
        # "Post-planting for 1st season",
        # "Post-harvest for 1st season/post-planting for 2nd season",
        # "Post-harvest for 2nd season"
      ),
      selected = NULL,
      multiple = FALSE
    ),
    shiny::dateInput(
      inputId = ns("svy_start_date"),
      label = label_tooltip(
        lbl = "When will the survey data collection start?",
        desc = paste(
          "The app uses this date in a few ways.",
          "First, it uses it as a starting point for collecting",
          "tablet sync records for interviewers.",
          "Second, it uses it as the default start date",
          "for reports."
        )
      )
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 1_setup_5_visit Server Functions
#'
#' @noRd 
mod_1_setup_5_visit_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    if (!is.null(r6$visit_details_provided)) {

      if (r6$visit_details_provided == TRUE) {

        shiny::updateSelectizeInput(
          inputId = "svy_num_visits",
          selected = r6$svy_num_visits
        )

        shiny::updateSelectizeInput(
          inputId = "svy_current_visit",
          selected = r6$svy_current_visit
        )

        shiny::updateDateInput(
          inputId = "svy_start_date",
          value = r6$svy_start_date
        )

      }

    }

    # ==========================================================================
    # react to number of visits selection
    # ==========================================================================

    # limit choices of current visit?
    # alternatively, validate answer to current visit in case not understanding
    # prior question?
 
    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # react to whether or not all fields are filled
      # if not, provide user feedback
      if (
        is.null(input$svy_num_visits) | is.null(input$svy_current_visit) |
        is.null(input$svy_start_date)
      ) {

        shinyFeedback::showToast(
          type = "error",
          message = "Information about the survey visit not provided"
        )

      # if so, capture and save user inputs
      } else if (
        !is.null(input$svy_num_visits) & !is.null(input$svy_current_visit) &
        !is.null(input$svy_start_date)
      ) {

        # save params to R6
        r6$svy_num_visits <- input$svy_num_visits
        r6$svy_current_visit <- input$svy_current_visit
        r6$svy_start_date <- as.character(input$svy_start_date)
        r6$visit_details_provided <- TRUE

        # write R6 to disk
        r6$write()

        # react to whether or not all fields are filled
        # send signal that templates details saved
        gargoyle::trigger("save_visit")

      }



    })

  })
}
    
## To be copied in the UI
# mod_1_setup_5_visit_ui("1_setup_5_visit_1")
    
## To be copied in the server
# mod_1_setup_5_visit_server("1_setup_5_visit_1")
