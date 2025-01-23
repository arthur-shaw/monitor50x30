#' 3_complete_1_setup_2_clusters_2_computer_id UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_2_computer_id_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::selectizeInput(
      inputId = ns("computer_id_vars"),
      label = paste(
        "Select the variables that a computer needs to",
        "identify a cluster uniquely",
        sep = " "
      ),
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 3_complete_1_setup_2_clusters_2_computer_id Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_2_computer_id_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # compute the choices based on data
    # initialize as the values stored in R6
    # if never set, the value will be null
    comp_id_vars <- shiny::reactiveValues(choices = r6$cluster_id_var_choices)

    # when data are downloaded, compute the choices and update the choices
    gargoyle::on("download_data", {

      comp_id_vars$choices <- make_vars_options(
        path = fs::path(r6$dirs$qnr, "qnr_vars.rds"),
        var_types = c(
          "SingleQuestion",
          "TextQuestion",
          "NumericQuestion"
        )
      )

      shiny::updateSelectizeInput(
        inputId = "computer_id_vars",
        choices = comp_id_vars$choices
      )

    })

    # otherwise, load past selections from R6 and computed choices
    if (!is.null(r6$cluster_computer_id_provided)) {

      shiny::updateSelectizeInput(
        inputId = "computer_id_vars",
        choices = r6$domain_var_choices,
        selected = r6$computer_id_vars_selected
      )

    }

    # ==========================================================================
    # react to save
    # ==========================================================================


    shiny::observeEvent(input$save, {

      # store values in R6
      r6$cluster_id_var_choices <- comp_id_vars$choices
      r6$computer_id_vars_selected <- input$computer_id_vars
      r6$cluster_computer_id_provided <- TRUE

      # write R6 to disk
      r6$write()

      # signal that ID vars for computers have been saved
      gargoyle::trigger("save_computer_identify_clusters")

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_2_computer_id_ui("3_complete_1_setup_2_clusters_2_computer_id_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_2_computer_id_server("3_complete_1_setup_2_clusters_2_computer_id_1")
