#' 3_complete_1_setup_2_clusters_3_manager_id_1_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_3_manager_id_1_select_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::selectizeInput(
      inputId = ns("manager_id_vars"),
      label = paste0(
        "Select the variables that a survey manager needs to ",
        "identify a cluster"
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
    
#' 3_complete_1_setup_2_clusters_3_manager_id_1_select Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_3_manager_id_1_select_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # compute the choices based on data
    # initialize as the values stored in R6
    # if never set, the value will be null
    manager_id_vars <- shiny::reactiveValues(
      choices = r6$cluster_id_var_choices
    )

    # when data are downloaded, compute the choices and update the choices
    gargoyle::on("download_data", {

      manager_id_vars$choices <- make_vars_options(
        path = fs::path(r6$app_dir, "04_qnr_metadata", "qnr_vars.rds"),
        var_types = c(
          "SingleQuestion",
          "TextQuestion",
          "NumericQuestion"
        )
      )

      shiny::updateSelectizeInput(
        inputId = "manager_id_vars",
        choices = manager_id_vars$choices
      )

    })

    # otherwise, load past selections from R6 and computed choices
    if (!is.null(r6$cluster_manager_id_provided)) {

      shiny::updateSelectizeInput(
        inputId = "manager_id_vars",
        choices = r6$cluster_id_var_choices,
        selected = r6$manager_id_vars_selected
      )

    }

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # store inputs in R6
      r6$manager_id_vars_selected <- input$manager_id_vars
      r6$cluster_manager_id_provided <- TRUE

      # write R6 to disk
      r6$write()

      # signal that cluster ID variables have been selected
      gargoyle::trigger("save_manager_select_clusters")

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_3_manager_id_1_select_ui("3_complete_1_setup_2_clusters_3_manager_id_1_select_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_3_manager_id_1_select_server("3_complete_1_setup_2_clusters_3_manager_id_1_select_1")
