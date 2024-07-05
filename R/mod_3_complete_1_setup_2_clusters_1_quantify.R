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

    shiny::numericInput(
      inputId = ns("n_clusters"),
      label = "Number of clusters",
      value = NULL
    ),
    shiny::numericInput(
      inputId = ns("n_per_cluster"),
      label = "Number of observations per clusters",
      value = NULL
    ),
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

    # ==========================================================================
    # initialize page
    # ==========================================================================

    if (!is.null(r6$cluster_quantity_details_provided)) {

      shiny::updateNumericInput(
        inputId = "n_clusters",
        value = r6$n_clusters
      )

      shiny::updateNumericInput(
        inputId = "n_per_cluster",
        value = r6$n_per_cluster
      )

    }

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # react to whether fields are filled
      if (is.null(input$n_clusters) | is.null(input$n_per_cluster)) {

        if (is.null(input$n_clusters)) {

          shinyFeedback::showToast(
            type = "error",
            message = "Number clusters left unanswered."
          )

        }

        if (is.null(input$n_per_cluster)) {

          shinyFeedback::showToast(
            type = "error",
            message = "Number of observations per clusters left unanswered."
          )

        }

      } else if (!is.null(input$n_clusters) & !is.null(input$n_per_cluster)) {

        # store user inputs in R6
        r6$n_clusters <- input$n_clusters
        r6$n_per_cluster <- input$n_per_cluster
        r6$cluster_quantity_details_provided <- TRUE

        # write R6 to disk
        r6$write()

        # signal that quantitative information saved
        gargoyle::trigger("save_quantify_clusters")

      }

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_1_quantify_ui("3_complete_1_setup_2_clusters_1_quantify_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_1_quantify_server("3_complete_1_setup_2_clusters_1_quantify_1")
