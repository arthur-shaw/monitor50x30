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
    shiny::textOutput(outputId = ns("save_error")),
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

        # ----------------------------------------------------------------------
        # check that domain obs == cluster obs
        # ----------------------------------------------------------------------

        # compute total obs from domain table
        tot_obs_domain <- r6$obs_per_domain |>
          dplyr::pull(Obs) |>
          sum(na.rm = TRUE)

        # compute total obs implied by cluster number and size
        tot_obs_cluster <- input$n_clusters * input$n_per_cluster

        # check whether sample sizes implied by domain and cluster are the same
        tot_obs_domain_cluster_same <- identical(
          tot_obs_domain,
          tot_obs_cluster
        )

        # display an error if the same sizes are not the same.
        output$save_error <- shiny::renderText(
          if (!tot_obs_domain_cluster_same) {
            shiny::validate(
              message = glue::glue(
                "Domain observations ({tot_obs_domain}) !=",
                "cluster observations ({tot_obs_cluster}).",
                "The total observation counts should match.",
                "Please correct.",
                .sep = " "
              )
            )
          }
        )

        # halt execution until obs counts agree
        req(tot_obs_domain_cluster_same)

        # ----------------------------------------------------------------------
        # store values in R6, save R6 to disk, and send completion signal
        # ----------------------------------------------------------------------

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
