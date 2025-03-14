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

    rhandsontable::rHandsontableOutput(ns("n_per_team")),
    shiny::textOutput(outputId = ns("save_error")),
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

    # ==========================================================================
    # initialize page / compose rhandsontable
    # ==========================================================================

    output$n_per_team <- rhandsontable::renderRHandsontable({

      if (is.null(r6$team_workload_provided)) {

        teams_df <- haven::read_dta(
          file = fs::path(r6$dirs$team, "team_composition.dta")
        ) |>
        dplyr::distinct(SupervisorName) |>
        dplyr::select(team = SupervisorName) |>
        dplyr::mutate(Obs = NA_integer_)

      } else if (!is.null(r6$team_workload_provided)) {

        teams_df <- r6$n_per_team

      }

      rhandsontable::rhandsontable(data = teams_df) |>
        # make supervisor name column read-only
        rhandsontable::hot_col(
          col = "team",
          readOnly = TRUE
        ) |>
        # format inputs as whole nubmers
        rhandsontable::hot_col("Obs", format = "0") |>
        # allow column sorting and 
        rhandsontable::hot_cols(columnSorting = TRUE, colWidths = 110) |>
        # require inputs to be non-negative
        rhandsontable::hot_cols(
          validator = "
            function (value, callback) {
              setTimeout(function(){
                callback(value >= 0 );
              }, 1000)
            }",
          allowInvalid = FALSE)

    })

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # ----------------------------------------------------------------------
      # check that domain obs == cluster obs
      # ----------------------------------------------------------------------

      # compute total obs implied by cluster number and size
      tot_obs_cluster <- r6$n_clusters * r6$n_per_cluster

      # compute total obs from domain table
      tot_obs_team <- rhandsontable::hot_to_r(input$n_per_team) |>
        dplyr::pull(Obs) |>
        sum(na.rm = TRUE)

      # check whether sample sizes implied by cluster and team workload are same
      tot_obs_cluster_team_same <- identical(tot_obs_cluster, tot_obs_team)

      # display an error if the same sizes are not the same.
      output$save_error <- shiny::renderText(
        if (!tot_obs_cluster_team_same) {
          shiny::validate(
            message = glue::glue(
              "Cluster observations ({tot_obs_cluster}) !=",
              "workload observations ({tot_obs_team}).",
              "The total observation counts should match.",
              "Please correct.",
              .sep = " "
            )
          )
        }
      )

      # halt execution until obs counts agree
      req(tot_obs_cluster_team_same)

      # ----------------------------------------------------------------------
      # store values in R6, save R6 to disk, and send completion signal
      # ----------------------------------------------------------------------

      # capture inputs in R6
      r6$n_per_team <- rhandsontable::hot_to_r(input$n_per_team)
      r6$team_workload_provided <- TRUE

      # write R6 to disk
      r6$write()

      # write data to disk for use by report
      saveRDS(
        object = r6$n_per_team,
        file = fs::path(r6$dirs$obs_per_team, "obs_per_team.rds")
      )

      # signal that workload by team has been set
      gargoyle::trigger("save_workloads")

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_3_team_workload_ui("3_complete_1_setup_3_team_workload_1")
    
## To be copied in the server
# mod_3_complete_1_setup_3_team_workload_server("3_complete_1_setup_3_team_workload_1")
