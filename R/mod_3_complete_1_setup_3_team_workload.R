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
          file = fs::path(
            r6$app_dir, "03_team_composition",
            "team_composition.dta")
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

      # capture inputs in R6
      r6$n_per_team <- NULL
      r6$team_workload_provided <- TRUE

      # write R6 to disk
      r6$write()

      # signal that workload by team has been set
      gargoyle::trigger("save_workloads")

    })
 
  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_3_team_workload_ui("3_complete_1_setup_3_team_workload_1")
    
## To be copied in the server
# mod_3_complete_1_setup_3_team_workload_server("3_complete_1_setup_3_team_workload_1")
