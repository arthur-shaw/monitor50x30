#' 4_quality_2_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_2_report_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # TODO: add filters
    # - time
    # - team
    shiny::actionButton(
      inputId = ns("create"),
      "Create"
    ),
    # render download button UI here when conditions in server satisfied
    shiny::uiOutput(outputId = ns("dl_button"))

  )
}

#' 4_quality_2_report Server Functions
#'
#' @noRd 
mod_4_quality_2_report_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # react to create button
    # ==========================================================================

    # create a waitress that overlays the create button to communicate progress
    report_waitress <- waiter::Waitress$new(
      selector = 'div.tab-pane button.action-button',
      theme = "overlay-percent",
      infinite = TRUE
    )

    shiny::observeEvent(input$create, {

      # ------------------------------------------------------------------------
      # start the progress overlay
      # ------------------------------------------------------------------------

      report_waitress$start()

      # ------------------------------------------------------------------------
      # render document
      # ------------------------------------------------------------------------

      quality_params <- list(
        dir_app = r6$app_dir,
        dir_data = r6$dirs$micro_combine,
        dir_teams = r6$dirs$team,
        main_df_name = r6$qnr_var
      )

      render_report(
        report_type = "quality",
        proj_dir = r6$app_dir,
        params = quality_params
      )

      # ------------------------------------------------------------------------
      # show report download button
      # ------------------------------------------------------------------------

      output$dl_button <- shiny::renderUI(

        shiny::downloadButton(
          outputId = ns("download"),
          label = "Download"
        )

      )

      # ------------------------------------------------------------------------
      # close the progress overlay
      # ------------------------------------------------------------------------

      report_waitress$close()

    })

    # ==========================================================================
    # react to download button
    # ==========================================================================

    output$download <- shiny::downloadHandler(
      filename = "report_quality.html",
      content = function(file) {
        fs::file_copy(
          path = fs::path(
            r6$dirs$report_quality, "report_quality.html"
          ),
          new_path = file
        )
      }
    )

  })
}

## To be copied in the UI
# mod_4_quality_2_report_ui("4_quality_2_report_1")

## To be copied in the server
# mod_4_quality_2_report_server("4_quality_2_report_1")
