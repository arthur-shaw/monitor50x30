#' 3_complete_2_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_2_report_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::selectizeInput(
      inputId = ns("report_teams"),
      label = "Please choose the teams for which to generate the report",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::actionButton(
      inputId = ns("create"),
      "Create"
    ),
    # render download button UI here when conditions in server satisfied
    shiny::uiOutput(outputId = ns("dl_button"))

  )
}
    
#' 3_complete_2_report Server Functions
#'
#' @noRd 
mod_3_complete_2_report_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # ==========================================================================
    # initialize page
    # ==========================================================================

    if (!is.null(r6$data_downloaded)) {

      shiny::updateSelectizeInput(
        inputId = "report_teams",
        choices = r6$report_teams,
        selected = r6$report_teams_selected
      )

    }

    # ==========================================================================
    # react to data download
    # ==========================================================================

    gargoyle::on("download_data", {

      # construct path to teams composition data file
      teams_dta_path <- fs::path(
        r6$dirs$team, "team_composition.dta"
      )

      # create team choice options
      team_choices <- create_team_choices(
        path = teams_dta_path,
        # TODO: have this change with UI language (when relevant)
        all_teams_txt = "All teams"
      )

      # capture choices in R6 and write to disk
      r6$report_teams <- team_choices
      r6$write()

      # update options in UI
      shiny::updateSelectizeInput(
        inputId = "report_teams",
        choices = team_choices,
        selected = team_choices[1] # note: the language-appropriate "All teams"
      )

    })

    # ==========================================================================
    # update team choice so that either "all" or some teams; not both
    # ==========================================================================

    shiny::observeEvent(input$report_teams, {

      # prevents race condition where observer fire before UI loaded
      shiny::req(input$report_teams)

      if (length(input$report_teams) > 1) {

        # take the last entry in the selective input selection
        # this works because it's a vector "stack", where the last selection is
        # added to the end of the vector
        last_team_selected <- shiny::reactive({
          utils::tail(input$report_teams, n = 1)
        })

        if (any(input$report_teams %in% "All teams")) {

          shiny::updateVarSelectInput(
            inputId = "report_teams",
            selected = last_team_selected()
          )

        }

      }

    },
    ignoreInit = TRUE)

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

      # start the progress overlay
      report_waitress$start()

      # ------------------------------------------------------------------------
      # capture team selections in R6 and on disk
      # ------------------------------------------------------------------------

      # capture user intputs in R6
      r6$report_teams_selected <- input$report_teams

      # write R6 to disk
      r6$write()

      # ------------------------------------------------------------------------
      # render document
      # ------------------------------------------------------------------------

      # compose parameter list
      doc_params <- list(
        # paths
        dir_data = r6$dirs$micro_combine,
        dir_teams = r6$dirs$team,
        dir_sync = r6$dirs$sync,
        dir_by_domain = r6$dirs$obs_per_domain,
        dir_by_team = r6$dirs$obs_per_team,
        # main data file
        main_df_name = r6$qnr_var,
        # domains
        domain_vars = r6$domain_vars_selected,
        # clusters
        n_clusters = r6$n_clusters,
        n_per_cluster = r6$n_per_cluster,
        cluster_vars_computer = r6$computer_id_vars_selected,
        cluster_vars_manager = r6$manager_id_vars_selected,
        cluster_template_txt = r6$cluster_template_txt,
        # report scope
        which_teams = r6$report_teams_selected
      )

      # render document
      render_report(
        report_type = "completeness",
        proj_dir = r6$app_dir,
        params = doc_params
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

      # close the progress overlay
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
            r6$dirs$report_completeness, "report_completeness.html"
          ),
          new_path = file
        )
      }
    )


  })
}
    
## To be copied in the UI
# mod_3_complete_2_report_ui("3_complete_2_report_1")
    
## To be copied in the server
# mod_3_complete_2_report_server("3_complete_2_report_1")
