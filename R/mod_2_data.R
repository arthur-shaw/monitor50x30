#' 2_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_2_data_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::actionButton(
      inputId = ns("fetch_data"),
      label = "Fetch data"
    ),
    shiny::selectInput(
      inputId = ns("select_action"),
      label = "Select the action to take",
      choices = c(
        "Data completeness report",
        "Data quality report"
      )
    ),
    bslib::accordion(
      id = ns("download_data"),
      bslib::accordion_panel(
        title = "Download data",
        shiny::p("PLACEHOLDER for data details")
      )


    )

 
  )
}
    
#' 2_data Server Functions
#'
#' @noRd 
mod_2_data_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # react to fetch download button
    # ==========================================================================

    shiny::observeEvent(input$fetch_data, {

      # ========================================================================
      # Show waiter
      # ========================================================================

      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4("Starting data download process")
        )
      )
      Sys.sleep(1)

      # ========================================================================
      # Fetch data
      # ========================================================================

      # ------------------------------------------------------------------------
      # delete stale files from previous session
      # ------------------------------------------------------------------------

      # microdata
      delete_stale_data(dir = r6$dirs$micro_dl)
      delete_stale_data(dir = r6$dirs$micro_combine)
      # sync data
      delete_stale_data(dir = r6$dirs$sync)
      # team composition
      delete_stale_data(dir = r6$dirs$team)

      # ------------------------------------------------------------------------
      # get microdata
      # ------------------------------------------------------------------------

      # inform the user that microdata are being fetched
      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4("Fetching microdata")
        )
      )

      # extract ID and title for questionnaires matching user-provided pattern
      qnr_ids <- r6$matching_qnr_tbl |>
        # compose questionnaire ID expected by `susoflows::download_data()`
        # format: {GUID}${version}
        dplyr::mutate(id = paste0(.data$questionnaireId, "$", .data$version)) |>
        dplyr::pull(id)
      qnr_titles <- dplyr::pull(r6$matching_qnr_tbl, .data$title)

      # prepare to loop over questionnaire IDs
      qnr_tot <- length(qnr_ids)
      qnr_n <- 1

      # download by iterating through each matching questionnaire
      for(qnr_id in qnr_ids) {

        # extract title of current questionnaire
        qnr_title <- qnr_titles[qnr_n]

        # update user on which data is being downloaded
        waiter::waiter_show(html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4(
            glue::glue("
            Downloading data for questionnaire {qnr_n} of {qnr_tot} :
            {qnr_title}"
            )
          )
        ))

        # download data
        susoflows::download_data(
          qnr_id = qnr_id,
          export_type = "STATA",
          path = r6$dirs$micro_dl,
          server = r6$server,
          workspace = r6$workspace,
          user = r6$user,
          password = r6$password
        )

        # increment questionnaire counter
        qnr_n <- qnr_n + 1

      }
      # unpack downloaded data
      waiter::waiter_show(html = shiny::tagList(
        waiter::spin_ring(),
        shiny::h4("Unzipping downloaded microdata data")
      ))
      unpack_all_zip_to_dir(r6$dirs$micro_dl)

      # combine downloaded data
      waiter::waiter_show(html = shiny::tagList(
        waiter::spin_ring(),
        shiny::h4("Combining downloaded microdata")
      ))
      combine_and_save_all_dta(
        dir_in = r6$dirs$micro_dl,
        dir_out = r6$dirs$micro_combine
      )

      # ------------------------------------------------------------------------
      # questionnaire file
      # ------------------------------------------------------------------------

      # inform user that questionnaire metadata is being gotten
      waiter::waiter_show(html = shiny::tagList(
        waiter::spin_ring(),
        shiny::h4("Fetching questionnaire metadata")
      ))

      # download JSON file into an appropriate place
      # if file already exists, name it "old_document.json"
      # if no file exists, name it "document.json"
      get_qnr_json(
        qnr_id = r6$qnr_selected_suso_id,
        qnr_version = as.numeric(r6$qnr_selected_suso_version),
        qnr_dir = r6$dirs$qnr,
        server = r6$server,
        workspace = r6$workspace,
        user = r6$user,
        password = r6$password
      )

      # parse questionnaire JSON file if needed
      parse_json <- decide_whether_to_parse_json(
        qnr_dir = r6$dirs$qnr
      )
      # - if so, do the necessary
      if (parse_json == TRUE) {

        # update user on process
        waiter::waiter_show(
          html = shiny::tagList(
            waiter::spin_ring(),
            shiny::h4("Processing questionnaire metadata")
          )
        )

        # parse and save JSON file
        qnr_file_path <- fs::path(r6$dirs$qnr, "document.json")
        qnr_metadata <- susometa::parse_questionnaire(qnr_file_path)
        saveRDS(
          object = qnr_metadata,
          file = fs::path(r6$dirs$qnr, "qnr_full.rds")
        )

        # create and save variables data frame
        vars_metadata <- extract_vars_metadata(df = qnr_metadata)
        saveRDS(
          object = vars_metadata,
          file = fs::path(r6$dirs$qnr, "qnr_vars.rds")
        )

      }

      # ------------------------------------------------------------------------
      # sync data
      # ------------------------------------------------------------------------

      # inform user that tablet sync data is being gotten
      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4("Fetching sync data by user")
        )
      )

      # get action logs, compute last sync by user, and save both logs and sync
      get_sync_data(
        start_date = r6$svy_start_date,
        dir = r6$dirs$sync,
        server = r6$server,
        workspace = r6$workspace,
        user = r6$user,
        password = r6$password
      )

      # ------------------------------------------------------------------------
      # team composition
      # ------------------------------------------------------------------------

      # inform users that team composition data is being downloaded
      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4("Fetching team composition")
        )
      )

      # fetch team composition table and save it to disk
      get_team_composition(
        dir = r6$dirs$team,
        server = r6$server,
        workspace = r6$workspace,
        user = r6$user,
        password = r6$password
      )

      # ------------------------------------------------------------------------
      # signal that process is complete
      # ------------------------------------------------------------------------

      # signal that download process is complete
      waiter::waiter_show(
        html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4("Data download complete")
        )
      )

      # change flag about whether data downloaded
      r6$data_downloaded <- TRUE

      # write updated R6 to disk
      r6$write()

      # hide the waiter
      waiter::waiter_hide()

      # ========================================================================
      # update UI elements
      # ========================================================================

      # in this module
      # enable the next button
      shinyjs::show(id = "select_action")

      # enable the accordion for downloading data
      shinyjs::enable("download_data")

      # in a parent module
      # indirectly, by signal that data downloaded
      gargoyle::trigger("download_data")

    })

    # ==========================================================================
    # react to action selection
    # ==========================================================================

    shiny::observeEvent(input$select_action, {

      # send signal that an action was selected
      # code for updating UI appears in app_server.R
      gargoyle::trigger("select_action")

      r6$selected_action <- input$select_action

    })


  })
}
    
## To be copied in the UI
# mod_2_data_ui("2_data_1")
    
## To be copied in the server
# mod_2_data_server("2_data_1")
