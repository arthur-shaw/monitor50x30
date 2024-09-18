#' 2_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_data_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        # elements in the sidebar
        id = ns("download_data0"),
        title = "",
        position = "left",
        width = "30%",
        open = FALSE,
        # contents
        bslib::accordion(
          id = ns("download_data"),
          bslib::accordion_panel(
            title = "Download data",
            ## Button: Interview microdata
            shiny::downloadButton(
              outputId = ns("interview_microdata"),
              label = "Interview microdata",
              width = "100%"
            ),
            ## Button: Questionnaire metadata
            shiny::downloadButton(
              outputId = ns("questionnaire_metadata"),
              label = "Questionnaire metadata",
              width = "100%"
            ),
            ## Button: Team composition
            shiny::downloadButton(
              outputId = ns("team_composition"),
              label = "Team composition",
              width = "100%"
            ),

            ## Button: Sync dates
            shiny::downloadButton(
              outputId = ns("sync_dates"),
              label = "Sync dates",
              width = "100%"
            )
          )
        )
      ),
      # settings
      fillable = FALSE,
      # contents
      shiny::tags$p(""),
      shiny::fluidRow(
        style = "height: 15vh"
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          align = "center",
          shiny::htmlOutput(ns("last_download_data_warning"))
        )
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          offset = 4,
          align = "center",
          shiny::actionButton(
            inputId = ns("fetch_data"),
            label = "Fetch data",
            width = "100%"
          )
        )
      ),
      shiny::fluidRow(
        style = "height: 15vh"
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          align = "center",
          shiny::htmlOutput(ns("navigation_explainer"))
        )
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          align = "center",
          shiny::actionButton(
            inputId = ns("go_to_completeness"),
            label = "Data Completeness Report",
            width = "50%",
            icon = icon("arrow-right")
          )
        ),
        shiny::column(
          width = 6,
          align = "center",
          shiny::actionButton(
            inputId = ns("go_to_quality"),
            label = "Data Quality Report",
            width = "50%",
            icon = icon("arrow-right")
          )
        )
        # shiny::column(
        #   width = 4,
        #   offset = 4,
        #   shiny::selectInput(
        #     inputId = ns("select_action"),
        #     label = "Select the action to take",
        #     choices = c(
        #       "Data completeness report",
        #       "Data quality report"
        #     ),
        #     selected = NULL,
        #     width = "100%"
        #   )
        # )
      )
    )
  )
}

#' 2_data Server Functions
#'
#' @noRd
mod_2_data_server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # ==========================================================================
    # navigation buttons
    # ==========================================================================

    # if data exists and it is up to date

    if (2 == 2) { ## placeholder
      #  show the navigation buttons (default)
      #  display the navigation explainer
      output$navigation_explainer <- shiny::renderUI({
        shiny::HTML(
          "
          Once you've fetched the data, you can click the buttons below to view the completeness report or
          data quality report.
          "
        )
      })
    } else {
      #  else hide them
      shinyjs::hide(id = "go_to_completeness")
      shinyjs::hide(id = "go_to_quality")
    }

    # ==========================================================================
    # last data download warning
    # ==========================================================================

    output$last_download_data_warning <- shiny::renderUI({

      combined_microdata_path = create_data_dirs(r6$app_dir)$micro_combine_dir
      last_download_date = get_last_data_download_date(combined_microdata_path)

      # print(last_download_date)

      if(!is.null(last_download_date[[1]])){
        time_diff_in_hours = difftime(Sys.time(),last_download_date[[1]], units = "hours")
        time_diff_in_hours = round(as.numeric(trimws(gsub("Time difference of|hours", "", time_diff_in_hours))),0)
      }else{
        time_diff_in_hours = 0
      }

      if(time_diff_in_hours == 0){
        "Please click the button below to fetch the data."
      }else{
        if(time_diff_in_hours >= 1 & time_diff_in_hours < 24){
        shiny::HTML(
          paste0(
            "You last downloaded data ",
            time_diff_in_hours,
            " hours ago."
          )

        )
      }else{
        shiny::HTML(
          paste0(
            "
          You last downloaded data on
          ",
            last_download_date[[2]]
            ,
            ". Please click the button below to get the most recent data."
          )

        )
      }
      }


    })


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
      # create directories to hold data
      # ------------------------------------------------------------------------

      dirs <- create_data_dirs(app_dir = r6$app_dir)

      # ------------------------------------------------------------------------
      # delete stale files from previous session
      # ------------------------------------------------------------------------

      # microdata
      delete_stale_data(dir = dirs$micro_dl_dir)
      delete_stale_data(dir = dirs$micro_combine_dir)
      # sync data
      delete_stale_data(dir = dirs$sync_dir)
      # team composition
      delete_stale_data(dir = dirs$team_dir)

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
      for (qnr_id in qnr_ids) {
        # extract title of current questionnaire
        qnr_title <- qnr_titles[qnr_n]

        # update user on which data is being downloaded
        waiter::waiter_show(html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4(
            glue::glue("
            Downloading data for questionnaire {qnr_n} of {qnr_tot} :
            {qnr_title}")
          )
        ))

        # download data
        susoflows::download_data(
          qnr_id = qnr_id,
          export_type = "STATA",
          path = dirs$micro_dl_dir,
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
      unpack_all_zip_to_dir(dirs$micro_dl_dir)

      # combine downloaded data
      waiter::waiter_show(html = shiny::tagList(
        waiter::spin_ring(),
        shiny::h4("Combining downloaded microdata")
      ))
      combine_and_save_all_dta(
        dir_in = dirs$micro_dl_dir,
        dir_out = dirs$micro_combine_dir
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
        qnr_dir = dirs$qnr_dir,
        server = r6$server,
        workspace = r6$workspace,
        user = r6$user,
        password = r6$password
      )

      # parse questionnaire JSON file if needed
      parse_json <- decide_whether_to_parse_json(
        qnr_dir = dirs$qnr_dir
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
        qnr_file_path <- fs::path(dirs$qnr_dir, "document.json")
        qnr_metadata <- susometa::parse_questionnaire(qnr_file_path)
        saveRDS(
          object = qnr_metadata,
          file = fs::path(dirs$qnr_dir, "qnr_full.rds")
        )

        # create and save variables data frame
        vars_metadata <- extract_vars_metadata(df = qnr_metadata)
        saveRDS(
          object = vars_metadata,
          file = fs::path(dirs$qnr_dir, "qnr_vars.rds")
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
        dir = dirs$sync_dir,
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
        dir = dirs$team_dir,
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

      # show the text that explains where to go next after fetching the data

      output$navigation_explainer <- shiny::renderUI({
        shiny::HTML(
          "
          Once you've fetched the data, you can click the buttons below to view the completeness report or
          data quality report.
          "
        )
      })

      # enable the navigation buttons
      shinyjs::show(id = "go_to_completeness")
      shinyjs::show(id = "go_to_quality")

      # shinyjs::show(id = "select_action")

      ## Hide the last download date info
      shinyjs::hide(id = "last_download_data_warning")
    })

    # ==========================================================================
    # react to buttons in the sidebar clicked
    # ==========================================================================

    output_ids = c("interview_microdata", "questionnaire_metadata", "team_composition", "sync_dates")
    file_dirs = c("micro_combine_dir", "qnr_dir", "team_dir", "sync_dir")

    # output$sync_dates <- downloadHandler(
    #
    #   filename = function() {
    #     paste("sync_dates","-", Sys.Date(), '.zip', sep='')
    #   },
    #   content = function(file) {
    #     zip::zip(file,
    #       files = paste(create_data_dirs(r6$app_dir)$sync_dir),
    #       mode = "cherry-pick")
    #   }
    # )

    lapply(1:4, function(i){
      output[[output_ids[i]]] <- downloadHandler(

        filename = function() {
          paste(output_ids[i],"-", Sys.Date(), '.zip', sep='')
        },
        content = function(file) {
          zip::zip(file, files = paste(create_data_dirs(r6$app_dir)[[file_dirs[i]]]), mode = "cherry-pick")
        }
      )
    })

    # ==========================================================================
    # react to Data Completeness Report button clicked
    # ==========================================================================

    shiny::observeEvent(input$go_to_completeness, {
      # send signal that this button was clicked
      # code for updating UI appears in app_server.R
      gargoyle::trigger("go_to_completeness_btn")

      r6$go_to_completeness_btn <- input$go_to_completeness
    })


    # ==========================================================================
    # react to Data Quality Report button clicked
    # ==========================================================================

    shiny::observeEvent(input$go_to_quality, {
      # send signal that this button was clicked
      # code for updating UI appears in app_server.R
      gargoyle::trigger("go_to_quality_btn")

      r6$go_to_quality_btn <- input$go_to_quality
    })


  })
}

## To be copied in the UI
# mod_2_data_ui("2_data_1")

## To be copied in the server
# mod_2_data_server("2_data_1")
