#' 0_about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_0_about_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    bslib::accordion(
      id = ns("about_app"),
      bslib::accordion_panel(
        title = "Overview",
        value = "overview",
        shiny::tags$p(
          "
          This app helps you monitor your 50x30 survey, with
          one tab for each major action.
          "
        ),
        shiny::tags$p(
          "Click the buttons below to learn more"
        ),
        shiny::actionButton(
          inputId = ns("open_setup"),
          label = "Setup"
        ),
        shiny::actionButton(
          inputId = ns("open_data"),
          label = "Data"
        ),
        shiny::actionButton(
          inputId = ns("open_completeness"),
          label = "Completeness"
        ),
        shiny::actionButton(
          inputId = ns("open_quality"),
          label = "Quality"
        )
      ),
      bslib::accordion_panel(
        title = "Setup tab",
        value = "setup_panel",
          "
          Where the app captures the core information needed to help you:
          credentials for connecting to your Survey Solutions server;
          details on the Survey Solutions questionnaire(s) of interest;
          information on the questionnaire template survey visit, etc.
          "
      ),
      bslib::accordion_panel(
        title = "Data tab",
        value = "data_panel",
          "
          Where the app downloads data for your survey.
          Once downloaded, these data are available for the app to help you
          (e.g., create reports)
          and
          for you to download and use outside of the app
          (e.g. run high-frequency checks).
          "
      ),
      bslib::accordion_panel(
        title = "Completeness tab",
        value = "completeness_panel",
          "
          Where the app creates a customized report to track survey progress
          and completeness of data by primarily sampling unit (PSU).
          Before generating a report, one must provide some details
          about the survey (e.g., sample size, number of PSUs, workoad
          per team, etc.).
          After those details have been provided. the app can produce a report.
          "
      ),
      bslib::accordion_panel(
        title = "Quality tab",
        value = "quality_panel",
          "
          Where the app generates a report to monitor data quality indicators
          that may indicate cheating or misunderstanding on the part of
          interviewers.
          Before generating the report, one selects the desired data quality
          indicators from menu of survey-specific options.
          Once those selections are made, the app can generate a report.
          "
      )

    )

  )
}
    
#' 0_about Server Functions
#'
#' @noRd 
mod_0_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # setup
    shiny::observeEvent(input$open_setup, {
      bslib::accordion_panel_open(
        id = "about_app",
        value = "setup_panel"
      )
    })

    # data
    shiny::observeEvent(input$open_data, {
      bslib::accordion_panel_open(
        id = "about_app",
        value = "data_panel"
      )
    })

    # completeness
    shiny::observeEvent(input$open_completeness, {
      bslib::accordion_panel_open(
        id = "about_app",
        value = "completeness_panel"
      )
    })

    # quality
    shiny::observeEvent(input$open_quality, {
      bslib::accordion_panel_open(
        id = "about_app",
        value = "quality_panel"
      )
    })

  })
}
    
## To be copied in the UI
# mod_0_about_ui("0_about_1")
    
## To be copied in the server
# mod_0_about_server("0_about_1")
