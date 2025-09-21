#' 4_quality_1_setup_2_data_plots_per_parcel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_plots_per_parcel_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = "Plot data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("parcel_id_var"),
      label = "Parcel ID variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}

#' 4_quality_1_setup_2_data_plots_per_parcel Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_plots_per_parcel_server <- function(
  id, parent, r6
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      parcel_id_var = r6$plots_per_parcel_gps_var_choices
    )

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # --------------------------------------------------------------------------
    # compute choices from downloaded data
    # --------------------------------------------------------------------------

    # when data are downloaded, compute the choices and update the choices
    gargoyle::on("download_data", {

      shiny::req(r6$dirs$micro_combine)

      # update UI to reflect data choices
      shiny::updateSelectInput(
        inputId = "data",
        choices = input_choices$data,
        selected = NULL
      )

      # (re)set to `NULL` variable and value selections
      shiny::updateSelectInput(
        inputId = "parcel_id_var",
        choices = NULL,
        selected = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load null values since values not previously saved
    # --------------------------------------------------------------------------

    if (is.null(r6$plots_per_parcel_provided)) {

      # update UI to reflect data choices
      # but do not trigger reactive
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choices = r6$data_choices,
        selected = NULL
      )

      # (re)set to `NULL` variable and value selections
      # but do not trigger reactive
      shiny::freezeReactiveValue(input, "parcel_id_var")
      shiny::updateSelectInput(
        inputId = "parcel_id_var",
        choices = NULL,
        selected = NULL
      )

    }


    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$plots_per_parcel_provided)) {

      shiny::req(
        r6$plots_per_parcel_df_choices, r6$plots_per_parcel_df,
        r6$plots_per_parcel_parcel_id_var_choices, r6$plots_per_parcel_parcel_id_var,
        r6$plots_per_parcel_provided
      )

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choices = r6$data_choices,
        selected = r6$plots_per_parcel_df
      )

      # GPS area variable
      shiny::freezeReactiveValue(input, "parcel_id_var")
      shiny::updateSelectInput(
        inputId = "parcel_id_var",
        choices = r6$plots_per_parcel_parcel_id_var_choices,
        selected = r6$plots_per_parcel_parcel_id_var
      )

    }

    # ==========================================================================
    # react to selections
    # ==========================================================================

    # --------------------------------------------------------------------------
    # data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$data, {

      shiny::req(input$data)

      input_choices$parcel_id_var <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_id_var_choices()

      # update parcel ID var in UI
      shiny::updateSelectInput(
        inputId = "parcel_id_var",
        choices = input_choices$parcel_id_var,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6
      # data
      r6$plots_per_parcel_df_choices <- input_choices$data
      r6$plots_per_parcel_df <- input$data
      # GPS measurement variable
      r6$plots_per_parcel_parcel_id_var_choices <- input_choices$parcel_id_var
      r6$plots_per_parcel_parcel_id_var <- input$parcel_id_var
      # save action
      r6$plots_per_parcel_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_plots_per_parcel")

    })

  })
}

## To be copied in the UI
# mod_4_quality_1_setup_2_data_plots_per_parcel_ui("4_quality_1_setup_2_data_plots_per_parcel_1")

## To be copied in the server
# mod_4_quality_1_setup_2_data_plots_per_parcel_server("4_quality_1_setup_2_data_plots_per_parcel_1")
