#' 4_quality_1_setup_2_data_plot_gps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_plot_gps_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = "Plot data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("gps_var"),
      label = "Question that captures the GPS area measurement",
      choices = NULL,
      selected = NULL
    ),
    shiny::numericInput(
      inputId = ns("not_measured_val"),
      label = "Value of the question indicating the parcel was not measured.",
      value = NULL
    ),
    shiny::selectInput(
      inputId = ns("why_not_measured_var"),
      label = "Question about why the area was not measured.",
      choices = NULL,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_quality_1_setup_2_data_plot_gps Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_plot_gps_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # initialize page
    # ==========================================================================

    # --------------------------------------------------------------------------
    # create reactive container for input choices
    # --------------------------------------------------------------------------

    input_choices <- shiny::reactiveValues(
      data = r6$plot_gps_df_choices,
      gps_var = r6$plot_gps_gps_var_choices,
      not_measured_val = r6$plot_gps_not_measured_val_choices,
      why_not_measured_var = r6$plot_gps_why_not_measured_var_choices
    )

    # --------------------------------------------------------------------------
    # compute choices from downloaded data
    # --------------------------------------------------------------------------

    # when data are downloaded, compute the choices and update the choices
    gargoyle::on("download_data", {

      shiny::req(r6$dirs$micro_combine)

      # get list of data files in combined folder
      input_choices$data <- r6$dirs$micro_combine |>
        make_data_choices()

      # update UI to reflect data choices
      shiny::updateSelectInput(
        inputId = "data",
        choices = input_choices$data,
        selected = NULL
      )

      # (re)set to `NULL` variable and value selections
      shiny::updateSelectInput(
        inputId = "gps_var",
        choices = NULL,
        selected = NULL
      )
      shiny::updateSelectInput(
        inputId = "not_measured_val",
        choices = NULL,
        selected = NULL
      )
      shiny::updateSelectInput(
        inputId = "why_not_measured_var",
        choices = NULL,
        selected = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$plot_gps_provided)) {

      shiny::req(
        r6$plot_gps_df_choices, r6$plot_gps_df,
        r6$plot_gps_gps_var_choices, r6$plot_gps_gps_var,
        r6$plot_gps_not_measured_val_choices, r6$plot_gps_not_measured_val,
        r6$plot_gps_why_not_measured_var_choices, r6$plot_gps_why_not_measured_var,
        r6$plot_gps_provided
      )

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$plot_gps_df_choices,
        selected = r6$plot_gps_df
      )

      # GPS area variable
      shiny::freezeReactiveValue(input, "gps_var")
      shiny::updateSelectInput(
        inputId = "gps_var",
        choice = r6$plot_gps_gps_var_choices,
        selected = r6$plot_gps_gps_var
      )

      # GPS area not measured value
      shiny::freezeReactiveValue(input, "not_measured_val")
      shiny::updateSelectInput(
        inputId = "not_measured_val",
        choice = r6$plot_gps_not_measured_val_choices,
        selected = r6$plot_gps_not_measured_val
      )

      # why parcel not measured variable
      shiny::freezeReactiveValue(input, "why_not_measured_var")
      shiny::updateSelectInput(
        inputId = "why_not_measured_var",
        choice = r6$plot_gps_why_not_measured_var_choices,
        selected = r6$plot_gps_why_not_measured_var
      )

    }

    # ==========================================================================
    # react to selections
    # ==========================================================================

    # --------------------------------------------------------------------------
    # data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$data, {

      # load variables data frame from disk
      qnr_vars_df <- fs::path(r6$dirs$qnr, "qnr_vars.rds") |>
        readRDS()

      # get variable names from selected file
      input_choices$gps_var <- fs::path(
          r6$dirs$micro_combine,
          paste0(input$data, ".dta")
        ) |>
        make_data_var_choices(
          vars_df = qnr_vars_df,
          var_type = "numeric"
        )

      # update GPS measurement var in UI
      shiny::updateSelectInput(
        inputId = "gps_var",
        choices = input_choices$gps_var,
        selected = NULL
      )

      # update why not measured vals to `NULL`, since prior choice invalidated
      shiny::updateSelectInput(
        inputId = "not_measured_val",
        choices = NULL,
        selected = NULL
      )

      # get categorical questions for why not measured
      input_choices$why_not_measured_var <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_data_var_choices(
          vars_df = qnr_vars_df,
          var_type = "categorical"
        ) |>
        # add "None" in case there is no such question
        append(
          values = c("None"),
          after = 0
        )

      # update why parcel not measured choices
      shiny::updateSelectInput(
        inputId = "why_not_measured_var",
        choices = input_choices$why_not_measured_var,
        selected = NULL
      )

    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # GPS measurement variable
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$gps_var, {

      shiny::req(
        r6$dirs$qnr,
        input$gps_var
      )

      # load questionnaire data frame from disk
      qnr_df <- fs::path(r6$dirs$qnr, "qnr_full.rds") |>
        readRDS()

      # extract variable from variable choice character string
      var_selected <- extract_var_names(input$gps_var)

      # get use value answer options
      input_choices$not_measured_val <- make_val_options(
        qnr_df = qnr_df,
        varname = var_selected
      )

      # update area not measured value label to show what's found in the data
      shiny::observeEvent(input_choices$not_measured_val, {

        # compose the label message based on the answer options found for the
        # GPS variable selected
        value_label <- shiny::reactive(

          dplyr::if_else(
            condition = !is.na(input_choices$not_measured_val),
            true = glue::glue_collapse(
              x = c(
                "Value of the question indicating the plot was not measured.",
                "NOTE: labelled value found for this variable.",
                input_choices$not_measured_val
              ),
              # NOTE: would prefer a line break,
              # but that seems to require rendering the UI in the server
              # because `updateNumericInput()` sends a signal to update
              # elements in the existing UI rather than update it
              sep = " "
            ),
            false =
              "Value of the question indicating the plot was not measured.",
            missing =
              "Value of the question indicating the plot was not measured."
          )

        )

        # send signal to update label element in UI
        shiny::updateNumericInput(
          inputId = "not_measured_val",
          label = value_label()
        )

      })

    }, ignoreInit = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6
      # data
      r6$plot_gps_df_choices <- input_choices$data
      r6$plot_gps_df <- input$data
      # GPS measurement variable
      r6$plot_gps_gps_var_choices <- input_choices$gps_var
      r6$plot_gps_gps_var <- input$gps_var
      # not measured values
      r6$plot_gps_not_measured_val_choices <- as.character(
        input_choices$not_measured_val
      )
      r6$plot_gps_not_measured_val <- as.character(
        input$not_measured_val
      )
      # why not measured variable
      r6$plot_gps_why_not_measured_var_choices <-
        input_choices$why_not_measured_var
      r6$plot_gps_why_not_measured_var <- input$why_not_measured_var
      # save action
      r6$plot_gps_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_plot_gps")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_plot_gps_ui("4_quality_1_setup_2_data_plot_gps_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_plot_gps_server("4_quality_1_setup_2_data_plot_gps_1")
