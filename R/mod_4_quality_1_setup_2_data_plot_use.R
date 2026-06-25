#' 4_quality_1_setup_2_data_plot_use UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_plot_use_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = info_popover(
        lbl = "Data: plots",
        desc = paste(
          "Roster of plots,",
          "from the section typically named 'Plot roster and details'"
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("plot_use_var"),
      label = info_popover(
        lbl = "Question: How the plot was used",
        desc = "For example, whether it was cultivated, left fallow, etc."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}

#' 4_quality_1_setup_2_data_plot_use Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_plot_use_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      plot_use_var = r6$plot_use_plot_use_var_choices
    )

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # --------------------------------------------------------------------------
    # compute choices from downloaded data
    # --------------------------------------------------------------------------

    # when data are downloaded, compute the choices and update the choices
    gargoyle::on("download_data", {

      # update UI to reflect data choices
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choices = input_choices$data,
        selected = NULL
      )

      # (re)set to `NULL` variable and value selections
      shiny::freezeReactiveValue(input, "plot_use_var")
      shiny::updateSelectInput(
        inputId = "plot_use_var",
        choices = NULL,
        selected = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load null values since values not previously saved
    # --------------------------------------------------------------------------

    if (is.null(r6$plot_use_provided)) {

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
      shiny::freezeReactiveValue(input, "plot_use_var")
      shiny::updateSelectInput(
        inputId = "plot_use_var",
        choices = NULL,
        selected = NULL
      )

    }

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$plot_use_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = r6$plot_use_df
      )

      # plot use variable
      shiny::freezeReactiveValue(input, "plot_use_var")
      shiny::updateSelectInput(
        inputId = "plot_use_var",
        choice = r6$plot_use_plot_use_var_choices,
        selected = r6$plot_use_plot_use_var
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

      # get variable names from selected file
      input_choices$plot_use_var <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # update plot use var in UI
      shiny::updateSelectInput(
        inputId = "plot_use_var",
        choices = input_choices$plot_use_var,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      shiny::req(input$data, input$plot_use_var)

      # capture values in R6
      # data
      r6$plot_use_df_choices <- input_choices$data
      r6$plot_use_df <- input$data
      # plot use variable
      r6$plot_use_plot_use_var_choices <- input_choices$plot_use_var
      r6$plot_use_plot_use_var <- input$plot_use_var
      # save action
      r6$plot_use_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_plot_use")

    })

  })
}

## To be copied in the UI
# mod_4_quality_1_setup_2_data_plot_use_ui("4_quality_1_setup_2_data_plot_use_1")

## To be copied in the server
# mod_4_quality_1_setup_2_data_plot_use_server("4_quality_1_setup_2_data_plot_use_1")
