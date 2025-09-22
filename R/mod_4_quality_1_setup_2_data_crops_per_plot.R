#' 4_quality_1_setup_2_data_crops_per_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_crops_per_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = "Crop roster data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("parcel_id_var"),
      label = "Parcel ID variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("plot_id_var"),
      label = "Plot ID variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_quality_1_setup_2_data_crops_per_plot Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_crops_per_plot_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      parcel_id_var = r6$crops_per_plot_parcel_id_var_choices,
      plot_id_var = r6$crops_per_plot_plot_id_var_choices
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
      shiny::freezeReactiveValue(input, "parcel_id_var")
      shiny::updateSelectInput(
        inputId = "parcel_id_var",
        choices = NULL,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "plot_id_var")
      shiny::updateSelectInput(
        inputId = "plot_id_var",
        choices = NULL,
        selected = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load NULL values if never saved
    # --------------------------------------------------------------------------

    if (is.null(r6$crops_per_plot_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = NULL
      )

      # parcel ID
      shiny::freezeReactiveValue(input, "parcel_id_var")
      shiny::updateSelectInput(
        inputId = "parcel_id_var",
        choice = NULL,
        selected = NULL
      )

      # plot ID
      shiny::freezeReactiveValue(input, "plot_id_var")
      shiny::updateSelectInput(
        inputId = "plot_id_var",
        choice = NULL,
        selected = NULL
      )

    }

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$crops_per_plot_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = r6$crops_per_plot_df
      )

      # parcel ID
      shiny::freezeReactiveValue(input, "parcel_id_var")
      shiny::updateSelectInput(
        inputId = "parcel_id_var",
        choice = r6$crops_per_plot_parcel_id_var_choices,
        selected = r6$crops_per_plot_parcel_id_var
      )

      # plot ID
      shiny::freezeReactiveValue(input, "plot_id_var")
      shiny::updateSelectInput(
        inputId = "plot_id_var",
        choice = r6$crops_per_plot_plot_id_var_choices,
        selected = r6$crops_per_plot_plot_id_var
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

      # make ID variables choices
      # parcel
      input_choices$parcel_id_var <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_id_var_choices()
      # plot
      input_choices$plot_id_var <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_id_var_choices()

      # update in UI
      # parcel ID var
      shiny::updateSelectInput(
        inputId = "parcel_id_var",
        choices = input_choices$parcel_id_var,
        selected = NULL
      )
      # parcel ID var
      shiny::updateSelectInput(
        inputId = "plot_id_var",
        choices = input_choices$plot_id_var,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6
      # data
      r6$crops_per_plot_df_choices <- input_choices$data
      r6$crops_per_plot_df <- input$data
      # parcel ID
      r6$crops_per_plot_parcel_id_var_choices <- input_choices$parcel_id_var
      r6$crops_per_plot_parcel_id_var <- input$parcel_id_var
      # plot ID
      r6$crops_per_plot_plot_id_var_choices <- input_choices$plot_id_var
      r6$crops_per_plot_plot_id_var <- input$plot_id_var
      # save action
      r6$crops_per_plot_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_crops_per_plot")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_crops_per_plot_ui("4_quality_1_setup_2_data_crops_per_plot_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_crops_per_plot_server("4_quality_1_setup_2_data_crops_per_plot_1")
