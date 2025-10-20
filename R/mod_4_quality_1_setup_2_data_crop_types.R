#' 4_quality_1_setup_2_data_crop_types UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_crop_types_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = "Crop roster data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("crop_type_var"),
      label = paste0(
        "Question/variable that captures the crop type ",
        "(i.e., whether temporary or tree/permanent)"
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::numericInput(
      inputId = ns("temp_crop_val"),
      label = "Value indicating a temporary crop",
      value = NULL
    ),
    shiny::numericInput(
      inputId = ns("perm_crop_val"),
      label = "Value indicating a permanent crop",
      value = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_quality_1_setup_2_data_crop_types Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_crop_types_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      crop_type_var = r6$crop_types_var_choices
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
      shiny::freezeReactiveValue(input, "crop_type_var")
      shiny::updateSelectInput(
        inputId = "crop_type_var",
        choices = NULL,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "temp_crop_val")
      shiny::updateNumericInput(
        inputId = "temp_crop_val",
        value = NULL
      )
      shiny::freezeReactiveValue(input, "perm_crop_val")
      shiny::updateNumericInput(
        inputId = "perm_crop_val",
        value = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load NULL values when not previously saved
    # --------------------------------------------------------------------------

    if (is.null(r6$crop_types_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = NULL
      )

      # crop type variable
      shiny::freezeReactiveValue(input, "crop_type_var")
      shiny::updateSelectInput(
        inputId = "crop_type_var",
        choice = NULL,
        selected = NULL
      )

      # temporary crop value
      shiny::freezeReactiveValue(input, "temp_crop_val")
      shiny::updateNumericInput(
        inputId = "temp_crop_val",
        value = NULL
      )

      # permanent crop value
      shiny::freezeReactiveValue(input, "perm_crop_val")
      shiny::updateNumericInput(
        inputId = "perm_crop_val",
        value = NULL
      )

    }

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$crop_types_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = r6$crop_types_df
      )

      # crop type variable
      shiny::freezeReactiveValue(input, "crop_type_var")
      shiny::updateSelectInput(
        inputId = "crop_type_var",
        choice = r6$crop_types_var_choices,
        selected = r6$crop_types_var
      )

      # temporary crop value
      shiny::freezeReactiveValue(input, "temp_crop_val")
      shiny::updateNumericInput(
        inputId = "temp_crop_val",
        value = r6$crop_type_temp_val
      )

      # permanent crop value
      shiny::freezeReactiveValue(input, "perm_crop_val")
      shiny::updateNumericInput(
        inputId = "perm_crop_val",
        value = r6$crop_type_perm_val
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

      # make crop choice var choices
      input_choices$crop_type_var <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # update in UI
      # crop type variable
      shiny::freezeReactiveValue(input, "crop_type_var")
      shiny::updateSelectInput(
        inputId = "crop_type_var",
        choices = input_choices$crop_type_var,
        selected = NULL
      )
      # temporary crop value
      shiny::freezeReactiveValue(input, "temp_crop_val")
      shiny::updateNumericInput(
        inputId = "temp_crop_val",
        value = NULL
      )
      # permanent crop value
      shiny::freezeReactiveValue(input, "perm_crop_val")
      shiny::updateNumericInput(
        inputId = "perm_crop_val",
        value = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6
      # data
      r6$crop_types_df_choices <- input_choices$data
      r6$crop_types_df <- input$data
      # crop type variable
      r6$crop_types_var_choices <- input_choices$crop_type_var
      r6$crop_types_var <- input$crop_type_var
      # temporary crop value
      r6$crop_type_temp_val <- input$temp_crop_val
      # permanent crop value
      r6$crop_type_perm_val <- input$perm_crop_val
      # save action
      r6$crop_types_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_crop_types")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_crop_types_ui("4_quality_1_setup_2_data_crop_types_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_crop_types_server("4_quality_1_setup_2_data_crop_types_1")
