#' 4_quality_1_setup_2_data_temp_crop_sales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_temp_crop_sales_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = "Parcel-plot-crop roster data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("crop_id_var"),
      label = "Crop ID variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("crop_vals"),
      label = "Crops that are temporary crops",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::selectInput(
      inputId = ns("sold_var"),
      label = "Whether sold any crop variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_val"),
      label = "Value indicating that crop was sold",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("amt_sold_vars"),
      label = "Variables for total value and unit value of sales",
      multiple = TRUE,
      choices = NULL,
      selected = NULL
    ),
    shiny::numericInput(
      inputId = ns("amt_sold_dk_val"),
      label = "Value indicating do not know (DK)",
      value = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_quality_1_setup_2_data_temp_crop_sales Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_temp_crop_sales_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      crop_id_vars = r6$temp_crop_sales_crop_id_var_choices,
      crop_vals = r6$temp_crop_sales_crop_vals_choices,
      sold_vars = r6$temp_crop_sales_sold_var_choices,
      sold_vals = r6$temp_crop_sales_sold_val_choices,
      amt_sold_vars = r6$temp_crop_sales_amt_sold_vars_choices
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
      # but do not trigger reactive
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choices = input_choices$data,
        selected = NULL
      )

      # (re)set to `NULL` variable and value selections
      # but do not trigger reactive
      shiny::freezeReactiveValue(input, "crop_id_var")
      shiny::updateSelectInput(
        inputId = "crop_id_var",
        choices = NULL,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "crop_vals")
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choices = NULL,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "sold_var")
      shiny::updateSelectInput(
        inputId = "sold_var",
        choices = NULL,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "sold_val")
      shiny::updateSelectInput(
        inputId = "sold_val",
        choices = NULL,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "amt_sold_vars")
      shiny::updateSelectInput(
        inputId = "amt_sold_vars",
        choices = NULL,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "amt_sold_dk_val")
      shiny::updateNumericInput(
        inputId = "amt_sold_dk_val",
        value = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load NULL values if vals not previously saved
    # --------------------------------------------------------------------------

    if (is.null(r6$temp_crop_sales_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = NULL
      )

      # crop ID variable
      shiny::freezeReactiveValue(input, "crop_id_var")
      shiny::updateSelectInput(
        inputId = "crop_id_var",
        choice = NULL,
        selected = NULL
      )

      # crop ID values
      shiny::freezeReactiveValue(input, "crop_vals")
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choice = NULL,
        selected = NULL
      )

      # sold variable
      shiny::freezeReactiveValue(input, "sold_var")
      shiny::updateSelectInput(
        inputId = "sold_var",
        choice = NULL,
        selected = NULL
      )

      # sold value
      shiny::freezeReactiveValue(input, "sold_val")
      shiny::updateSelectInput(
        inputId = "sold_val",
        choice = NULL,
        selected = NULL
      )

      # amount sold variables
      shiny::freezeReactiveValue(input, "amt_sold_vars")
      shiny::updateSelectInput(
        inputId = "amt_sold_vars",
        choice = NULL,
        selected = NULL
      )

      # amount sold value
      shiny::freezeReactiveValue(input, "amt_sold_val")
      shiny::updateNumericInput(
        inputId = "amt_sold_val",
        value = NULL
      )

    }

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$temp_crop_sales_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice =r6$data_choices, 
        selected = r6$temp_crop_sales_df
      )

      # crop ID variable
      shiny::freezeReactiveValue(input, "crop_id_var")
      shiny::updateSelectInput(
        inputId = "crop_id_var",
        choice = r6$temp_crop_sales_crop_id_var_choices,
        selected = r6$temp_crop_sales_crop_id_var
      )

      # crop ID values
      shiny::freezeReactiveValue(input, "crop_vals")
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choice = r6$temp_crop_sales_crop_vals_choices,
        selected = r6$temp_crop_sales_crop_vals
      )

      # sold variable
      shiny::freezeReactiveValue(input, "sold_var")
      shiny::updateSelectInput(
        inputId = "sold_var",
        choice = r6$temp_crop_sales_sold_var_choices,
        selected = r6$temp_crop_sales_sold_var
      )

      # sold value
      shiny::freezeReactiveValue(input, "sold_val")
      shiny::updateSelectInput(
        inputId = "sold_val",
        choice = r6$temp_crop_sales_sold_val_choices,
        selected = r6$temp_crop_sales_sold_val
      )

      # amount sold variables
      shiny::freezeReactiveValue(input, "amt_sold_vars")
      shiny::updateSelectInput(
        inputId = "amt_sold_vars",
        choice = r6$temp_crop_sales_amt_sold_vars_choices,
        selected = r6$temp_crop_sales_amt_sold_vars
      )

      # amount sold value
      shiny::freezeReactiveValue(input, "amt_sold_dk_val")
      shiny::updateNumericInput(
        inputId = "amt_sold_dk_val",
        value = r6$temp_crop_sales_amt_sold_dk_val
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

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      # make choices from selected data
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      # make crop ID variable choices
      input_choices$crop_id_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_id_var_choices()

      # make choices for sold variable
      input_choices$sold_vars <- fs::path(
          r6$dirs$micro_combine,
          paste0(input$data, ".dta")
        ) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # make choices for amount sold variables
      input_choices$amt_sold_vars <- fs::path(
          r6$dirs$micro_combine,
          paste0(input$data, ".dta")
        ) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "numeric"
        )

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      # update choices in UI
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      # crop ID
      shiny::freezeReactiveValue(input, "crop_id_var")
      shiny::updateSelectInput(
        inputId = "crop_id_var",
        choices = input_choices$crop_id_vars,
        selected = NULL
      )

      # crop ID values
      shiny::freezeReactiveValue(input, "crop_vals")
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choice = NULL,
        selected = NULL
      )

      # sold variable choices
      shiny::freezeReactiveValue(input, "sold_var")
      shiny::updateSelectInput(
        inputId = "sold_var",
        choices = input_choices$sold_vars,
        selected = NULL
      )

      # sold variable value to `NULL`
      shiny::freezeReactiveValue(input, "sold_val")
      shiny::updateSelectInput(
        inputId = "sold_val",
        choices = NULL,
        selected = NULL
      )

      # amount sold variables
      shiny::freezeReactiveValue(input, "amt_sold_vars")
      shiny::updateSelectInput(
        inputId = "amt_sold_vars",
        choices = input_choices$amt_sold_vars,
        selected = NULL
      )

      # amount sold DK values to `NULL`
      shiny::freezeReactiveValue(input, "amt_sold_val_dk")
      shiny::updateNumericInput(
        inputId = "amt_sold_val_dk",
        value = NULL,
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # crop_id_var -> crop_vals
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$crop_id_var, {

      shiny::req(
        input$data,
        input$crop_id_var
      )

      # load variables data frame from disk
      qnr_vars_df <- fs::path(r6$dirs$qnr, "qnr_vars.rds") |>
        readRDS()

      # make crop ID value choices
      input_choices$crop_id_vals <- make_id_val_options(
        path = fs::path(
          r6$dirs$micro_combine,
          paste0(input$data, ".dta")
        ),
        varname = extract_id_var_names(input$crop_id_var)
      )

      # update choices in UI
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choices = input_choices$crop_id_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # sold_var -> sold_val
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$sold_var, {

      shiny::req(input$sold_var)

      # extract values options sold variable
      input_choices$sold_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$sold_var)
      )

      # update sold value options in UI
      shiny::freezeReactiveValue(input, "sold_val")
      shiny::updateSelectInput(
        inputId = "sold_val",
        choices = input_choices$sold_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6

      # data
      r6$temp_crop_sales_df_choices <- input_choices$data
      r6$temp_crop_sales_df  <- input$data
      # crop ID
      r6$temp_crop_sales_crop_id_var_choices <- input_choices$crop_id_vars
      r6$temp_crop_sales_crop_id_var <- input$crop_id_var
      # crop values
      r6$temp_crop_sales_crop_vals_choices <- input_choices$crop_id_vals
      r6$temp_crop_sales_crop_vals <- input$crop_vals
      # sold variable
      r6$temp_crop_sales_sold_var_choices <- input_choices$sold_vars
      r6$temp_crop_sales_sold_var <- input$sold_var
      # sold value
      r6$temp_crop_sales_sold_val_choices <- input_choices$sold_vals
      r6$temp_crop_sales_sold_val <- input$sold_val
      # amount sold variables
      r6$temp_crop_sales_amt_sold_vars_choices <- input_choices$amt_sold_vars
      r6$temp_crop_sales_amt_sold_vars <- input$amt_sold_vars
      # amount sold DK values
      r6$temp_crop_sales_amt_sold_dk_val <- input$amt_sold_dk_val
      # save action
      r6$temp_crop_sales_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_temp_crop_sales")

    })
  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_temp_crop_sales_ui("4_quality_1_setup_2_data_temp_crop_sales_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_temp_crop_sales_server("4_quality_1_setup_2_data_temp_crop_sales_1")
