#' 4_quality_1_setup_2_data_perm_crop_sales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_perm_crop_sales_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = label_tooltip(
        lbl = "Data: Permanent crop destination.",
        desc = paste(
          "Roster of parcel-plot-crop observations.",
          "Named `harvestedCropsPermanent` in the public SuSo application."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("crop_id_var"),
      label = label_tooltip(
        lbl = "Variable: Crop ID",
        desc = "System-generated ID variable that identifies crops."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("crop_vals"),
      label = label_tooltip(
        lbl = "Values: crops that are permanent crops.",
        desc = "Select all that apply."
      ),
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::selectInput(
      inputId = ns("sold_var"),
      label = label_tooltip(
        lbl = "Question: Whether sold any of the crop.",
        desc = paste(
          "Typically, 'sell any' is in the question text",
          "of the target question.",
          "Type that to narrow the list of candidate questions."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_val"),
      label = label_tooltip(
        lbl = "Value: Crop sold",
        desc = "Typically, the value is 1 for 'Yes'."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("amt_sold_vars"),
      label = label_tooltip(
        lbl = "Question(s): value of sales.",
        desc = paste(
          "In some cases, a single variable.",
          "In others, one variable for total value, another for unit value."
        )
      ),
      multiple = TRUE,
      choices = NULL,
      selected = NULL
    ),
    shiny::numericInput(
      inputId = ns("amt_sold_dk_val"),
      label = label_tooltip(
        lbl = "Value: 'Do not know' value for sales",
        desc = paste(
          "Often, the questionnaire provides a code for a 'do not know' code.",
          "This code might be a special value in Designer,",
          "an interview instruction, or a note in the interviewer manual."
        )
      ),
      value = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_quality_1_setup_2_data_perm_crop_sales Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_perm_crop_sales_server <- function(id, r6, parent){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      crop_id_vars = r6$perm_crop_sales_crop_id_var_choices,
      crop_vals = r6$perm_crop_sales_crop_vals_choices,
      sold_vars = r6$perm_crop_sales_sold_var_choices,
      sold_vals = r6$perm_crop_sales_sold_val_choices,
      amt_sold_vars = r6$perm_crop_sales_amt_sold_vars_choices
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
        choices = r6$data_choices,
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
    # load NULL values when values not yet saved
    # --------------------------------------------------------------------------

    if (is.null(r6$perm_crop_sales_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = r6$perm_crop_sales_df
      )

      # crop ID variable
      shiny::freezeReactiveValue(input, "crop_id_var")
      shiny::updateSelectInput(
        inputId = "crop_id_var",
        choice = r6$perm_crop_sales_crop_id_var_choices,
        selected = r6$perm_crop_sales_crop_id_var
      )

      # crop ID values
      shiny::freezeReactiveValue(input, "crop_vals")
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choice = r6$perm_crop_sales_crop_vals_choices,
        selected = r6$perm_crop_sales_crop_vals
      )

      # sold variable
      shiny::freezeReactiveValue(input, "sold_var")
      shiny::updateSelectInput(
        inputId = "sold_var",
        choice = r6$perm_crop_sales_sold_var_choices,
        selected = r6$perm_crop_sales_sold_var
      )

      # sold value
      shiny::freezeReactiveValue(input, "sold_val")
      shiny::updateSelectInput(
        inputId = "sold_val",
        choice = r6$perm_crop_sales_sold_val_choices,
        selected = r6$perm_crop_sales_sold_val
      )

      # amount sold variables
      shiny::freezeReactiveValue(input, "amt_sold_vars")
      shiny::updateSelectInput(
        inputId = "amt_sold_vars",
        choice = r6$perm_crop_sales_amt_sold_vars_choices,
        selected = r6$perm_crop_sales_amt_sold_vars
      )

      # amount sold value
      shiny::freezeReactiveValue(input, "amt_sold_dk_val")
      shiny::updateNumericInput(
        inputId = "amt_sold_dk_val",
        value = r6$perm_crop_sales_amt_sold_dk_val
      )

    }

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$perm_crop_sales_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = r6$perm_crop_sales_df
      )

      # crop ID variable
      shiny::freezeReactiveValue(input, "crop_id_var")
      shiny::updateSelectInput(
        inputId = "crop_id_var",
        choice = r6$perm_crop_sales_crop_id_var_choices,
        selected = r6$perm_crop_sales_crop_id_var
      )

      # crop ID values
      shiny::freezeReactiveValue(input, "crop_vals")
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choice = r6$perm_crop_sales_crop_vals_choices,
        selected = r6$perm_crop_sales_crop_vals
      )

      # sold variable
      shiny::freezeReactiveValue(input, "sold_var")
      shiny::updateSelectInput(
        inputId = "sold_var",
        choice = r6$perm_crop_sales_sold_var_choices,
        selected = r6$perm_crop_sales_sold_var
      )

      # sold value
      shiny::freezeReactiveValue(input, "sold_val")
      shiny::updateSelectInput(
        inputId = "sold_val",
        choice = r6$perm_crop_sales_sold_val_choices,
        selected = r6$perm_crop_sales_sold_val
      )

      # amount sold variables
      shiny::freezeReactiveValue(input, "amt_sold_vars")
      shiny::updateSelectInput(
        inputId = "amt_sold_vars",
        choice = r6$perm_crop_sales_amt_sold_vars_choices,
        selected = r6$perm_crop_sales_amt_sold_vars
      )

      # amount sold value
      shiny::freezeReactiveValue(input, "amt_sold_dk_val")
      shiny::updateNumericInput(
        inputId = "amt_sold_dk_val",
        value = r6$perm_crop_sales_amt_sold_dk_val
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
      shiny::freezeReactiveValue(input, "amt_sold_dk_val")
      shiny::updateNumericInput(
        inputId = "amt_sold_dk_val",
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

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      # make choices from selected data
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      # make crop ID value choices
      input_choices$crop_vals <- make_id_val_options(
        path = fs::path(
          r6$dirs$micro_combine,
          paste0(input$data, ".dta")
        ),
        varname = extract_id_var_names(input$crop_id_var)
      )

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      # update choices in UI
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      # update choices in UI
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choices = input_choices$crop_vals,
        selected = NULL
      )


    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    # --------------------------------------------------------------------------
    # sold variable
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$sold_var, {

      shiny::req(input$sold_var)

      # extract values options sold variable
      input_choices$sold_vals <- make_val_options(
        json_path = r6$json_path,
        categories_dir = r6$categories_dir,
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
      r6$perm_crop_sales_df_choices <- input_choices$data
      r6$perm_crop_sales_df  <- input$data
      # crop ID
      r6$perm_crop_sales_crop_id_var_choices <- input_choices$crop_id_vars
      r6$perm_crop_sales_crop_id_var <- input$crop_id_var
      # crop values
      r6$perm_crop_sales_crop_vals_choices <- input_choices$crop_vals
      r6$perm_crop_sales_crop_vals <- input$crop_vals
      # sold variable
      r6$perm_crop_sales_sold_var_choices <- input_choices$sold_vars
      r6$perm_crop_sales_sold_var <- input$sold_var
      # sold value
      r6$perm_crop_sales_sold_val_choices <- input_choices$sold_vals
      r6$perm_crop_sales_sold_val <- input$sold_val
      # amount sold variables
      r6$perm_crop_sales_amt_sold_vars_choices <- input_choices$amt_sold_vars
      r6$perm_crop_sales_amt_sold_vars <- input$amt_sold_vars
      # amount sold DK values
      r6$perm_crop_sales_amt_sold_dk_val <- input$amt_sold_dk_val
      # save action
      r6$perm_crop_sales_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_perm_crop_sales")

    })
  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_perm_crop_sales_ui("4_quality_1_setup_2_data_perm_crop_sales_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_perm_crop_sales_server("4_quality_1_setup_2_data_perm_crop_sales_1")
