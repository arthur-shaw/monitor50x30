#' 4_quality_1_setup_2_data_process_crop_prod UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_process_crop_prod_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("hhold_df"),
      label = label_tooltip(
        lbl = "Data: Households.",
        desc = "The main, household-level data set."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("temp_crops_var"),
      label = label_tooltip(
        lbl = "Variable: Whether harvested any temporary crops",
        desc = "In the SuSo template apps, named `harvAnyTempCrops`."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("perm_crops_var"),
      label = label_tooltip(
        lbl = "Variable: Whether harvested any permanent crops",
        desc = "In the SuSo template apps, named `harvAnyPermCrops`."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("processed_var"),
      label = label_tooltip(
        lbl = "Question: whether processed any crop production",
        desc = paste(
          "Typically, 'process on the farm' is part of the question text.",
          "Type that to narrow the list of candidate questions.",
          "Frequently, this is the first question in the 'PROCESSING CROP PRODUCTION'",
          "section."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("processed_val"),
      label = label_tooltip(
        lbl = "Value: crop production processed",
        desc = "Typically, the value is 1 for 'Yes'."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("products_var"),
      label = label_tooltip(
        lbl = "Variable: which products produced",
        desc = paste(
          "The question that indicates, yes or no,",
          "which crop products are produced from a list.",
          "The question text contains 'produce'.",
          "Typing either of these may help find the right question."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("product_df"),
      label = label_tooltip(
        lbl = "Data: processed crop products",
        desc = paste(
          "Data set with one observation per crop product",
          "Typically, the data set has '_roster' in the name.",
          "Type this to narrow down the list of candidate data sets."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_var"),
      label = label_tooltip(
        lbl = "Question: Whether crop products sold.",
        desc = paste(
          "Typically, 'sell any' is in the question text",
          "Type that to narrow the list of candidate questions."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_val"),
      label = label_tooltip(
        lbl = "Value: product sold",
        desc = "Typically, the value is 1 for 'Yes'."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("amt_sold_vars"),
      label = label_tooltip(
        lbl = "Question(s): Value of crop production sold",
        desc = paste(
          "In some cases, a single variable.",
          "In others, one variable for total value, another for unit value."
        )
      ),
      choices = NULL,
      multiple = TRUE,
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

#' 4_quality_1_setup_2_data_process_crop_prod Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_process_crop_prod_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      hhold_dfs = r6$data_choices,
      temp_crop_vars = r6$process_crop_prod_temp_crops_var_choices,
      perm_crop_vars = r6$process_crop_prod_perm_crops_vars_choices,
      processed_vars = r6$process_crop_prod_processed_var_choices,
      processed_vals = r6$process_crop_prod_processed_val_choices,
      products_vars = r6$process_crop_prod_products_var_choices,
      product_dfs = r6$data_choices,
      sold_vars = r6$process_crop_prod_sold_var_choices,
      sold_vals = r6$process_crop_prod_sold_val_choices,
      amt_sold_vars = r6$process_crop_prod_amt_sold_var_choices
    )

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # --------------------------------------------------------------------------
    # when data are downloaded
    # --------------------------------------------------------------------------

    gargoyle::on("download_data", {

      shiny::req(r6$dirs$micro_combine)

      # ------------------------------------------------------------------------
      # compute choices from downloaded data
      # ------------------------------------------------------------------------

      # update UI to reflect data choices
      # but do not trigger reactive
      shiny::freezeReactiveValue(input, "hhold_df")
      shiny::updateSelectInput(
        inputId = "hhold_df",
        choices = r6$data_choices,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "product_df")
      shiny::updateSelectInput(
        inputId = "product_df",
        choices = r6$data_choices,
        selected = NULL
      )

      # ------------------------------------------------------------------------
      # (re)set to `NULL` variable and value selections
      # but do not trigger reactive
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "temp_crops_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "perm_crops_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "processed_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "processed_vals",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "products_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "sold_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "sold_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "amt_sold_vars",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "amt_sold_dk_val", updateNumericInput,  list(
          value = NULL
        )
      )

      update_inputs(
        input = input,
        session = session,
        specs = input_specs
      )

    })

    # --------------------------------------------------------------------------
    # load NULL values when values not previously saved
    # --------------------------------------------------------------------------

    if (is.null(r6$process_crop_prod_provided)) {

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "temp_crops_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "perm_crops_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "processed_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "processed_val",    updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "products_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "product_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "sold_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "sold_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "amt_sold_vars",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "amt_sold_dk_val", updateNumericInput,  list(
          value = NULL
        )

      )

      update_inputs(
        input = input,
        session = session,
        specs = input_specs
      )

    }

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$process_crop_prod_provided)) {

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$process_crop_prod_hhold_df
        ),
        "temp_crops_var",  updateSelectInput,    list(
          choices = r6$process_crop_prod_temp_crops_var_choices,
          selected = r6$process_crop_prod_temp_crops_var
        ),
        "perm_crops_var",   updateSelectInput,    list(
          choices = r6$process_crop_prod_perm_crops_var_choices,
          selected = r6$process_crop_prod_perm_crops_var
        ),
        "processed_var",  updateSelectInput,    list(
          choices = r6$process_crop_prod_processed_var_choices,
          selected = r6$process_crop_prod_processed_var
        ),
        "processed_val",    updateSelectInput,    list(
          choices = r6$process_crop_prod_processed_val_choices,
          selected = r6$process_crop_prod_processed_val
        ),
        "products_var",   updateSelectInput,    list(
          choices = r6$process_crop_prod_products_var_choices,
          selected = r6$process_crop_prod_products_var
        ),
        "product_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$process_crop_prod_product_df
        ),
        "sold_var",       updateSelectInput,    list(
          choices = r6$process_crop_prod_sold_var_choices,
          selected = r6$process_crop_prod_sold_var
        ),
        "sold_val",       updateSelectInput,    list(
          choices = r6$process_crop_prod_sold_val_choices,
          selected = r6$process_crop_prod_sold_val
        ),
        "amt_sold_vars",  updateSelectInput,    list(
          choices = r6$process_crop_prod_amt_sold_var_choices,
          selected = r6$process_crop_prod_amt_sold_var
        ),
        "amt_sold_dk_val", updateNumericInput,  list(
          value = r6$process_crop_prod_amt_sold_dk_val
        )

      )

      update_inputs(
        input = input,
        session = session,
        specs = input_specs
      )

    }

    # ==========================================================================
    # react to selections
    # ==========================================================================

    # --------------------------------------------------------------------------
    # hhold data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$hhold_df, {

      shiny::req(input$hhold_df)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # temporary and permanent crop harvest variables
      input_choices$temp_crop_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$hhold_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )
      input_choices$perm_crop_vars <- input_choices$temp_crop_vars

      # processed variable
      input_choices$processed_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$hhold_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # products variable
      input_choices$products_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$hhold_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "multi-select"
        )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "temp_crops_var",   updateSelectInput,    list(
          choices = input_choices$temp_crop_vars,
          selected = NULL
        ),
        "perm_crops_var",  updateSelectInput,    list(
          choices = input_choices$perm_crop_vars,
          selected = NULL
        ),
        "processed_var",  updateSelectInput,    list(
          choices = input_choices$processed_vars,
          selected = NULL
        ),
        "products_var",   updateSelectInput,    list(
          choices = input_choices$products_vars,
          selected = NULL
        ),

      )

      update_inputs(
        input = input,
        session = session,
        specs = input_specs
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # processed variable -> processed values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$processed_var, {

      shiny::req(
        r6$dirs$qnr,
        input$processed_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$processed_vals <- make_val_options(
        json_path = r6$json_path,
        categories_dir = r6$categories_dir,
        varname = extract_var_names(input$processed_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "processed_val")
      shiny::updateSelectInput(
        inputId = "processed_val",
        choices = input_choices$processed_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # product data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$product_df, {

      shiny::req(input$product_df)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # sold variable
      input_choices$sold_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$product_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # amount sold variables
      input_choices$amt_sold_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$product_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "numeric"
        )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "sold_var",       updateSelectInput,    list(
          choices = input_choices$sold_vars,
          selected = NULL
        ),
        "sold_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "amt_sold_vars",  updateSelectInput,    list(
          choices = input_choices$amt_sold_vars,
          selected = NULL
        ),
        "amt_sold_dk_val", updateNumericInput,  list(
          value = NULL
        )

      )

      update_inputs(
        input = input,
        session = session,
        specs = input_specs
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # sold variable -> sold values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$sold_var, {

      shiny::req(input$sold_var)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # compute choices
      input_choices$sold_vals <- make_val_options(
        json_path = r6$json_path,
        categories_dir = r6$categories_dir,
        varname = extract_var_names(input$sold_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

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

      # household data
      r6$process_crop_prod_hhold_df_choices <- input_choices$hhold_dfs
      r6$process_crop_prod_hhold_df <- input$hhold_df
      # temporary crop harvest variable
      r6$process_crop_prod_temp_crops_var_choices <-
        input_choices$temp_crop_vars
      r6$process_crop_prod_temp_crops_var <- input$temp_crops_var
      # permanent crop harvest variable
      r6$process_crop_prod_perm_crops_var_choices <-
        input_choices$perm_crop_vars
      r6$process_crop_prod_perm_crops_var <- input$perm_crops_var 
      # processed variable
      r6$process_crop_prod_processed_var_choices <- input_choices$processed_vars
      r6$process_crop_prod_processed_var <- input$processed_var
      # processed value
      r6$process_crop_prod_processed_val_choices <- input_choices$processed_vals
      r6$process_crop_prod_processed_val <- input$processed_val
      # products variable
      r6$process_crop_prod_products_var_choices <- input_choices$products_vars
      r6$process_crop_prod_products_var <- input$products_var
      # products data
      r6$process_crop_prod_product_df_choices <- input_choices$product_dfs
      r6$process_crop_prod_product_df <- input$product_df
      # sold variable
      r6$process_crop_prod_sold_var_choices <- input_choices$sold_vars
      r6$process_crop_prod_sold_var <- input$sold_var
      # sold value
      r6$process_crop_prod_sold_val_choices <- input_choices$sold_vals
      r6$process_crop_prod_sold_val <- input$sold_val
      # amount sold variables
      r6$process_crop_prod_amt_sold_var_choices <- input_choices$amt_sold_vars
      r6$process_crop_prod_amt_sold_var <- input$amt_sold_vars
      # amount sold DK value
      r6$process_crop_prod_amt_sold_dk_val <- input$amt_sold_dk_val
      # save action
      r6$process_crop_prod_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_process_crop_prod_sales")

    })

  })
}

## To be copied in the UI
# mod_4_quality_1_setup_2_data_process_crop_prod_ui("4_quality_1_setup_2_data_process_crop_prod_1")

## To be copied in the server
# mod_4_quality_1_setup_2_data_process_crop_prod_server("4_quality_1_setup_2_data_process_crop_prod_1")
