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
      label = "Household-level data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("temp_crops_var"),
      label = "Indicator variable: whether harvested any temporary crops",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("perm_crops_var"),
      label = "Indicator variable: whether harvested any temporary crops",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("processed_var"),
      label = "Question: whether processed any crop production",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("processed_val"),
      label = "Value: crop production processed",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("products_var"),
      label = "Variable: which products produced",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("product_df"),
      label = "Data set: processed crop products",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_var"),
      label = "Variable: whether product sold",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_val"),
      label = "Value: product sold",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("amt_sold_vars"),
      label = "Variables: Value of crop production sold",
      choices = NULL,
      multiple = TRUE,
      selected = NULL
    ),
    shiny::numericInput(
      inputId = ns("amt_sold_dk_val"),
      label = "Value 'do not know' sales amount",
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
      amt_sold_vars = r6$process_crop_prod_sold_var_choices
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
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$process_crop_prod_provided)) {

      shiny::req(
        # data directory
        r6$dirs$micro_combine,
        # household data
        r6$process_crop_prod_hhold_df_choices,
        r6$process_crop_prod_hhold_df,
        # processed variables
        r6$process_crop_prod_processed_var_choices,
        r6$process_crop_prod_processed_var,
        # processed value
        r6$process_crop_prod_processed_val_choices,
        r6$process_crop_prod_processed_val,
        # products variable
        r6$process_crop_prod_products_var_choices,
        r6$process_crop_prod_products_var,
        # products data
        r6$process_crop_prod_product_df_choices,
        r6$process_crop_prod_product_df,
        # sold variable
        r6$process_crop_prod_sold_var_choices,
        r6$process_crop_prod_sold_var,
        # sold value
        r6$process_crop_prod_sold_val_choices,
        r6$process_crop_prod_sold_val,
        # amount sold variables
        r6$process_crop_prod_amt_sold_var_choices,
        r6$process_crop_prod_amt_sold_var,
        # amount sold DK value
        r6$process_crop_prod_amt_sold_dk_val
      )

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$process_crop_prod_hhold_df
        ),
        "temp_crops_var",  updateSelectInput,    list(
          choices = r6$process_crop_prod_temp_crop_choices,
          selected = r6$process_crop_prod_temp_crop
        ),
        "perm_crops_var",   updateSelectInput,    list(
          choices = r6$process_crop_prod_temp_crop_choices,
          selected = r6$process_crop_prod_temp_crop
        ),
        "processed_var",  updateSelectInput,    list(
          choices = r6$process_crop_prod_processed_var_choices,
          selected = r6$process_crop_prod_processed_var
        ),
        "processed_vals",    updateSelectInput,    list(
          choices = r6$process_crop_prod_processed_val_choices,
          selected = r6$process_crop_prod_processed_val
        ),
        "products_var",   updateSelectInput,    list(
          choices = r6$process_crop_prod_products_var_choices,
          selected = r6$process_crop_prod_products_var,
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

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$hhold_df
      )

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

      input_choices$processed_val <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$processed_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveVal("processed_val")
      shiny::updateSelectInput(
        choices = input_choices$processed_val,
        selected = NULL
      )

    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # product data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$product_df, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$product_df
      )

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

      shiny::req(
        r6$dirs$qnr,
        input$sold_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # compute choices
      input_choices$sold_val <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$sold_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveVal("sold_val")
      shiny::updateSelectInput(
        choices = input_choices$sold_val,
        selected = NULL
      )

    }, ignoreInit = TRUE)

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
      r6$process_crop_prod_temp_crops_var <- input$temp_crop_var
      # permanent crop harvest variable
      r6$process_crop_prod_perm_crops_var_choices <-
        input_choices$perm_crop_vars
      r6$process_crop_prod_perm_crops_var <- input$perm_crop_var 
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
