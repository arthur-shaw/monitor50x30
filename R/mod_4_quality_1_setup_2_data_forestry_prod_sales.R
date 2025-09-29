#' 4_quality_1_setup_2_data_forestry_prod_sales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_forestry_prod_sales_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("hhold_df"),
      label = "Household-level data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("practice_var"),
      label = "Whether practice forestry variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("practice_val"),
      label = "Value indicating that the household practices forestry",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("products_var"),
      label = "forestry products variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("product_df"),
      label = "forestry products-level data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_var"),
      label = "Whether sold forestry product variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_val"),
      label = "Value indicating forestry product sold",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("amt_sold_vars"),
      label = "Amount forestry sold variables",
      choices = NULL,
      multiple = TRUE,
      selected = NULL
    ),
    shiny::numericInput(
      inputId = ns("amt_sold_dk_val"),
      label = "Code for 'do not know' sales amount",
      value = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )


  )
}
    
#' 4_quality_1_setup_2_data_forestry_prod_sales Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_forestry_prod_sales_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      hhold_dfs = r6$data_choices,
      practice_vars = r6$forestry_prod_practice_var_choices,
      practice_vals = r6$forestry_prod_practice_val_choices,
      products_vars = r6$forestry_prod_products_var_choices,
      product_dfs = r6$data_choices,
      sold_vars = r6$forestry_prod_sold_var_choices,
      sold_vals = r6$forestry_prod_sold_val_choices,
      amt_sold_vars = r6$forestry_prod_sold_var_choices
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
        "practice_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "practice_vals",  updateSelectInput,    list(
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
    # load NULL values if values not previously saved
    # --------------------------------------------------------------------------

    if (is.null(r6$forestry_prod_provided)) {

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "practice_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "practice_val",    updateSelectInput,    list(
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

    if (!is.null(r6$forestry_prod_provided)) {

      shiny::req(
        # data directory
        r6$dirs$micro_combine,
        # household data
        r6$forestry_prod_hhold_df_choices,
        r6$forestry_prod_hhold_df,
        # practiced forestry variables
        r6$forestry_prod_practice_var_choices,
        r6$forestry_prod_practice_var,
        # practiced forestry value
        r6$forestry_prod_practice_val_choices,
        r6$forestry_prod_practice_val,
        # forestry products variable
        r6$forestry_prod_products_var_choices,
        r6$forestry_prod_products_var,
        # products data
        r6$forestry_prod_product_df_choices,
        r6$forestry_prod_product_df,
        # sold variable
        r6$forestry_prod_sold_var_choices,
        r6$forestry_prod_sold_var,
        # sold value
        r6$forestry_prod_sold_val_choices,
        r6$forestry_prod_sold_val,
        # amount sold variables
        r6$forestry_prod_amt_sold_var_choices,
        r6$forestry_prod_amt_sold_var,
        # amount sold DK value
        r6$forestry_prod_amt_sold_dk_val
      )

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$forestry_prod_hhold_df
        ),
        "practice_var",  updateSelectInput,    list(
          choices = r6$forestry_prod_practice_var_choices,
          selected = r6$forestry_prod_practice_var
        ),
        "practice_val",    updateSelectInput,    list(
          choices = r6$forestry_prod_practice_val_choices,
          selected = r6$forestry_prod_practice_val
        ),
        "products_var",   updateSelectInput,    list(
          choices = r6$forestry_prod_products_var_choices,
          selected = r6$forestry_prod_products_var
        ),
        "product_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$forestry_prod_product_df
        ),
        "sold_var",       updateSelectInput,    list(
          choices = r6$forestry_prod_sold_var_choices,
          selected = r6$forestry_prod_sold_var
        ),
        "sold_val",       updateSelectInput,    list(
          choices = r6$forestry_prod_sold_val_choices,
          selected = r6$forestry_prod_sold_val
        ),
        "amt_sold_vars",  updateSelectInput,    list(
          choices = r6$forestry_prod_amt_sold_var_choices,
          selected = r6$forestry_prod_amt_sold_var
        ),
        "amt_sold_dk_val", updateNumericInput,  list(
          value = r6$forestry_prod_amt_sold_dk_val
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
    # data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$hhold_df, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$hhold_df
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # practice variable
      input_choices$practice_vars <- r6$dirs$micro_combine |>
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
        "practice_var",  updateSelectInput,    list(
          choices = input_choices$practice_vars,
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
    # practiced variable -> practiced values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$practice_var, {

      shiny::req(
        r6$dirs$qnr,
        input$practice_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # compute choices
      input_choices$practice_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$practice_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "practice_val")
      shiny::updateSelectInput(
        inputId = "practice_val",
        choices = input_choices$practice_vals,
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

      input_choices$sold_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
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

    }, ignoreInit = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6

      # household data
      r6$forestry_prod_hhold_df_choices <- input_choices$hhold_dfs
      r6$forestry_prod_hhold_df <- input$hhold_df
      # practiced forestry variables
      r6$forestry_prod_practice_var_choices <- input_choices$practice_vars
      r6$forestry_prod_practice_var <- input$practice_var
      # practiced forestry value
      r6$forestry_prod_practice_val_choices <- input_choices$practice_vals
      r6$forestry_prod_practice_val <- input$practice_val
      # forestry products variable
      r6$forestry_prod_products_var_choices <- input_choices$products_vars
      r6$forestry_prod_products_var <- input$products_var
      # products data
      r6$forestry_prod_product_df_choices <- input_choices$product_dfs
      r6$forestry_prod_product_df <- input$product_df
      # sold variable
      r6$forestry_prod_sold_var_choices <- input_choices$sold_vars
      r6$forestry_prod_sold_var <- input$sold_var
      # sold value
      r6$forestry_prod_sold_val_choices <- input_choices$sold_vals
      r6$forestry_prod_sold_val <- input$sold_val
      # amount sold variables
      r6$forestry_prod_amt_sold_var_choices <- input_choices$amt_sold_vars
      r6$forestry_prod_amt_sold_var <- input$amt_sold_vars
      # amount sold DK value
      r6$forestry_prod_amt_sold_dk_val <- input$amt_sold_dk_val
      # save action
      r6$forestry_prod_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_forestry_prod_sales")

    })


  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_forestry_prod_sales_ui("4_quality_1_setup_2_data_forestry_prod_sales_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_forestry_prod_sales_server("4_quality_1_setup_2_data_forestry_prod_sales_1")
