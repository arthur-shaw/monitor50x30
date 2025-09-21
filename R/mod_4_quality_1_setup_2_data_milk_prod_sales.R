#' 4_quality_1_setup_2_data_milk_prod_sales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_milk_prod_sales_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
    shiny::selectInput(
      inputId = ns("data"),
      label = "Livestock-level data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("animal_id_var"),
      label = "Livestock ID variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("animal_vals"),
      label = "Milk-producing animals",
      choices = NULL,
      multiple = TRUE,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("produced_var"),
      label = "Whether produced milk variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("produced_val"),
      label = "Value indicating milk produced",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_var"),
      label = "Whether sold milk variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_val"),
      label = "Value indicating milk sold",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("amt_sold_vars"),
      label = "Amount milk sold variables",
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
    
#' 4_quality_1_setup_2_data_milk_prod_sales Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_milk_prod_sales_server <- function(id, r6, parent){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$milk_prod_df_choices,
      animal_id_vars = r6$milk_prod_var_choices,
      animal_vals = r6$milk_prod_val_choices,
      produced_vars = r6$milk_prod_var_choices,
      produced_vals = r6$milk_prod_val_choices,
      sold_vars = r6$milk_sold_var_choices,
      sold_vals = r6$milk_sold_val_choices,
      amt_sold_vars = r6$milk_sold_var_choices
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

      # get list of data files in combined folder
      input_choices$data <- r6$dirs$micro_combine |>
        make_data_choices()

      # update UI to reflect data choices
      # but do not trigger reactive
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choices = input_choices$data,
        selected = NULL
      )

      # ------------------------------------------------------------------------
      # (re)set to `NULL` variable and value selections
      # but do not trigger reactive
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "animal_id_var",  updateSelectInput,    list(choices = NULL, selected = NULL),
        "animal_vals",    updateSelectInput,    list(choices = NULL, selected = NULL),
        "produced_var",   updateSelectInput,    list(choices = NULL, selected = NULL),
        "produced_val",   updateSelectInput,    list(choices = NULL, selected = NULL),
        "sold_var",       updateSelectInput,    list(choices = NULL, selected = NULL),
        "sold_val",       updateSelectInput,    list(choices = NULL, selected = NULL),
        "amt_sold_vars",  updateSelectInput,    list(choices = NULL, selected = NULL),
        "amt_sold_dk_val", updateNumericInput,  list(value = NULL),
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

    if (!is.null(r6$milk_prod_provided)) {

      shiny::req(
        # data directory
        r6$dirs$micro_combine,
        # data
        r6$milk_prod_df_choices,
        r6$milk_prod_df,
        # animal ID variable
        r6$milk_prod_animal_id_var_choices, 
        r6$milk_prod_animal_id_var,
        # animal values
        r6$milk_prod_animal_val_choices, 
        r6$milk_prod_animal_val, 
        # production variable
        r6$milk_prod_produced_var_choices,
        r6$milk_prod_produced_var,
        # produced value
        r6$milk_prod_produced_val_choices,
        r6$milk_prod_produced_val,
        # sold variable
        r6$milk_prod_sold_var_choices,
        r6$milk_prod_sold_var,
        # sold value
        r6$milk_prod_sold_val_choices,
        r6$milk_prod_sold_val,
        # amount sold variables
        r6$milk_prod_amt_sold_vars_choices,
        r6$milk_prod_amt_sold_vars,
        # amount sold DK value
        r6$milk_prod_amt_sold_dk_val
      )

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "data",           updateSelectInput,    list(
          choices = r6$milk_prod_df_choices,
          selected = r6$milk_prod_df
        ),
        "animal_id_var",  updateSelectInput,    list(
          choices = r6$milk_prod_animal_id_var_choices, 
          selected = r6$milk_prod_animal_id_var
        ),
        "animal_vals",    updateSelectInput,    list(
          choices = r6$milk_prod_animal_val_choices, 
          selected = r6$milk_prod_animal_val
        ),
        "produced_var",   updateSelectInput,    list(
          choices = r6$milk_prod_produced_var_choices,
          selected = r6$milk_prod_produced_var
        ),
        "produced_val",   updateSelectInput,    list(
          choices = r6$milk_prod_produced_val_choices,
          selected = r6$milk_prod_produced_val
        ),
        "sold_var",       updateSelectInput,    list(
          choices = r6$milk_prod_sold_var_choices,
          selected = r6$milk_prod_sold_var
        ),
        "sold_val",       updateSelectInput,    list(
          choices = r6$milk_prod_sold_val_choices,
          selected = r6$milk_prod_sold_val
        ),
        "amt_sold_vars",  updateSelectInput,    list(
          choices = r6$milk_prod_amt_sold_vars_choices,
          selected = r6$milk_prod_amt_sold_vars
        ),
        "amt_sold_dk_val", updateNumericInput,  list(
          value = r6$milk_prod_amt_sold_dk_val
        )

      )

    }

    # ==========================================================================
    # react to selections
    # ==========================================================================

    # --------------------------------------------------------------------------
    # data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$data, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$data
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # load variables data frame from disk
      qnr_vars_df <- fs::path(r6$dirs$qnr, "qnr_vars.rds") |>
        readRDS()

      # livestock ID variable choices
      input_choices$animal_id_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_id_var_choices()

      # produced variable
      input_choices$produced_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_data_var_choices(
          vars_df = qnr_vars_df,
          var_type = "single-select"
        )

      # sold variable
      input_choices$sold_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_data_var_choices(
          vars_df = qnr_vars_df,
          var_type = "single-select"
        )

      # amount sold variables
      input_choices$amt_sold_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_data_var_choices(
          vars_df = qnr_vars_df,
          var_type = "numeric"
        )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "animal_id_var",  updateSelectInput,    list(
          choices = input_choices$animal_id_vars,
          selected = NULL
        ),
        "animal_vals",    updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "produced_var",   updateSelectInput,    list(
          choices = input_choices$produced_vars,
          selected = NULL
        ),
        "produced_val",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
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
    # livestock ID variable
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$animal_id_var, {

      shiny::req(
        r6$dirs$qnr,
        input$animal_id_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # load variables data frame from disk
      qnr_vars_df <- fs::path(r6$dirs$qnr, "qnr_vars.rds") |>
        readRDS()

      # compute choices
      input_choices$animal_vals <- make_val_options(
        qnr_df = qnr_vars_df,
        varname = extract_var_names(input$animal_id_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "animal_vals")
      shiny::updateSelectInput(
        inputId = "animal_vals",
        choices = input_choices$animal_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # produced variable -> produced values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$produced_var, {

      shiny::req(
        r6$dirs$qnr,
        input$produced_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # load variables data frame from disk
      qnr_vars_df <- fs::path(r6$dirs$qnr, "qnr_vars.rds") |>
        readRDS()

      # compute choices
      input_choices$produced_val <- make_val_options(
        qnr_df = qnr_vars_df,
        varname = extract_var_names(input$produced_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "produced_val")
      shiny::updateSelectInput(
        inputId = "produced_val",
        choices = input_choices$produced_val,
        selected = NULL
      )

    }, ignoreInit = TRUE)

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

      # load variables data frame from disk
      qnr_vars_df <- fs::path(r6$dirs$qnr, "qnr_vars.rds") |>
        readRDS()

      # compute choices
      input_choices$sold_val <- make_val_options(
        qnr_df = qnr_vars_df,
        varname = extract_var_names(input$sold_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "sold_val")
      shiny::updateSelectInput(
        inputId = "sold_val",
        choices = input_choices$sold_val,
        selected = NULL
      )

    }, ignoreInit = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6

      # data
      r6$milk_prod_df_choices <- input_choices$data
      r6$milk_prod_df <- input$data
      # livestock animal ID
      r6$milk_prod_animal_id_var_choices <- input_choices$animal_id_vars
      r6$milk_prod_animal_id_var <- input$animal_id_var
      # livestock IDs
      r6$milk_prod_animal_vals_choices <- input_choices$animal_vals
      r6$milk_prod_animal_val <- input$animal_vals
      # produced variable
      r6$milk_prod_produced_var_choices <- input_choices$produced_vars
      r6$milk_prod_produced_var <- input$produced_var
      # produced values
      r6$milk_prod_produced_val_choices <- input_choices$produced_vals
      r6$milk_prod_produced_val <- input$produced_val
      # sold variable
      r6$milk_prod_sold_var_choices <- input_choices$sold_vars
      r6$milk_prod_sold_var <- input$sold_var
      # sold values
      r6$milk_prod_sold_val_choices <- input_choices$sold_vals
      r6$milk_prod_sold_val <- input$sold_val
      # amount sold variables
      r6$milk_prod_amt_sold_var_choices <- input_choices$amt_sold_vars
      r6$milk_prod_amt_sold_var <- input$amt_sold_vars
      # amount sold DK values
      r6$milk_prod_amt_sold_dk_val <- input$amt_sold_dk_val
      # save action
      r6$milk_prod_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_milk_prod_sales")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_milk_prod_sales_ui("4_quality_1_setup_2_data_milk_prod_sales_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_milk_prod_sales_server("4_quality_1_setup_2_data_milk_prod_sales_1")
