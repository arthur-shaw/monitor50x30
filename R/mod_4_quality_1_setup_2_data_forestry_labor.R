#' 4_quality_1_setup_2_data_forestry_labor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_forestry_labor_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("hhold_df"),
      label = "Household-level data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("produce_var"),
      label = "Question: whether involved in forestry",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("produce_val"),
      label = "Value: involved in forestry",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("labor_var"),
      label = "Variable: which categories of labor involved in forestry",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("hhold_labor_vals"),
      label = "Value: household labor",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::selectInput(
      inputId = ns("free_labor_val"),
      label = "Value: free labor",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("paid_labor_val"),
      label = "Value: paid labor",
      choices = NULL,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}

#' 4_quality_1_setup_2_data_forestry_labor Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_forestry_labor_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      hhold_dfs = r6$data_choices,
      produce_vars = r6$forestry_labor_produce_var_choices,
      produce_vals = r6$forestry_labor_produce_val_choices,
      labor_vars = r6$forestry_labor_labor_var_choices,
      labor_vals = r6$forestry_labor_labor_val_choices
    )

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # --------------------------------------------------------------------------
    # when data are downloaded
    # --------------------------------------------------------------------------

    gargoyle::on("download_data", {

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

      # ------------------------------------------------------------------------
      # (re)set to `NULL` variable and value selections
      # but do not trigger reactive
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        # produce
        "produce_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "produce_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # labor categories
        "labor_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "hhold_labor_vals",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "free_labor_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "paid_labor_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
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

    if (is.null(r6$livestock_labor_provided)) {

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        # hhold data
        "hhold_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        # produce
        "produce_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "produce_val",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # labor categories
        "labor_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "hhold_labor_vals",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "free_labor_val",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "paid_labor_val",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
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

    if (!is.null(r6$livestock_labor_provided)) {

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        # hhold data
        "hhold_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$forestry_labor_hhold_df
        ),
        # produce
        "produce_var",  updateSelectInput,    list(
          choices = r6$forestry_labor_produce_var_choices,
          selected = r6$forestry_labor_produce_var
        ),
        "produce_val",  updateSelectInput,    list(
          choices = r6$forestry_labor_produce_val_choices,
          selected = r6$forestry_labor_produce_val
        ),
        # labor categories
        "labor_var",   updateSelectInput,    list(
          choices = r6$forestry_labor_labor_var_choices,
          selected = r6$forestry_labor_labor_var
        ),
        "hhold_labor_vals",   updateSelectInput,    list(
          choices = r6$forestry_labor_labor_val_choices,
          selected = r6$forestry_labor_hhold_labor_vals
        ),
        "free_labor_val",   updateSelectInput,    list(
          choices = r6$forestry_labor_labor_val_choices,
          selected = r6$forestry_labor_free_labor_val
        ),
        "paid_labor_val",   updateSelectInput,    list(
          choices = r6$forestry_labor_labor_val_choices,
          selected = r6$forestry_labor_paid_labor_val
        ),
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

      # produce variables
      input_choices$produce_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$hhold_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # labor category variables
      input_choices$labor_vars <- r6$dirs$micro_combine |>
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
        # produce
        "produce_var",   updateSelectInput,    list(
          choices = input_choices$produce_vars,
          selected = NULL
        ),
        "produce_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # labor categories
        "labor_var",       updateSelectInput,    list(
          choices = input_choices$labor_vars,
          selected = NULL
        ),
        "hhold_labor_vals",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "free_labor_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "paid_labor_val",       updateSelectInput,    list(
          choices = NULL,
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
    # production variable -> production values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$produce_var, {

      shiny::req(input$produce_var)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$produce_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$produce_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "produce_val")
      shiny::updateSelectInput(
        inputId = "produce_val",
        choices = input_choices$produce_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # labor_var variable -> labor_var values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$labor_var, {

      shiny::req(input$labor_var)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$labor_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$labor_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_labor_vals",  updateSelectInput,    list(
          choices = input_choices$labor_vals,
          selected = NULL
        ),
        "free_labor_val",  updateSelectInput,    list(
          choices = input_choices$labor_vals,
          selected = NULL
        ),
        "paid_labor_val",  updateSelectInput,    list(
          choices = input_choices$labor_vals,
          selected = NULL
        ),
      )

      update_inputs(
        input = input,
        session = session,
        specs = input_specs
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6

      # household data
      r6$forestry_labor_hhold_df_choices <- input_choices$hhold_dfs
      r6$forestry_labor_hhold_df <- input$hhold_df
      # forestry production variable
      r6$forestry_labor_produce_var_choices <- input_choices$produce_vars
      r6$forestry_labor_produce_var <- input$produce_var
      # forestry production values
      r6$forestry_labor_produce_val_choices <- input_choices$produce_vals
      r6$forestry_labor_produce_val <- input$produce_val
      # forestry labor variable
      r6$forestry_labor_labor_var_choices <- input_choices$labor_vars
      r6$forestry_labor_labor_var <- input$labor_var
      # forestry labor values
      r6$forestry_labor_labor_val_choices <- input_choices$labor_vals
      r6$forestry_labor_hhold_labor_vals <- input$hhold_labor_vals
      r6$forestry_labor_free_labor_val <- input$free_labor_val
      r6$forestry_labor_paid_labor_val <- input$paid_labor_val
      # save action
      r6$forestry_labor_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_forestry_labor_sales")

    })



  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_forestry_labor_ui("4_quality_1_setup_2_data_forestry_labor_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_forestry_labor_server("4_quality_1_setup_2_data_forestry_labor_1")
