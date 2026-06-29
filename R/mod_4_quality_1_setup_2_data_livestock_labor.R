#' 4_quality_1_setup_2_data_livestock_labor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_livestock_labor_ui <- function(id) {
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
      inputId = ns("have_anim_var"),
      label = label_tooltip(
        lbl = "Preloaded question: whether raised any livestock.",
        desc = paste(
          "Since this question is preloaded from the post-planting visit,",
          "the question text starts with 'PRELOADED' and also contains",
          "'household raised livestock in the past'.",
          "Type this text to narrow the list of candidate questions."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("have_anim_val"),
      label = label_tooltip(
        lbl = "Value: raise livestock",
        desc = "Typically, the value is 1 for 'Yes'."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("anim_labor_df"),
      label = label_tooltip(
        lbl = "Data: livestock labor",
        desc = paste(
          "Roster with one observation per category of worker.",
          "In the SuSo template app, the roster appears in the",
          "'LIVESTOCK LABOUR' section and the roster variable ends",
          "in `_workerCategory`"
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("anim_labor_id_var"),
      label = label_tooltip(
        lbl = "Variable: livestock labor category ID variable",
        desc = "System-generated ID variable that identifies crops."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("anim_labor_var"),
      label = label_tooltip(
        lbl = "Question: number of workers",
        desc = paste(
          "Typically, the question text contains 'How many'",
          "and 'keeping/managing livestock'",
          "Type that to narrow the list of candidate questions."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("anim_labor_none_val"),
      label = label_tooltip(
        lbl = "Value: none as number keeping/managing livestock",
        desc = paste(
          "In SuSo app, the special value for 'none'.",
          "Typically 0"
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("hhold_labor_vals"),
      label = label_tooltip(
        lbl = "Value: ID for household labor inputs",
        desc = paste(
          "Value of the roster ID variable that identifies household labor.",
          "In SuSo template app, typically 1, 2, and 3"
        )
      ),
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::selectInput(
      inputId = ns("free_labor_val"),
      label = label_tooltip(
        lbl = "Value: ID free labor inputs",
        desc = paste(
          "Value of the roster ID variable that identifies free labor.",
          "In SuSo template app, typically 4."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("paid_labor_val"),
      label = label_tooltip(
        lbl = "Value: ID paid labor inputs",
        desc = paste(
          "Value of the roster ID variable that identifies paid labor.",
          "In SuSo template app, typically 5."
        )
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

#' 4_quality_1_setup_2_data_livestock_labor Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_livestock_labor_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      hhold_dfs = r6$data_choices,
      have_anim_vars = r6$livestock_labor_have_anim_var_choices,
      have_anim_vals = r6$livestock_labor_have_anim_val_choices,
      anim_labor_dfs = r6$data_choices,
      anim_labor_vars = r6$livestock_labor_anim_var_choices,
      anim_labor_id_vars = r6$livestock_labor_anim_labor_id_var_choices,
      anim_labor_id_vals = r6$livestock_anim_id_val_choices
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
      shiny::freezeReactiveValue(input, "anim_labor_df")
      shiny::updateSelectInput(
        inputId = "anim_labor_df",
        choices = r6$data_choices,
        selected = NULL
      )

      # ------------------------------------------------------------------------
      # (re)set to `NULL` variable and value selections
      # but do not trigger reactive
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        # hhold
        "have_anim_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "have_anim_val",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # labor data
        "anim_labor_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "anim_labor_id_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "anim_labor_none_val",   updateSelectInput,    list(
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
        "have_anim_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "have_anim_val",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # labor data
        "anim_labor_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "anim_labor_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "anim_labor_id_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "anim_labor_none_val",   updateSelectInput,    list(
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
          selected = r6$livestock_labor_hhold_df
        ),
        "have_anim_var",  updateSelectInput,    list(
          choices = r6$livestock_labor_have_anim_var_choices,
          selected = r6$livestock_labor_have_anim_var
        ),
        "have_anim_val",  updateSelectInput,    list(
          choices = r6$livestock_labor_have_anim_val_choices,
          selected = r6$livestock_labor_have_anim_val
        ),
        # labor data
        "anim_labor_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$livestock_labor_anim_labor_df
        ),
        "anim_labor_var",   updateSelectInput,    list(
          choices = r6$livestock_labor_anim_var_choices,
          selected = r6$livestock_labor_anim_var
        ),
        "anim_labor_id_var",   updateSelectInput,    list(
          choices = r6$livestock_labor_anim_labor_id_var_choices,
          selected = r6$livestock_labor_anim_labor_id_var
        ),
        "anim_labor_none_val",   updateSelectInput,    list(
          choices = r6$livestock_anim_id_val_choices,
          selected = r6$livestock_labor_anim_labor_none_val
        ),
        "hhold_labor_vals",   updateSelectInput,    list(
          choices = r6$livestock_anim_id_val_choices,
          selected = r6$livestock_labor_hhold_labor_vals
        ),
        "free_labor_val",   updateSelectInput,    list(
          choices = r6$livestock_anim_id_val_choices,
          selected = r6$livestock_labor_free_labor_val
        ),
        "paid_labor_val",   updateSelectInput,    list(
          choices = r6$livestock_anim_id_val_choices,
          selected = r6$livestock_labor_paid_labor_val
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

      # temporary and permanent crop harvest variables
      input_choices$have_anim_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$hhold_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "have_anim_var",  updateSelectInput,    list(
          choices = input_choices$have_anim_vars,
          selected = NULL
        ),
        "have_anim_val",  updateSelectInput,    list(
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
    # have anim variable -> have anim values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$have_anim_var, {

      shiny::req(input$have_anim_var)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$have_anim_vals <- make_val_options(
        json_path = r6$json_path,
        categories_dir = r6$categories_dir,
        varname = extract_var_names(input$have_anim_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "have_anim_val")
      shiny::updateSelectInput(
        inputId = "have_anim_val",
        choices = input_choices$have_anim_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # anim_labor_df data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$anim_labor_df, {

      shiny::req(input$anim_labor_df)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$anim_labor_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$anim_labor_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "numeric"
        )

      input_choices$anim_labor_id_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$anim_labor_df, ".dta")) |>
        make_id_var_choices()

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "anim_labor_var")
      shiny::updateSelectInput(
        inputId = "anim_labor_var",
        choices = input_choices$anim_labor_vars,
        selected = NULL
      )

      shiny::freezeReactiveValue(input, "anim_labor_id_var")
      shiny::updateSelectInput(
        inputId = "anim_labor_id_var",
        choices = input_choices$anim_labor_id_vars,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # anim_labor_id_var variable -> anim_labor_id_var values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$anim_labor_id_var, {

      shiny::req(
        input$anim_labor_id_var,
        input$anim_labor_df
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$anim_labor_id_vals <- make_id_val_options(
        path = fs::path(
          r6$dirs$micro_combine,
          paste0(input$anim_labor_df, ".dta")
        ),
        varname = extract_id_var_names(input$anim_labor_id_var)
      ) |>
        append(values = c("[0] : None"))

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "anim_labor_none_val",  updateSelectInput,    list(
          choices = input_choices$anim_labor_id_vals,
          selected = NULL
        ),
        "hhold_labor_vals",  updateSelectInput,    list(
          choices = input_choices$anim_labor_id_vals,
          selected = NULL
        ),
        "free_labor_val",  updateSelectInput,    list(
          choices = input_choices$anim_labor_id_vals,
          selected = NULL
        ),
        "paid_labor_val",  updateSelectInput,    list(
          choices = input_choices$anim_labor_id_vals,
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
      r6$livestock_labor_hhold_df_choices <- input_choices$hhold_dfs
      r6$livestock_labor_hhold_df <- input$hhold_df
      # have animals variable
      r6$livestock_labor_have_anim_var_choices <- input_choices$have_anim_vars
      r6$livestock_labor_have_anim_var <- input$have_anim_var
      # have animal values
      r6$livestock_labor_have_anim_val_choices <- input_choices$have_anim_vals
      r6$livestock_labor_have_anim_val <- input$have_anim_val
      # livestock labor data
      r6$livestock_labor_anim_labor_df_choices <- input_choices$anim_labor_dfs
      r6$livestock_labor_anim_labor_df <- input$anim_labor_df
      # livestock labor quantity variable
      r6$livestock_labor_anim_var_choices <- input_choices$anim_labor_vars
      r6$livestock_labor_anim_var <- input$anim_labor_var
      # livestock labor ID variable
      r6$livestock_labor_anim_labor_id_var_choices <-
        input_choices$anim_labor_id_vars
      r6$livestock_labor_anim_labor_id_var <- input$anim_labor_id_var
      # livestock labor values
      r6$livestock_anim_id_val_choices <- input_choices$anim_labor_id_vals
      r6$livestock_labor_anim_labor_none_val <- input$anim_labor_none_val
      r6$livestock_labor_hhold_labor_vals <- input$hhold_labor_vals
      r6$livestock_labor_free_labor_val <- input$free_labor_val
      r6$livestock_labor_paid_labor_val <- input$paid_labor_val
      # save action
      r6$livestock_labor_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_livestock_labor_sales")

    })


  })
}

## To be copied in the UI
# mod_4_quality_1_setup_2_data_livestock_labor_ui("4_quality_1_setup_2_data_livestock_labor_1")

## To be copied in the server
# mod_4_quality_1_setup_2_data_livestock_labor_server("4_quality_1_setup_2_data_livestock_labor_1")
