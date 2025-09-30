#' 4_quality_1_setup_2_data_crop_labor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_crop_labor_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("hhold_df"),
      label = "Household-level data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("grew_crops_var"),
      label = "Question: whether grew any crops",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("grew_crops_val"),
      label = "Value: grew any crops",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("paid_var"),
      label = "Variable: whether paid for any labor inputs",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("free_var"),
      label = "Variable: whether got any free or exchange labor; which type(s)",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("members_df"),
      label = "Member-level data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("member_worked_var"),
      label = "Variable: whether member worked in any hhold cropping activity",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("member_worked_val"),
      label = "Value: member worked in hhold cropping activity.",
      choices = NULL,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}

#' 4_quality_1_setup_2_data_crop_labor Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_crop_labor_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      hhold_dfs = r6$data_choices,
      grew_crops_vars = r6$crop_labor_grew_crops_var_choices,
      grew_crops_vals = r6$crop_labor_grew_crops_val_choices,
      paid_vars = r6$crop_labor_paid_var_choices,
      free_vars = r6$crop_labor_free_var_choices,
      members_dfs = r6$data_choices,
      member_worked_vars = r6$crop_labor_member_worked_var_choices,
      member_worked_vals = r6$crop_labor_member_worked_val_choices
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
      # update UI to reflect data choices
      # but do not trigger reactive
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "hhold_df")
      shiny::updateSelectInput(
        inputId = "hhold_df",
        choices = r6$data_choices,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "members_df")
      shiny::updateSelectInput(
        inputId = "members_df",
        choices = r6$data_choices,
        selected = NULL
      )

      # ------------------------------------------------------------------------
      # (re)set to `NULL` variable and value selections
      # but do not trigger reactive
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "grew_crops_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "grew_crops_vals",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "paid_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "free_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "member_worked_var",       updateSelectInput,    list(
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
    # load NULL values when values not previously saved
    # --------------------------------------------------------------------------

    if (is.null(r6$crop_labor_provided)) {

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "grew_crops_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "grew_crops_val",    updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "paid_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "free_var",   updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "members_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "member_worked_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "member_worked_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
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

    if (!is.null(r6$crop_labor_provided)) {

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_df",           updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$crop_labor_hhold_df
        ),
        "grew_crops_var",  updateSelectInput,    list(
          choices = r6$crop_labor_grew_crops_var_choices,
          selected = r6$crop_labor_grew_crops_var
        ),
        "grew_crops_val",    updateSelectInput,    list(
          choices = r6$crop_labor_grew_crops_val_choices,
          selected = r6$crop_labor_grew_crops_val
        ),
        "paid_var",   updateSelectInput,    list(
          choices = r6$crop_labor_paid_var_choices,
          selected = r6$crop_labor_paid_var
        ),
        "free_var",   updateSelectInput,    list(
          choices = r6$crop_labor_free_var_choices,
          selected = r6$crop_labor_free_var
        ),
        "members_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$crop_labor_members_df
        ),
        "member_worked_var",       updateSelectInput,    list(
          choices = r6$crop_labor_member_worked_var_choices,
          selected = r6$crop_labor_member_worked_var
        ),
        "member_worked_val",       updateSelectInput,    list(
          choices = r6$crop_labor_member_worked_val_choices,
          selected = r6$crop_labor_member_worked_val
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
      input_choices$grew_crops_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$hhold_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # paid labor variable
      input_choices$paid_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$hhold_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "multi-select"
        )

      # free labor variable
      input_choices$free_vars <- r6$dirs$micro_combine |>
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
        "grew_crops_var",   updateSelectInput,    list(
          choices = input_choices$grew_crops_vars,
          selected = NULL
        ),
        "paid_var",  updateSelectInput,    list(
          choices = input_choices$paid_vars,
          selected = NULL
        ),
        "free_var",  updateSelectInput,    list(
          choices = input_choices$free_vars,
          selected = NULL
        )
      )

      update_inputs(
        input = input,
        session = session,
        specs = input_specs
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # grew_crops variable -> grew_crops values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$grew_crops_var, {

      shiny::req(input$grew_crops_var)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # compute choices
      input_choices$grew_crops_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$grew_crops_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "grew_crops_val")
      shiny::updateSelectInput(
        inputId = "grew_crops_val",
        choices = input_choices$grew_crops_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # paid variable -> paid values
    # --------------------------------------------------------------------------

    # shiny::observeEvent(input$paid_var, {

    #   shiny::req(input$paid_var)

    #   # ------------------------------------------------------------------------
    #   # compute choices
    #   # ------------------------------------------------------------------------

    #   # compute choices
    #   input_choices$paid_vals <- make_val_options(
    #     qnr_df = r6$qnr_vars_df,
    #     categories_df = r6$q_categories_df,
    #     varname = extract_var_names(input$paid_var)
    #   )

    #   # ------------------------------------------------------------------------
    #   # update choices in the UI
    #   # ------------------------------------------------------------------------

    #   shiny::freezeReactiveValue(input, "paid_val")
    #   shiny::updateSelectInput(
    #     inputId = "paid_val",
    #     choices = input_choices$paid_vals,
    #     selected = NULL
    #   )

    # }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # free variable -> free values
    # --------------------------------------------------------------------------

    # shiny::observeEvent(input$free_var, {

    #   shiny::req(input$free_var)

    #   # ------------------------------------------------------------------------
    #   # compute choices
    #   # ------------------------------------------------------------------------

    #   # compute choices
    #   input_choices$free_vals <- make_val_options(
    #     qnr_df = r6$qnr_vars_df,
    #     categories_df = r6$q_categories_df,
    #     varname = extract_var_names(input$free_var)
    #   )

    #   # ------------------------------------------------------------------------
    #   # update choices in the UI
    #   # ------------------------------------------------------------------------

    #   shiny::freezeReactiveValue(input, "free_val")
    #   shiny::updateSelectInput(
    #     inputId = "free_val",
    #     choices = input_choices$free_vals,
    #     selected = NULL
    #   )

    # }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # members data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$members_df, {

      shiny::req(input$members_df)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # member worked variable
      input_choices$member_worked_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$members_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "member_worked_var")
      shiny::updateSelectInput(
        inputId = "member_worked_var",
        choices = input_choices$member_worked_vars,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # member worked variable -> member worked values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$member_worked_var, {

      shiny::req(input$member_worked_var)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # compute choices
      input_choices$member_worked_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$member_worked_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      shiny::freezeReactiveValue(input, "member_worked_val")
      shiny::updateSelectInput(
        inputId = "member_worked_val",
        choices = input_choices$member_worked_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6

      # household data
      r6$crop_labor_hhold_df_choices <- input_choices$hhold_dfs
      r6$crop_labor_hhold_df <- input$hhold_df
      # grew crops variables
      r6$crop_labor_grew_crops_var_choices <- input_choices$grew_crops_vars
      r6$crop_labor_grew_crops_var <- input$grew_crops_var
      # grew crops value
      r6$crop_labor_grew_crops_val_choices <- input_choices$grew_crops_vals
      r6$crop_labor_grew_crops_val <- input$grew_crops_val
      # paid labor variable
      r6$crop_labor_paid_var_choices <- input_choices$paid_vars
      r6$crop_labor_paid_var <- input$paid_var
      # free labor variable
      r6$crop_labor_free_var_choices <- input_choices$free_vars
      r6$crop_labor_free_var <- input$free_var
      # members data
      r6$crop_labor_members_df_choices <- input_choices$members_dfs
      r6$crop_labor_members_df <- input$members_df
      # member_worked variable
      r6$crop_labor_member_worked_var_choices <-
        input_choices$member_worked_vars
      r6$crop_labor_member_worked_var <- input$member_worked_var
      # member_worked value
      r6$crop_labor_member_worked_val_choices <-
        input_choices$member_worked_vals
      r6$crop_labor_member_worked_val <- input$member_worked_val
      # save action
      r6$crop_labor_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_crop_labor_sales")

    })


  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_crop_labor_ui("4_quality_1_setup_2_data_crop_labor_1")

## To be copied in the server
# mod_4_quality_1_setup_2_data_crop_labor_server("4_quality_1_setup_2_data_crop_labor_1")
