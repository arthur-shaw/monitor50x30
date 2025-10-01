#' 4_quality_1_setup_2_data_income_sources UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_income_sources_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("hhold_df"),
      label = "File: Household-level data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("crop_var"),
      label = "Preloaded variable: whether grew any crops",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("crop_val"),
      label = "Value: grew crops",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("livestock_var"),
      label = "Preloaded variable: whether raised any livestock",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("livestock_val"),
      label = "Value: raised livestocks",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("temp_crop_df"),
      label = "File: temporary crop-level data",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("temp_crop_var"),
      label = "Question: whether sold any [TEMPORARY CROP]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("temp_crop_val"),
      label = "Value: sold some [TEMPORARY CROP]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("perm_crop_df"),
      label = "File: permanent crop-level data",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("perm_crop_var"),
      label = "Question: whether sold any [PERMANENT CROP]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("perm_crop_val"),
      label = "Value: sold some [PERMANENT CROP]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("processed_df"),
      label = "File: processed crop product-level data",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("processed_var"),
      label = "Question: whether sold any [PROCESSED CROP PRODUCT]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("processed_val"),
      label = "Value: sold some [PROCESSED CROP PRODUCT]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("anim_df"),
      label = "File: livestock-level data",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_live_anim_var"),
      label = "Question: number of live [ANIMAL] sold",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("slaughter_anim_var"),
      label = "Question: whether sold any slaughtered [ANIMAL]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("slaughter_anim_val"),
      label = "Value: sold slaughtered [ANIMAL]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("sold_live_poultry_var"),
      label = "Question: number [POULTRY] sold alive",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("slaughter_poultry_var"),
      label = "Question: whether sold any slaughtered [POULTRY]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("slaughter_poultry_val"),
      label = "Value: sold slaughtered [POULTRY]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("milk_var"),
      label = "Question: whether sold any [ANIMAL] milk",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("milk_val"),
      label = "Value: sold [ANIMAL] milk",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("eggs_var"),
      label = "Question: whether sold any [POULTRY] eggs",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("eggs_val"),
      label = "Value: sold [POULTRY] eggs",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("oth_anim_prod_df"),
      label = "File: other animal products-level data",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("oth_anim_var"),
      label = "Question: whether sold any [ANIMAL PRODUCT]",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("oth_anim_val"),
      label = "Value: sold some [ANIMAL PRODUCT]",
      choices = NULL,
      selected = NULL
    )

  )
}

#' 4_quality_1_setup_2_data_income_sources Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_income_sources_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      hhold_dfs = r6$data_choices,
      # raises crops
      crop_vars = r6$income_sources_crop_var_choices,
      crop_vals = r6$income_sources_crop_val_choices,
      # raises livestock
      livestock_vars = r6$income_sources_livestock_var_choices,
      livestock_vals = r6$income_sources_livestock_val_choices,
      # temporary crops
      temp_crop_dfs = r6$data_choices,
      temp_crop_vars = r6$income_sources_temp_crop_var_choices,
      temp_crop_vals = r6$income_sources_temp_crop_vals,
      # permanent crops
      perm_crop_dfs = r6$data_choices,
      perm_crop_vars = r6$income_sources_perm_crop_var_choices,
      perm_crop_vals = r6$income_sources_perm_crop_vals,
      # processed crop products
      processed_dfs = r6$data_choices,
      processed_vars = r6$income_sources_processed_var_choices,
      processed_vals = r6$income_sources_processed_vals,
      # livestock
      anim_dfs = r6$data_choices,
      sold_live_anim_vars = r6$income_sources_sold_live_anim_var_choices,
      slaughter_anim_vars = r6$income_sources_slaughter_anim_var_choices,
      slaughter_anim_vals = r6$income_sources_slaughter_anim_val_choices,
      sold_live_poultry_vars = r6$income_sources_sold_live_poultry_var_choices,
      slaughter_poultry_vars = r6$income_sources_slaughter_poultry_var_choices,
      slaughter_poultry_vals = r6$income_sources_slaughter_poultry_val_choices,
      milk_vars = r6$income_sources_milk_var_choices,
      milk_vals = r6$income_sources_milk_val_choices,
      eggs_vars = r6$income_sources_eggs_var_choices,
      eggs_vals = r6$income_sources_eggs_val_choices
    )

    # --------------------------------------------------------------------------
    # when data are downloaded
    # --------------------------------------------------------------------------

    gargoyle::on("download_data", {

      # ------------------------------------------------------------------------
      # compute choices from downloaded data
      # ------------------------------------------------------------------------

      # pass list to all data reactives
      input_choices$temp_crop_dfs <- r6$data_choices
      input_choices$perm_crop_dfs <- r6$data_choices
      input_choices$processed_dfs <- r6$data_choices
      input_choices$anim_dfs <- r6$data_choices
      input_choices$oth_anim_prod_dfs <- r6$data_choices

      # ------------------------------------------------------------------------
      # update values
      # but do not trigger reactive
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        # raises crops
        "crop_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "crop_val",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # raises livestock
        "livestock_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "livestock_val",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # temporary crops
        "temp_crop_df",  updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "temp_crop_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "temp_crop_val",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # permanent crops
        "perm_crop_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "perm_crop_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "perm_crop_val",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # processed crop products
        "processed_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "processed_var",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "processed_val",  updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        # livestock
        "anim_df",       updateSelectInput,    list(
          choices = r6$data_choices,
          selected = NULL
        ),
        "sold_live_anim_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "slaughter_anim_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "slaughter_anim_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "sold_live_poultry_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "slaughter_poultry_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "slaughter_poultry_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "milk_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "milk_val",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "eggs_var",       updateSelectInput,    list(
          choices = NULL,
          selected = NULL
        ),
        "eggs_val",       updateSelectInput,    list(
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
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$income_sources_provided)) {

      shiny::req(
        # data directory
        r6$dirs$micro_combine,
        # household data
        r6$income_sources_hhold_df_choices,
        r6$income_sources_hhold_df,
        # grew crops variable
        r6$income_sources_crops_var_choices,
        r6$income_sources_crops_var,
        # grew crops value
        r6$income_soures_crops_val_choices,
        r6$income_soures_crops_val,
        # raised livestock variable
        r6$income_soures_livestock_var_choices,
        r6$income_soures_livestock_var,
        # raised livestock value
        r6$income_soures_livestock_val_choices,
        r6$income_soures_livestock_val,
        # temporary crop data
        r6$income_sources_temp_crop_df_choices,
        r6$income_sources_temp_crop_df,
        # temporary crop variable
        r6$income_sources_temp_crop_var_choices,
        r6$income_sources_temp_crop_var,
        # temporary crop value
        r6$income_sources_temp_crop_val_choices,
        r6$income_sources_temp_crop_val,
        # permanent crop data
        r6$income_sources_perm_crop_df_choices,
        r6$income_sources_perm_crop_df,
        # permanent crop variable
        r6$income_sources_perm_crop_var_choices,
        r6$income_sources_perm_crop_var,
        # permanent crop value
        r6$income_sources_perm_crop_val_choices,
        r6$income_sources_perm_crop_val,
        # processed crop product data
        r6$income_sources_processed_df_choices,
        r6$income_sources_processed_df,
        # processed crop product variable
        r6$income_sources_processed_var_choices,
        r6$income_sources_processed_var,
        # processed crop product value
        r6$income_sources_processed_val_choices,
        r6$income_sources_processed_val,
        # livestock data
        r6$income_sources_anim_df_choices,
        r6$income_sources_anim_df,
        # sold live animals variable
        r6$income_sources_sold_live_anim_var_choices,
        r6$income_sources_sold_live_anim_var,
        # sold live animals value
        r6$income_sources_sold_live_anim_val_choices,
        r6$income_sources_sold_live_anim_val,
        # sold slaughtered animals variable
        r6$income_sources_slaughter_anim_var_choices,
        r6$income_sources_slaughter_anim_var,
        # sold slaughtered animals value
        r6$income_sources_slaughter_anim_val_choices,
        r6$income_sources_slaughter_anim_val,
        # sold live poultry variable
        r6$income_sources_sold_live_poultry_var_choices,
        r6$income_sources_sold_live_poultry_var,
        # sold live poultry value
        r6$income_sources_sold_live_poultry_val_choices,
        r6$income_sources_sold_live_poultry_val,
        # sold slaughtered poultry variable
        r6$income_sources_slaughter_poultry_var_choices,
        r6$income_sources_slaughter_poultry_var,
        # sold slaughtered poultry value
        r6$income_sources_slaughter_poultry_val_choices,
        r6$income_sources_slaughter_poultry_val,
        # sold milk variable
        r6$income_sources_milk_var_choices,
        r6$income_sources_milk_var,
        # sold milk value
        r6$income_sources_milk_val_choices,
        r6$income_sources_milk_val,
        # sold eggs variable
        r6$income_sources_eggs_var_choices,
        r6$income_sources_eggs_var,
        # sold eggs value
        r6$income_sources_eggs_val_choices,
        r6$income_sources_eggs_val,
        # other animal products data
        r6$income_sources_oth_anim_prod_df_choices,
        r6$income_sources_oth_anim_prod_df,
        # sold other animal products variable
        r6$income_sources_oth_anim_prod_var_choices,
        r6$income_sources_oth_anim_prod_var,
        # sold other animal products value
        r6$income_sources_oth_anim_prod_val_choices,
        r6$income_sources_oth_anim_prod_val
      )

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "hhold_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$income_sources_hhold_df
        ),
        # raises crops
        "crop_var",  updateSelectInput,    list(
          choices = r6$income_sources_crops_var_choices,
          selected = r6$income_sources_crops_var
        ),
        "crop_val",  updateSelectInput,    list(
          choices = r6$income_sources_crops_val_choices,
          selected = r6$income_sources_crops_val
        ),
        # raises livestock
        "livestock_var",  updateSelectInput,    list(
          choices = r6$income_soures_livestock_var_choices,
          selected = r6$income_soures_livestock_var
        ),
        "livestock_val",  updateSelectInput,    list(
          choices = r6$income_soures_livestock_val_choices,
          selected = r6$income_soures_livestock_val
        ),
        # temporary crops
        "temp_crop_df",  updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$income_sources_temp_crop_df
        ),
        "temp_crop_var",  updateSelectInput,    list(
          choices = r6$income_sources_temp_crop_var_choices,
          selected = r6$income_sources_temp_crop_var
        ),
        "temp_crop_val",  updateSelectInput,    list(
          choices = r6$income_sources_temp_crop_val_choices,
          selected = r6$income_sources_temp_crop_val
        ),
        # permanent crops
        "perm_crop_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$income_sources_perm_crop_df
        ),
        "perm_crop_var",  updateSelectInput,    list(
          choices = r6$income_sources_perm_crop_var_choices,
          selected = r6$income_sources_perm_crop_var
        ),
        "perm_crop_val",  updateSelectInput,    list(
          choices = r6$income_sources_perm_crop_val_choices,
          selected = r6$income_sources_perm_crop_val
        ),
        # processed crop products
        "processed_df",   updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$income_sources_processed_df
        ),
        "processed_var",  updateSelectInput,    list(
          choices = r6$income_sources_processed_var_choices,
          selected = r6$income_sources_processed_var
        ),
        "processed_val",  updateSelectInput,    list(
          choices = r6$income_sources_processed_val_choices,
          selected = r6$income_sources_processed_val
        ),
        # livestock
        "anim_df",       updateSelectInput,    list(
          choices = r6$data_choices,
          selected = r6$income_sources_anim_df
        ),
        "sold_live_anim_var",       updateSelectInput,    list(
          choices = r6$income_sources_sold_live_anim_var_choices,
          selected = r6$income_sources_sold_live_anim_var
        ),
        "slaughter_anim_var",       updateSelectInput,    list(
          choices = r6$income_sources_slaughter_anim_var_choices,
          selected = r6$income_sources_slaughter_anim_var
        ),
        "slaughter_anim_val",       updateSelectInput,    list(
          choices = r6$income_sources_slaughter_anim_val_choices,
          selected = r6$income_sources_slaughter_anim_val
        ),
        "sold_live_poultry_var",       updateSelectInput,    list(
          choices = r6$income_sources_sold_live_poultry_var_choices,
          selected = r6$income_sources_sold_live_poultry_var
        ),
        "slaughter_poultry_var",       updateSelectInput,    list(
          choices = r6$income_sources_slaughter_poultry_var_choices,
          selected = r6$income_sources_slaughter_poultry_var
        ),
        "slaughter_poultry_val",       updateSelectInput,    list(
          choices = r6$income_sources_slaughter_poultry_val_choices,
          selected = r6$income_sources_slaughter_poultry_val
        ),
        "milk_var",       updateSelectInput,    list(
          choices = r6$income_sources_milk_var_choices,
          selected = r6$income_sources_milk_var
        ),
        "milk_val",       updateSelectInput,    list(
          choices = r6$income_sources_milk_val_choices,
          selected = r6$income_sources_milk_val
        ),
        "eggs_var",       updateSelectInput,    list(
          choices = r6$income_sources_eggs_var_choices,
          selected = r6$income_sources_eggs_var
        ),
        "eggs_val",       updateSelectInput,    list(
          choices = r6$income_sources_eggs_val_choices,
          selected = r6$income_sources_eggs_val
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

      # grew crops variables
      input_choices$crop_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$hhold_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # raised livestock variables
      input_choices$livestock_vars <- r6$dirs$micro_combine |>
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
        "crop_var",   updateSelectInput,    list(
          choices = input_choices$crop_vars,
          selected = NULL
        ),
        "livestock_var",  updateSelectInput,    list(
          choices = input_choices$livestock_vars,
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
    # crop variable -> values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$crop_var, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$crop_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$crop_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$crop_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "crop_val",   updateSelectInput,    list(
          choices = input_choices$crop_vals,
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
    # livestock variable -> values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$livestock_var, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$livestock_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$livestock_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$livestock_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "livestock_val",   updateSelectInput,    list(
          choices = input_choices$crop_vals,
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
    # temporary crop data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$temp_crop_df, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$temp_crop_df
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # grew crops variables
      input_choices$temp_crop_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$temp_crop_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )


      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "temp_crop_var",   updateSelectInput,    list(
          choices = input_choices$temp_crop_vars,
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
    # temporary crop variable -> values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$temp_crop_var, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$temp_crop_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$temp_crop_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$temp_crop_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "temp_crop_val",   updateSelectInput,    list(
          choices = input_choices$temp_crop_vals,
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
    # permanent crop data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$perm_crop_df, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$perm_crop_df
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # grew crops variables
      input_choices$perm_crop_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$perm_crop_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )


      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "perm_crop_var",   updateSelectInput,    list(
          choices = input_choices$perm_crop_vars,
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
    # permanent crop variable -> values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$perm_crop_var, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$perm_crop_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$perm_crop_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$perm_crop_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "perm_crop_val",   updateSelectInput,    list(
          choices = input_choices$perm_crop_vals,
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
    # processed crop products data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$processed_df, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$processed_df
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # grew crops variables
      input_choices$processed_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$processed_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )


      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "processed_var",   updateSelectInput,    list(
          choices = input_choices$processed_vars,
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
    # livestock data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$anim_df, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$anim_df
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # load variables data frame from disk
      qnr_vars_df <- fs::path(r6$dirs$qnr, "qnr_vars.rds") |>
        readRDS()

      # numeric: number of live animals sold
      input_choices$sold_live_anim_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$anim_df, ".dta")) |>
        make_data_var_choices(
          vars_df = qnr_vars_df,
          var_type = "numeric"
        )
      input_choices$sold_live_poultry_var <- input_choices$sold_live_anim_var

      # single-select
      input_choices$sold_live_anim_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$anim_df, ".dta")) |>
        make_data_var_choices(
          vars_df = qnr_vars_df,
          var_type = "numeric"
        )
      slaughter_anim_vars <- input_choices$sold_live_anim_var
      slaughter_poultry_vars <- input_choices$sold_live_anim_var
      milk_vars <- input_choices$sold_live_anim_var
      eggs_vars <- input_choices$sold_live_anim_var

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "sold_live_anim_var",   updateSelectInput,    list(
          choices = input_choices$sold_live_anim_vars,
          selected = NULL
        ),
        "slaughter_anim_var",   updateSelectInput,    list(
          choices = input_choices$slaughter_anim_vars,
          selected = NULL
        ),
        "sold_live_poultry_var",  updateSelectInput,    list(
          choices = input_choices$sold_live_poultry_vars,
          selected = NULL
        ),
        "slaughter_poultry_var",  updateSelectInput,    list(
          choices = input_choices$slaughter_poultry_vars,
          selected = NULL
        ),
        "milk_var",  updateSelectInput,    list(
          choices = input_choices$milk_vars,
          selected = NULL
        ),
        "eggs_var",  updateSelectInput,    list(
          choices = input_choices$egg_vars,
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
    # slaughter animals variable -> values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$slaughter_anim_var, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$slaughter_anim_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$slaughter_anim_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$slaughter_anim_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "slaughter_anim_val",   updateSelectInput,    list(
          choices = input_choices$slaughter_anim_vals,
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
    # slaughter poultry variable -> values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$slaughter_poultry_var, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$slaughter_poultry_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$slaughter_poultry_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$slaughter_poultry_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "slaughter_poultry_val",   updateSelectInput,    list(
          choices = input_choices$slaughter_poultry_vals,
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
    # milk variable -> values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$milk_var, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$milk_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$milk_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$milk_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "milk_val",   updateSelectInput,    list(
          choices = input_choices$milk_vals,
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
    # eggs variable -> values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$eggs_var, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$eggs_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      input_choices$eggs_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$eggs_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "eggs_val",   updateSelectInput,    list(
          choices = input_choices$eggs_vals,
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
    # other animal products data -> variables in data
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$oth_anim_prod_df, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$oth_anim_prod_df
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # sold any products variables
      input_choices$oth_anim_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$oth_anim_prod_df, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "oth_anim_var",   updateSelectInput,    list(
          choices = input_choices$oth_anim_vars,
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
    # sold other animal products variable -> values
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$oth_anim_var, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$oth_anim_var
      )

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # compute choices
      input_choices$oth_anim_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$oth_anim_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      input_specs <- tibble::tribble(
        ~ id,             ~ updater,            ~ args,
        "oth_anim_val",   updateSelectInput,    list(
          choices = input_choices$oth_anim_vals,
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
      r6$income_sources_hhold_df_choices <- input_choices$hhold_dfs
      r6$income_sources_hhold_df <- input$hhold_df
      # raises crops
      r6$income_sources_crop_var_choices <- input_choices$crop_vars
      r6$income_sources_crop_var <- input$crop_var
      r6$income_sources_crop_val_choices <- input_choices$crop_vals
      r6$income_sources_crop_val <- input$crop_val
      # raises livestock
      r6$income_sources_livestock_var_choices <- input_choices$livestock_vars
      r6$income_sources_livestock_var <- input$livestock_var
      r6$income_sources_livestock_val_choices <- input_choices$livestock_vals
      r6$income_sources_livestock_val <- input$livestock_val
      # temporary crop data
      r6$income_sources_temp_crop_df_choices <- input_choices$temp_crop_dfs
      r6$income_sources_temp_crop_df <- input$temp_crop_df
      # temporary crops
      r6$income_sources_temp_crop_var_choices <- input_choices$temp_crop_vars
      r6$income_sources_temp_crop_var <- input$temp_crop_var
      r6$income_sources_temp_crop_vals <- input_choices$temp_crop_vals
      r6$income_sources_temp_crop_val <- input$temp_crop_val
      # permanent crop data
      r6$income_sources_perm_crop_df_choices <- input_choices$perm_crop_dfs
      r6$income_sources_perm_crop_df <- input$perm_crop_df
      # permanent crops
      r6$income_sources_perm_crop_var_choices <- input_choices$perm_crop_vars
      r6$income_sources_perm_crop_var <- input$perm_crop_var
      r6$income_sources_perm_crop_vals <- input_choices$perm_crop_vals
      r6$income_sources_perm_crop_val <- input$perm_crop_val
      # processed crop data
      r6$income_sources_processed_df_choices <- input_choices$processed_dfs
      r6$income_sources_processed_df <- input$processed_df
      # processed crop products
      r6$income_sources_processed_var_choices <- input_choices$processed_vars
      r6$income_sources_processed_var <- input$processed_var
      r6$income_sources_processed_vals <- input_choices$processed_vals
      r6$income_sources_processed_val <- input$processed_val
      # livestock data
      r6$income_sources_anim_df_choices <- input_choices$anim_dfs
      r6$income_sources_anim_df <- input$anim_df
      # livestock
      r6$income_sources_sold_live_anim_var_choices <- input_choices$sold_live_anim_vars
      r6$income_sources_sold_live_anim_var <- input$sold_live_anim_var
      r6$income_sources_slaughter_anim_var_choices <- input_choices$slaughter_anim_vars
      r6$income_sources_slaughter_anim_var <- input$slaughter_anim_var
      r6$income_sources_slaughter_anim_val_choices <- input_choices$slaughter_anim_vals
      r6$income_sources_slaughter_anim_val <- input$slaughter_anim_val
      r6$income_sources_sold_live_poultry_var_choices <- input_choices$sold_live_poultry_vars
      r6$income_sources_sold_live_poultry_var <- input$sold_live_poultry_var
      r6$income_sources_slaughter_poultry_var_choices <- input_choices$slaughter_poultry_vars
      r6$income_sources_slaughter_poultry_var <- input$slaughter_poultry_var
      r6$income_sources_slaughter_poultry_val_choices <- input_choices$slaughter_poultry_vals
      r6$income_sources_slaughter_poultry_val <- input$slaughter_poultry_val
      r6$income_sources_milk_var_choices <- input_choices$milk_vars
      r6$income_sources_milk_var <- input$milk_var
      r6$income_sources_milk_val_choices <- input_choices$milk_vals
      r6$income_sources_milk_val <- input$milk_val
      r6$income_sources_eggs_var_choices <- input_choices$eggs_vars
      r6$income_sources_eggs_var <- input$eggs_var
      r6$income_sources_eggs_val_choice <- input_choices$eggs_vals
      r6$income_sources_eggs_val <- input$eggs_val
      # save action
      r6$income_sources_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_income_sources_sales")

    })

  })

}

## To be copied in the UI
# mod_4_quality_1_setup_2_data_income_sources_ui("4_quality_1_setup_2_data_income_sources_1")

## To be copied in the server
# mod_4_quality_1_setup_2_data_income_sources_server("4_quality_1_setup_2_data_income_sources_1")
