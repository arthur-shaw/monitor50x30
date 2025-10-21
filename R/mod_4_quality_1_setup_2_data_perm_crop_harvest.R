#' 4_quality_1_setup_2_data_perm_crop_harvest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_perm_crop_harvest_ui <- function(id, r6, module) {
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
      label = "Crops that are permanent crops",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::selectInput(
      inputId = ns("harvest_var"),
      label = "Question on whether the crop was harvested",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("harvest_val"),
      label = "Value indicating that the crop was harvested",
      choices = NULL,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )
  )

}

#' 4_quality_1_setup_2_data_perm_crop_harvest Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_perm_crop_harvest_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      crop_id_var = r6$perm_crop_harvest_crop_id_var_choices,
      crop_id_vals = r6$perm_crop_harvest_crop_vals_choices,
      harvest_var = r6$perm_crop_harvest_harvest_var_choices,
      harvest_var_vals = r6$perm_crop_harvest_harvest_val_choices
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
      shiny::freezeReactiveValue(input, "harvest_var")
      shiny::updateSelectInput(
        inputId = "harvest_var",
        choices = NULL,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "harvest_val")
      shiny::updateNumericInput(
        inputId = "harvest_val",
        value = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load NULL values if never saved vals
    # --------------------------------------------------------------------------

    # when data are downloaded, compute the choices and update the choices
    if (is.null(r6$perm_crop_sales_provided)) {

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
      shiny::freezeReactiveValue(input, "harvest_var")
      shiny::updateSelectInput(
        inputId = "harvest_var",
        choices = NULL,
        selected = NULL
      )
      shiny::freezeReactiveValue(input, "harvest_val")
      shiny::updateNumericInput(
        inputId = "harvest_val",
        value = NULL
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
        selected = r6$perm_crop_harvest_df
      )

      # crop ID variable
      shiny::freezeReactiveValue(input, "crop_id_var")
      shiny::updateSelectInput(
        inputId = "crop_id_var",
        choice = r6$perm_crop_harvest_crop_id_var_choices,
        selected = r6$perm_crop_harvest_crop_id_var
      )

      # crop ID values
      shiny::freezeReactiveValue(input, "crop_vals")
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choice = r6$perm_crop_harvest_crop_vals_choices,
        selected = r6$perm_crop_harvest_crop_vals
      )

      # harvest variable
      shiny::freezeReactiveValue(input, "harvest_var")
      shiny::updateSelectInput(
        inputId = "harvest_var",
        choice = r6$perm_crop_harvest_harvest_var_choices,
        selected = r6$perm_crop_harvest_harvest_var
      )

      # harvest value
      shiny::freezeReactiveValue(input, "harvest_val")
      shiny::updateSelectInput(
        inputId = "harvest_val",
        choice = r6$perm_crop_harvest_harvest_val_choices,
        selected = r6$perm_crop_harvest_harvest_val
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

      # make crop ID variable choices
      input_choices$crop_id_var <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_id_var_choices()

      # make choices harvest variable
      input_choices$harvest_var <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "single-select"
        )

      # update in UI
      # crop ID variable
      shiny::freezeReactiveValue(input, "crop_id_var")
      shiny::updateSelectInput(
        inputId = "crop_id_var",
        choices = input_choices$crop_id_var,
        selected = NULL
      )
      # crop ID values
      shiny::freezeReactiveValue(input, "crop_vals")
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choices = NULL,
        selected = NULL
      )
      # harvest variable
      shiny::freezeReactiveValue(input, "harvest_var")
      shiny::updateSelectInput(
        inputId = "harvest_var",
        choices = input_choices$harvest_var,
        selected = NULL
      )
      # harvest values
      shiny::freezeReactiveValue(input, "harvest_val")
      shiny::updateSelectInput(
        inputId = "harvest_val",
        choices = NULL,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)
      shiny::req(input$crop_id_var)

      # make crop ID value choices
      input_choices$crop_id_vals <- make_id_val_options(
        path = fs::path(
          r6$dirs$micro_combine,
          paste0(input$data, ".dta")
        ),
        varname = extract_id_var_names(input$crop_id_var)
      )

      # update choices in UI
      shiny::freezeReactiveValue(input, "crop_vals")
      shiny::updateSelectInput(
        inputId = "crop_vals",
        choices = input_choices$crop_id_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # harvest variable -> harvest_val
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$harvest_var, {

      shiny::req(input$crop_id_var)

      # make harvest value choices
      input_choices$harvest_var_vals <- make_val_options(
        qnr_df = r6$qnr_vars_df,
        categories_df = r6$q_categories_df,
        varname = extract_var_names(input$harvest_var)
      )

      # update choices in UI
      shiny::freezeReactiveValue(input, "harvest_val")
      shiny::updateSelectInput(
        inputId = "harvest_val",
        choices = input_choices$harvest_var_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6
      # data
      r6$perm_crop_harvest_df_choices <- input_choices$data
      r6$perm_crop_harvest_df  <- input$data
      # crop ID variable
      r6$perm_crop_harvest_crop_id_var_choices <- input_choices$crop_id_var
      r6$perm_crop_harvest_crop_id_var <- input$crop_id_var
      # crop values
      r6$perm_crop_harvest_crop_vals_choices <- input_choices$crop_id_vals
      r6$perm_crop_harvest_crop_vals <- input$crop_vals
      # harvest variable
      r6$perm_crop_harvest_harvest_var_choices <- input_choices$harvest_var
      r6$perm_crop_harvest_harvest_var <- input$harvest_var
      # harvest value
      r6$perm_crop_harvest_harvest_val_choices <- input_choices$harvest_var_vals
      r6$perm_crop_harvest_harvest_val <- input$harvest_val

      # save action
      r6$perm_crop_sales_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_perm_crop_harvest")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_perm_crop_harvest_ui("4_quality_1_setup_2_data_perm_crop_harvest_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_perm_crop_harvest_server("4_quality_1_setup_2_data_perm_crop_harvest_1")
