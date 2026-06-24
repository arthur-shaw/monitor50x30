#' 4_quality_1_setup_2_data_cow_displacement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_cow_displacement_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = "Household-level data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("animal_var"),
      label = "Livestock ownership variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("bull_val"),
      label = "Code for bulls",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("cow_val"),
      label = "Code for cows",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("steer_heifer_val"),
      label = "Code for steers / heifers",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("calf_val"),
      label = "Code for calves (male and female)",
      choices = NULL,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_quality_1_setup_2_data_cow_displacement Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_cow_displacement_server <- function(id, r6, parent){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      animal_vars = r6$cow_displacement_var_choices,
      animal_vals = r6$cow_displacement_val_choices
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
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choices = r6$data_choices,
        selected = NULL
      )

      # ------------------------------------------------------------------------
      # (re)set to `NULL` variable and value selections
      # but do not trigger reactive
      # ------------------------------------------------------------------------

      # livestock ownership variable
      shiny::freezeReactiveValue(input, "animal_var")
      shiny::updateSelectInput(
        inputId = "animal_var",
        choices = NULL,
        selected = NULL
      )

      # bulls code
      shiny::freezeReactiveValue(input, "bull_val")
      shiny::updateSelectInput(
        inputId = "bull_val",
        choices = NULL,
        selected = NULL
      )

      # cow code
      shiny::freezeReactiveValue(input, "cow_val")
      shiny::updateSelectInput(
        inputId = "cow_val",
        choices = NULL,
        selected = NULL
      )

      # steer/heifer code
      shiny::freezeReactiveValue(input, "steer_heifer_val")
      shiny::updateSelectInput(
        inputId = "steer_heifer_val",
        choices = NULL,
        selected = NULL
      )

      # calf code
      shiny::freezeReactiveValue(input, "calf_val")
      shiny::updateSelectInput(
        inputId = "calf_val",
        choices = NULL,
        selected = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load NULL values if values not previously saved
    # --------------------------------------------------------------------------

    if (is.null(r6$cow_displacement_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = NULL
      )

      # livestock ownership variable
      shiny::freezeReactiveValue(input, "animal_var")
      shiny::updateSelectInput(
        inputId = "animal_var",
        choice = NULL,
        selected = NULL
      )

      # bulls code
      shiny::freezeReactiveValue(input, "bull_val")
      shiny::updateSelectInput(
        inputId = "bull_val",
        choices = NULL,
        selected = NULL
      )

      # cow code
      shiny::freezeReactiveValue(input, "cow_val")
      shiny::updateSelectInput(
        inputId = "cow_val",
        choices = NULL,
        selected = NULL
      )

      # steer/heifer code
      shiny::freezeReactiveValue(input, "steer_heifer_val")
      shiny::updateSelectInput(
        inputId = "steer_heifer_val",
        choices = NULL,
        selected = NULL
      )

      # calf code
      shiny::freezeReactiveValue(input, "calf_val")
      shiny::updateSelectInput(
        inputId = "calf_val",
        choices = NULL,
        selected = NULL
      )

    }

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$cow_displacement_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = r6$cow_displacement_df
      )

      # livestock ownership variable
      shiny::freezeReactiveValue(input, "animal_var")
      shiny::updateSelectInput(
        inputId = "animal_var",
        choice = r6$cow_displacement_animal_var_choices,
        selected = r6$cow_displacement_animal_var
      )

      # bulls code
      shiny::freezeReactiveValue(input, "bull_val")
      shiny::updateSelectInput(
        inputId = "bull_val",
        choices = r6$cow_displacement_val_choices,
        selected = r6$cow_displacement_bull_val
      )

      # cow code
      shiny::freezeReactiveValue(input, "cow_val")
      shiny::updateSelectInput(
        inputId = "cow_val",
        choices = r6$cow_displacement_val_choices, 
        selected = r6$cow_displacement_cow_val
      )

      # steer/heifer code
      shiny::freezeReactiveValue(input, "steer_heifer_val")
      shiny::updateSelectInput(
        inputId = "steer_heifer_val",
        choices = r6$cow_displacement_val_choices,
        selected = r6$cow_displacement_steer_heifer_val
      )

      # calf code
      shiny::freezeReactiveValue(input, "calf_val")
      shiny::updateSelectInput(
        inputId = "calf_val",
        choices = r6$cow_displacement_val_choices,
        selected = r6$cow_displacement_calf_val
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

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # make livestock variable choices
      input_choices$animal_vars <- r6$dirs$micro_combine |>
        fs::path(paste0(input$data, ".dta")) |>
        make_data_var_choices(
          vars_df = r6$qnr_vars_df,
          var_type = "multi-select"
        )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      # livestock ownership
      shiny::freezeReactiveValue(input, "animal_var")
      shiny::updateSelectInput(
        inputId = "animal_var",
        choices = input_choices$animal_vars,
        selected = NULL
      )

      # bulls code
      shiny::freezeReactiveValue(input, "bull_val")
      shiny::updateSelectInput(
        inputId = "bull_val",
        choices = NULL,
        selected = NULL
      )

      # cow code
      shiny::freezeReactiveValue(input, "cow_val")
      shiny::updateSelectInput(
        inputId = "cow_val",
        choices = NULL,
        selected = NULL
      )

      # steer/heifer code
      shiny::freezeReactiveValue(input, "steer_heifer_val")
      shiny::updateSelectInput(
        inputId = "steer_heifer_val",
        choices = NULL,
        selected = NULL
      )

      # calf code
      shiny::freezeReactiveValue(input, "calf_val")
      shiny::updateSelectInput(
        inputId = "calf_val",
        choices = NULL,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # livestock ownership variable
    # --------------------------------------------------------------------------

    shiny::observeEvent(input$animal_var, {

      shiny::req(input$animal_var)

      # ------------------------------------------------------------------------
      # compute choices
      # ------------------------------------------------------------------------

      # compute choices
      input_choices$animal_vals <- make_val_options(
        json_path = r6$json_path,
        categories_dir = r6$categories_dir,
        varname = extract_var_names(input$animal_var)
      )

      # ------------------------------------------------------------------------
      # update choices in the UI
      # ------------------------------------------------------------------------

      # bulls code
      shiny::freezeReactiveValue(input, "bull_val")
      shiny::updateSelectInput(
        inputId = "bull_val",
        choices = input_choices$animal_vals,
        selected = NULL
      )

      # cow code
      shiny::freezeReactiveValue(input, "cow_val")
      shiny::updateSelectInput(
        inputId = "cow_val",
        choices = input_choices$animal_vals,
        selected = NULL
      )

      # steer/heifer code
      shiny::freezeReactiveValue(input, "steer_heifer_val")
      shiny::updateSelectInput(
        inputId = "steer_heifer_val",
        choices = input_choices$animal_vals,
        selected = NULL
      )

      # calf code
      shiny::freezeReactiveValue(input, "calf_val")
      shiny::updateSelectInput(
        inputId = "calf_val",
        choices = input_choices$animal_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6

      # data
      r6$cow_displacement_df_choices <- input_choices$data
      r6$cow_displacement_df  <- input$data
      # livestock ownership variable
      r6$cow_displacement_animal_var_choices <- input_choices$animal_vars
      r6$cow_displacement_animal_var <- input$animal_var
      # livestock values
      r6$cow_displacement_val_choices <- input_choices$animal_vals
      # codes for bull, cow, steer/heifer, and calf
      r6$cow_displacement_bull_val <- input$bull_val
      r6$cow_displacement_cow_val <- input$cow_val
      r6$cow_displacement_steer_heifer_val <- input$steer_heifer_val
      r6$cow_displacement_calf_val <- input$calf_val
      # save action
      r6$cow_displacement_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_cow_displacement")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_cow_displacement_ui("4_quality_1_setup_2_data_cow_displacement_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_cow_displacement_server("4_quality_1_setup_2_data_cow_displacement_1")
