#' 4_quality_1_setup_2_data_parcels_per_hhold UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_parcels_per_hhold_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = "Parcel data set",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("use"),
      label = "Question on the parcel's main use(s)",
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("use_val"),
      label = "Values of the use question that denote an agricultural use",
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_quality_1_setup_2_data_parcels_per_hhold Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_parcels_per_hhold_server <- function(id,  parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # --------------------------------------------------------------------------
    # create reactive container for input choices
    # --------------------------------------------------------------------------

    input_choices <- shiny::reactiveValues(
      data = r6$parcels_per_hhold_df_choices,
      use = r6$parcels_per_hhold_use_choices,
      use_val = r6$parcels_per_hhold_use_val_choices
    )

    # --------------------------------------------------------------------------
    # compute choices from downloaded data
    # --------------------------------------------------------------------------

    # when data are downloaded, compute the choices and update the choices
    gargoyle::on("download_data", {

      shiny::req(r6$dirs$micro_combine)

      # get list of data files in combined folder
      input_choices$data <- r6$dirs$micro_combine |>
        make_data_choices()

      # update UI to reflect data choices
      shiny::updateSelectInput(
        inputId = "data",
        choices = input_choices$data,
        selected = NULL
      )

      # (re)set to `NULL` variable and value selections
      shiny::updateSelectInput(
        inputId = "use",
        choices = NULL,
        selected = NULL
      )
      shiny::updateSelectInput(
        inputId = "use_val",
        choices = NULL,
        selected = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$parcels_per_hhold_provided)) {

      shiny::req(
        r6$parcels_per_hhold_df_choices, r6$parcels_per_hhold_df,
        r6$parcels_per_hhold_use_choices, r6$parcels_per_hhold_use,
        r6$parcels_per_hhold_use_val_choices, r6$parcels_per_hhold_use_val
      )

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$parcels_per_hhold_df_choices,
        selected = r6$parcels_per_hhold_df
      )

      # parcel use variable
      shiny::freezeReactiveValue(input, "use")
      shiny::updateSelectInput(
        inputId = "use",
        choice = r6$parcels_per_hhold_use_choices,
        selected = r6$parcels_per_hhold_use
      )

      # parcel use values
      shiny::freezeReactiveValue(input, "use_val")
      shiny::updateSelectInput(
        inputId = "use_val",
        choice = r6$parcels_per_hhold_use_val_choices,
        selected = r6$parcels_per_hhold_use_val
      )

    }

    # ==========================================================================
    # react to data choice
    # ==========================================================================

    shiny::observeEvent(input$data, {

      shiny::req(
        r6$dirs$qnr, r6$dirs$micro_combine,
        input$data, input_choices$data
      )

      # load variables data frame from disk
      qnr_vars_df <- fs::path(r6$dirs$qnr, "qnr_vars.rds") |>
        readRDS()

      # get variable names from selected file
      input_choices$use <- fs::path(
          r6$dirs$micro_combine,
          paste0(input$data, ".dta")
        ) |>
        make_data_var_choices(
          vars_df = qnr_vars_df,
          var_type = "categorical"
        )

      # update parcel use in UI
      shiny::updateSelectInput(
        inputId = "use",
        choices = input_choices$use,
        selected = NULL
      )

      # update use vals to `NULL` since prior choice, if any, invalidated
      shiny::updateSelectInput(
        inputId = "use_val",
        choices = NULL,
        selected = NULL
      )

    },
    ignoreInit = TRUE)

    # ==========================================================================
    # react to variable choice
    # ==========================================================================

    shiny::observeEvent(input$use, {

      shiny::req(
        r6$dirs$qnr,
        input$use
      )

      # load questionnaire data frame from disk
      qnr_df <- fs::path(r6$dirs$qnr, "qnr_full.rds") |>
        readRDS()

      # extract variable from variable choice character string
      use_var_selected <- extract_var_names(input$use)

      # get use value answer options
      input_choices$use_val <- make_val_options(
        qnr_df = qnr_df,
        varname = use_var_selected
      )

      # set values choices in the UI
      shiny::updateSelectInput(
        inputId = "use_val",
        choices = input_choices$use_val
      )

    },
    ignoreInit = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6
      # data
      r6$parcels_per_hhold_df_choices <- input_choices$data
      r6$parcels_per_hhold_df <- input$data
      # use variable
      r6$parcels_per_hhold_use_choices <- input_choices$use
      r6$parcels_per_hhold_use <- input$use
      # use values
      r6$parcels_per_hhold_use_val_choices <- as.character(
        input_choices$use_val
      )
      r6$parcels_per_hhold_use_val <- as.character(input$use_val)
      # save action
      r6$parcels_per_hhold_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_parcels_per_hhold")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_parcels_per_hhold_ui("4_quality_1_setup_2_data_parcels_per_hhold_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_parcels_per_hhold_server("4_quality_1_setup_2_data_parcels_per_hhold_1")
