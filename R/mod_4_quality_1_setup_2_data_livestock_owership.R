#' 4_quality_1_setup_2_data_livestock_owership UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_livestock_owership_ui <- function(id) {
  ns <- NS(id)
  tagList(

    shiny::selectInput(
      inputId = ns("data"),
      label = label_tooltip(
        lbl = "Data: Households.",
        desc = "The main, household-level data set."
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("animal_var"),
      label = label_tooltip(
        lbl = "Question: Which livestock owned.",
        desc = paste(
          "The question that indicates, yes or no,",
          "which livestock are owned from a list.",
          "The question text contains 'keep' and 'own'.",
          "Typing either of these may help find the right question."
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

#' 4_quality_1_setup_2_data_livestock_owership Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_livestock_owership_server <- function(id, r6, parent){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      animal_vars = r6$livestock_ownership_var_choices
    )

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # when data are downloaded, compute the choices and update the choices
    gargoyle::on("download_data", {

      shiny::req(r6$dirs$micro_combine)

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

      shiny::freezeReactiveValue(input, "animal_var")
      shiny::updateSelectInput(
        inputId = "animal_var",
        choices = NULL,
        selected = NULL
      )

    })

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (is.null(r6$livestock_ownership_provided)) {

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

    }

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$livestock_ownership_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = r6$livestock_ownership_df
      )

      # livestock ownership variable
      shiny::freezeReactiveValue(input, "animal_var")
      shiny::updateSelectInput(
        inputId = "animal_var",
        choice = r6$livestock_ownership_animal_var_choices,
        selected = r6$livestock_ownership_animal_var
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

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6

      # data
      r6$livestock_ownership_df_choices <- input_choices$data
      r6$livestock_ownership_df  <- input$data
      # crop ID
      r6$livestock_ownership_animal_var_choices <- input_choices$animal_vars
      r6$livestock_ownership_animal_var <- input$animal_var
      # save action
      r6$livestock_ownership_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_livestock_ownership")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_livestock_owership_ui("4_quality_1_setup_2_data_livestock_owership_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_livestock_owership_server("4_quality_1_setup_2_data_livestock_owership_1")
