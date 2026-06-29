#' 4_quality_1_setup_2_data_hen_displacement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_hen_displacement_ui <- function(id) {
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
    shiny::selectInput(
      inputId = ns("cock_val"),
      label = label_tooltip(
        lbl = "Value: Cocks / broilers",
        desc = paste(
          "Livestock code corresponding to cocks / broilers",
          "In the public SuSo template, code 51."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("hen_val"),
      label = label_tooltip(
        lbl = "Value: Hens / layers",
        desc = paste(
          "Livestock code corresponding to hens / layers",
          "In the public SuSo template, code 52."
        )
      ),
      choices = NULL,
      selected = NULL
    ),
    shiny::selectInput(
      inputId = ns("pullet_val"),
      label = label_tooltip(
        lbl = "Value: Pullets / DOCs",
        desc = paste(
          "Livestock code corresponding to pullets / DOCs",
          "In the public SuSo template, code 53."
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

#' 4_quality_1_setup_2_data_hen_displacement Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_hen_displacement_server <- function(id, r6, parent){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # create a reactive container for input choices
    # ==========================================================================

    input_choices <- shiny::reactiveValues(
      data = r6$data_choices,
      animal_vars = r6$hen_displacement_animal_var_choices,
      animal_vals = r6$hen_displacement_val_choices
    )

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # --------------------------------------------------------------------------
    # when data are downloaded
    # --------------------------------------------------------------------------

    gargoyle::on("download_data", {

      # update UI to reflect data choices
      # but do not trigger reactive
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choices = r6$data_choices,
        selected = NULL
      )

      # livestock ownership variable
      shiny::freezeReactiveValue(input, "animal_var")
      shiny::updateSelectInput(
        inputId = "animal_var",
        choices = NULL,
        selected = NULL
      )

      # cock code
      shiny::freezeReactiveValue(input, "cock_val")
      shiny::updateSelectInput(
        inputId = "cock_val",
        choices = NULL,
        selected = NULL
      )

      # hen code
      shiny::freezeReactiveValue(input, "hen_val")
      shiny::updateSelectInput(
        inputId = "hen_val",
        choices = NULL,
        selected = NULL
      )

      # pullet code
      shiny::freezeReactiveValue(input, "pullet_val")
      shiny::updateSelectInput(
        inputId = "pullet_val",
        choices = NULL,
        selected = NULL
      )

    })

    # --------------------------------------------------------------------------
    # when vals not previously saved
    # --------------------------------------------------------------------------

    if (is.null(r6$hen_displacement_provided)) {

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

      # cock code
      shiny::freezeReactiveValue(input, "cock_val")
      shiny::updateSelectInput(
        inputId = "cock_val",
        choices = NULL,
        selected = NULL
      )

      # hen code
      shiny::freezeReactiveValue(input, "hen_val")
      shiny::updateSelectInput(
        inputId = "hen_val",
        choices = NULL,
        selected = NULL
      )

      # pullet
      shiny::freezeReactiveValue(input, "pullet_val")
      shiny::updateSelectInput(
        inputId = "pullet_val",
        choices = NULL,
        selected = NULL
      )

    }

    # --------------------------------------------------------------------------
    # load past selections from R6
    # --------------------------------------------------------------------------

    if (!is.null(r6$hen_displacement_provided)) {

      # data
      shiny::freezeReactiveValue(input, "data")
      shiny::updateSelectInput(
        inputId = "data",
        choice = r6$data_choices,
        selected = r6$hen_displacement_df
      )

      # livestock ownership variable
      shiny::freezeReactiveValue(input, "animal_var")
      shiny::updateSelectInput(
        inputId = "animal_var",
        choice = r6$hen_displacement_animal_var_choices,
        selected = r6$hen_displacement_animal_var
      )

      # cock code
      shiny::freezeReactiveValue(input, "cock_val")
      shiny::updateSelectInput(
        inputId = "cock_val",
        choices = r6$hen_displacement_val_choices,
        selected = r6$hen_displacement_cock_val
      )

      # hen code
      shiny::freezeReactiveValue(input, "hen_val")
      shiny::updateSelectInput(
        inputId = "hen_val",
        choices = r6$hen_displacement_val_choices,
        selected = r6$hen_displacement_hen_val
      )

      # pullet
      shiny::freezeReactiveValue(input, "pullet_val")
      shiny::updateSelectInput(
        inputId = "pullet_val",
        choices = r6$hen_displacement_val_choices,
        selected = r6$hen_displacement_pullet_val
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

      # cock code
      shiny::freezeReactiveValue(input, "cock_val")
      shiny::updateSelectInput(
        inputId = "cock_val",
        choices = NULL,
        selected = NULL
      )

      # hen code
      shiny::freezeReactiveValue(input, "hen_val")
      shiny::updateSelectInput(
        inputId = "hen_val",
        choices = NULL,
        selected = NULL
      )

      # pullet code
      shiny::freezeReactiveValue(input, "pullet_val")
      shiny::updateSelectInput(
        inputId = "pullet_val",
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

      # cock code
      shiny::freezeReactiveValue(input, "cock_val")
      shiny::updateSelectInput(
        inputId = "cock_val",
        choices = input_choices$animal_vals,
        selected = NULL
      )

      # hen code
      shiny::freezeReactiveValue(input, "hen_val")
      shiny::updateSelectInput(
        inputId = "hen_val",
        choices = input_choices$animal_vals,
        selected = NULL
      )

      # pullet code
      shiny::freezeReactiveValue(input, "pullet_val")
      shiny::updateSelectInput(
        inputId = "pullet_val",
        choices = input_choices$animal_vals,
        selected = NULL
      )

    }, ignoreInit = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture values in R6

      # data
      r6$hen_displacement_df_choices <- input_choices$data
      r6$hen_displacement_df  <- input$data
      # livestock ownership variable
      r6$hen_displacement_animal_var_choices <- input_choices$animal_vars
      r6$hen_displacement_animal_var <- input$animal_var
      # livestock values
      r6$hen_displacement_val_choices <- input_choices$animal_vals
      # codes for cock, hen, and pullet
      r6$hen_displacement_cock_val <- input$cock_val
      r6$hen_displacement_hen_val <- input$hen_val
      r6$hen_displacement_pullet_val <- input$pullet_val
      # save action
      r6$hen_displacement_provided <- TRUE

      # write R6 to disk
      r6$write()

      # send signal that info provided
      gargoyle::trigger("saved_hen_displacement")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_hen_displacement_ui("4_quality_1_setup_2_data_hen_displacement_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_hen_displacement_server("4_quality_1_setup_2_data_hen_displacement_1")
