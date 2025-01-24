#' 1_setup_4_template UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_setup_4_template_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::selectizeInput(
      inputId = ns("qnr_templates"),
      label = "Which template(s)?",
      choices = c(
        "CORE-AG",
        "ILP",
        "ILS",
        "PME",
        "MEA"
      ),
      multiple = TRUE,
      selected = NULL
    ),
    shiny::selectizeInput(
      inputId = ns("qnr_extensions"),
      label = "Which optional extension(s)?",
      choices = c(
        "None",
        "Crop labor inputs",
        "Input use",
        "Livestock labor"
      ),
      multiple = TRUE,
      selected = NULL
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 1_setup_4_template Server Functions
#'
#' @noRd 
mod_1_setup_4_template_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # initialize page
    # ==========================================================================

    if (!is.null(r6$template_details_provided)) {

      if (r6$template_details_provided == TRUE) {

        shiny::updateSelectizeInput(
          inputId = "qnr_templates",
          selected = r6$qnr_templates
        )
        shiny::updateSelectizeInput(
          inputId = "qnr_extensions",
          selected = r6$qnr_extensions
        )

      }

    }

    # ==========================================================================
    # update template selection based on last selection
    # ==========================================================================

    # treat CORE, PME, and MEA as special templates
    # if only one template selected, do nothing
    # if more than one template selected, evaluate which one to keep
    # if any is a special template, update the selection to be only last one
    # if there is no special template, do nothing
    shiny::observeEvent(input$qnr_templates, {

      # prevents race condition where observer fire before UI loaded
      shiny::req(input$qnr_templates)

      if (length(input$qnr_templates) > 1) {

        # take the last entry in the selective input selection
        # this works because it's a vector "stack", where the last selection is
        # added to the end of the vector
        last_template_selected <- shiny::reactive({
          utils::tail(input$qnr_templates, n = 1)
        })

        if (any(input$qnr_templates %in% c("CORE-AG", "PME", "MEA"))) {

          shiny::updateVarSelectInput(
            inputId = "qnr_templates",
            selected = last_template_selected()
          )

        }

      }

    },
    ignoreInit = TRUE)

    # ==========================================================================
    # update extension selection options
    # ==========================================================================

    # if "none" is among the selections, update to the last selection
    # if the last is "none", update to "none"
    # if the last is not "none", update to that selection
    # otherwise, do nothing
    shiny::observeEvent(input$qnr_extensions, {

      # prevents race condition where observer fire before UI loaded
      shiny::req(input$qnr_extensions)

      last_extension_selected <- shiny::reactive({
        utils::tail(input$qnr_extensions, n = 1)
      })

      if (any(input$qnr_extensions %in% "None")) {

        shiny::updateSelectizeInput(
          inputId = "qnr_extensions",
          selected = last_extension_selected()
        )

      }

    },
    ignoreInit = TRUE)

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # react to whether or not all fields are filled
      # if not, provide user feedback
      if (is.null(input$qnr_templates) | is.null(input$qnr_extensions)) {

        shinyFeedback::showToast(
          type = "error",
          message = "Questionnaire template and/or extension not provided."
        )

      # if so, capture and save user inputs
      } else if (
        !is.null(input$qnr_templates) &
        !is.null(input$qnr_extensions)
      ) {

        # save params to R6
        r6$qnr_templates <- input$qnr_templates
        r6$qnr_extensions <- input$qnr_extensions
        r6$template_details_provided <- TRUE

        # write R6 to disk
        r6$write()

        # send signal that templates details saved
        gargoyle::trigger("save_template")

      }

    })

  })
}

## To be copied in the UI
# mod_1_setup_4_template_ui("1_setup_4_template_1")
    
## To be copied in the server
# mod_1_setup_4_template_server("1_setup_4_template_1")
