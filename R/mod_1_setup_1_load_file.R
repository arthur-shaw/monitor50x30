#' 1_setup_1_load_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_setup_1_load_file_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::radioButtons(
      inputId = ns("have_setup_file"),
      label = "Do you have a setup file for this application?",
      choices = list("Yes" = 1, "No" = 2),
      selected = 2
    ),
    shiny::fileInput(
      inputId = ns("setup_file"),
      label = "Please select the file",
      accept = ".rds"
    ),
    shiny::actionButton(
      inputId = ns("fill"),
      label = "Next"
    ),
    shiny::actionButton(
      inputId = ns("load"),
      label = "Load"
    )
 
  )
}
    
#' 1_setup_1_load_file Server Functions
#'
#' @noRd 
mod_1_setup_1_load_file_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # =========================================================================
    # initialize page
    # =========================================================================

    # hide UI elements that depend on other elements
    shinyjs::hide(id = "setup_file")
    shinyjs::hide(id = "fill")
    shinyjs::hide(id = "load")

    # =========================================================================
    # update UI
    # =========================================================================

    # whether have setup file
    shiny::observeEvent(input$have_setup_file, {

      # have setup file
      if (input$have_setup_file == 1) {

        # re-hide next button, if needed
        shinyjs::hide(id = "fill")

        # show prompt to load setup file
        shinyjs::show(id = "setup_file")

      # do not have setup file
      } else if (input$have_setup_file == 2) {

        # re-hide UI elements, if needed
        shinyjs::hide(id = "setup_file")
        shinyjs::hide(id = "load")

        # show next button to advance in filling out settings
        shinyjs::show(id = "fill")

      }

    })

    # once have selected setup file
    shiny::observeEvent(input$setup_file, {
      shinyjs::show(id = "load")
    })

    # ==========================================================================
    # trigger event
    # ==========================================================================

    # load existing setup file
    shiny::observeEvent(input$load, {
      gargoyle::trigger("load_setup")
    })

    # fill out necessary setup info
    shiny::observeEvent(input$fill, {
      gargoyle::trigger("need_setup")
    })

  })
}
    
## To be copied in the UI
# mod_1_setup_1_load_file_ui("1_setup_1_load_file_1")
    
## To be copied in the server
# mod_1_setup_1_load_file_server("1_setup_1_load_file_1")
