#' 4_quality UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
 
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        # settings
        id = ns("settings"),
        title = shiny::tagList(shiny::icon("cog"), "Settings"),
        position = "left",
        width = "75%",
        open = FALSE,
        # contents
        shiny::tags$p("Sidebar"),
        shiny::actionButton(
          inputId = ns("save_settings"),
          label = "Save"
        )
      ),
      # settings
      fillable = FALSE,
      # contents
      shiny::tags$p("Main area")
    )

  )
}
    
#' 4_quality Server Functions
#'
#' @noRd 
mod_4_quality_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # TODO: toggle sidebar state
    # when do not have (all) settings provided
    # if (r6$saved_completeness_settings == FALSE) {

    #   bslib::toggle_sidebar(
    #     id = "settings",
    #     open = TRUE
    #   )

    # }

    # when save button pressed in settings sidebar
    shiny::observeEvent(input$save_settings, {

      bslib::toggle_sidebar(
        id = "settings",
        open = FALSE
      )

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_ui("4_quality_1")
    
## To be copied in the server
# mod_4_quality_server("4_quality_1")
