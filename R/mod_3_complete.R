#' 3_complete UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        # settings
        id = ns("settings"),
        title = shiny::span(
          "Settings",
          shiny::icon("cog"),
          class = "sidebar-title"
        ),
        position = "left",
        width = "75%",
        open = FALSE,
        # contents
        mod_3_complete_1_setup_ui(ns("3_complete_1_setup_1"))
      ),
      # settings
      fillable = FALSE,
      # contents
      shiny::tags$p("Main area"),
      mod_3_complete_2_report_ui(ns("3_complete_2_report_1"))
    )

  )
}
    
#' 3_complete Server Functions
#'
#' @noRd 
mod_3_complete_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # when do not have (all) settings provided, open the sidebar
    # otherwise, leave it in its default (closed) state
    if (is.null(r6$completeness_settings_saved)) {

      bslib::toggle_sidebar(
        id = "settings",
        open = TRUE
      )

    }

    # ==========================================================================
    # load server logic of child modules
    # ==========================================================================

    # note: parent param passes the session down to descendent modules
    mod_3_complete_1_setup_server(
      id = "3_complete_1_setup_1",
      parent = session,
      r6 = r6
    )
    mod_3_complete_2_report_server(
      id = "3_complete_2_report_1",
      parent = session,
      r6 = r6
    )

    # ==========================================================================
    # react to saving settings
    # ==========================================================================

    # when save button pressed in settings sidebar
    gargoyle::on("save_completeness_setup", {

      bslib::toggle_sidebar(
        id = "settings",
        open = NULL
      )

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_ui("3_complete_1")
    
## To be copied in the server
# mod_3_complete_server("3_complete_1")
