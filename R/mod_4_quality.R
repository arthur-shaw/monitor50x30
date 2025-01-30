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
        title = shiny::span(
          "Settings",
          shiny::icon("cog"),
          class = "sidebar-title"
        ),
        position = "left",
        width = "75%",
        open = FALSE,
        # contents
        mod_4_quality_1_setup_ui(ns("4_quality_1_setup_1"))
      ),
      # settings
      fillable = FALSE,
      # contents
      mod_4_quality_2_report_ui(ns("4_quality_2_report_1"))
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

    # ==========================================================================
    # load server logic of child modules
    # ==========================================================================

    # note: parent param passes the session down to descendent modules
    mod_4_quality_1_setup_server(
      id = "4_quality_1_setup_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_2_report_server(
      id = "4_quality_2_report_1",
      parent = session,
      r6 = r6
    )

  })
}

## To be copied in the UI
# mod_4_quality_ui("4_quality_1")
    
## To be copied in the server
# mod_4_quality_server("4_quality_1")
