#' 4_quality_1_setup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(

    bslib::accordion(
      id = ns("setup"),
      icon = fontawesome::fa(name = "cogs"),
      bslib::accordion_panel(
        title = "Tables",
        value = "tables",
        mod_4_quality_1_setup_1_tables_ui(
          id = ns("4_quality_1_setup_1_tables_1")
        )
      ),
      bslib::accordion_panel(
        title = "Data",
        value = "data",
        mod_4_quality_1_setup_2_data_ui(
          id = ns("4_quality_1_setup_2_data_1")
        )
      ),
      bslib::accordion_panel(
        title = "Interviews",
        value = "interviews",
        id = mod_4_quality_1_setup_3_interviews_ui(
          ns("4_quality_1_setup_3_interviews_1")
        )
      )

    )
  )
}
    
#' 4_quality_1_setup Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # load server logic of child modules
    # ==========================================================================

    mod_4_quality_1_setup_1_tables_server(
      id = "4_quality_1_setup_1_tables_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_server(
      id = "4_quality_1_setup_2_data_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_3_interviews_server(
      id = "4_quality_1_setup_3_interviews_1",
      parent = session,
      r6 = r6
    )

    # ==========================================================================
    # manage opening/closing of accordion panels
    # ==========================================================================

    gargoyle::on("save_tables", {

      # close table selections
      bslib::accordion_panel_close(
        id = "setup",
        values = "tables"
      )

      # open data for tables
      bslib::accordion_panel_open(
        id = "setup",
        values = "data"
      )

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_ui("4_quality_1_setup_1")
    
## To be copied in the server
# mod_4_quality_1_setup_server("4_quality_1_setup_1")
