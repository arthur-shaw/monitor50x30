#' 3_complete_1_setup_2_clusters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    bslib::accordion(
      id = ns("clusters"),
      bslib::accordion_panel(
        title = "Quantify",
        value = "quantify",
        mod_3_complete_1_setup_2_clusters_1_quantify_ui(
          ns("3_complete_1_setup_2_clusters_1_quantify_1")
        )
      ),
      bslib::accordion_panel(
        title = "Identify for computers",
        value = "identify_for_computers",
        mod_3_complete_1_setup_2_clusters_2_computer_id_ui(
          ns("3_complete_1_setup_2_clusters_2_computer_id_1")
        )
      ),
      bslib::accordion_panel(
        title = "Identify for survey managers",
        value = "identify_for_managers",
        mod_3_complete_1_setup_2_clusters_3_human_id_ui(
          ns("3_complete_1_setup_2_clusters_3_human_id_1")
        )
      ),

    )
 
  )
}
    
#' 3_complete_1_setup_2_clusters Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # load server logic of child modules
    # ==========================================================================

    # note: parent param passes the session down to descendent modules
    mod_3_complete_1_setup_2_clusters_1_quantify_server(
      id = "3_complete_1_setup_2_clusters_1_quantify_1",
      parent = session,
      r6 = r6
    )
    mod_3_complete_1_setup_2_clusters_2_computer_id_server(
      id = "3_complete_1_setup_2_clusters_2_computer_id_1",
      parent = session,
      r6 = r6
    )
    mod_3_complete_1_setup_2_clusters_3_human_id_server(
      id = "3_complete_1_setup_2_clusters_3_human_id_1",
      parent = session,
      r6 = r6
    )

    # ==========================================================================
    # move focus between top-level child modules
    # ==========================================================================

    # from quantify to identify for computers
    gargoyle::on("save_quantify_clusters", {

      # close quantify
      bslib::accordion_panel_close(
        id = "clusters",
        value = "quantify"
      )

      # open identify for computers
      bslib::accordion_panel_open(
        id = "clusters",
        value = "identify_for_computers"
      )

    })

    # from identify for computers to identify for survey managers
    gargoyle::on("save_computer_identify_clusters", {

      # close identify for computers
      bslib::accordion_panel_close(
        id = "clusters",
        value = "identify_for_computers"
      )

      # open identify for managers
      bslib::accordion_panel_open(
        id = "clusters",
        value = "identify_for_managers"
      )

    })

    # from identify for survey managers to workloads
    gargoyle::on("save_manager_identify_clusters", {

      # close identify for computers
      bslib::accordion_panel_close(
        id = "clusters",
        value = "identify_for_managers"
      )

      # open workloads
      # see mod_3_complete_1_setup.R

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_ui("3_complete_1_setup_2_clusters_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_server("3_complete_1_setup_2_clusters_1")
