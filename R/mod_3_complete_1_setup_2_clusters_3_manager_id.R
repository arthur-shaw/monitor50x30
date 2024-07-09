#' 3_complete_1_setup_2_clusters_3_manager_id UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_3_manager_id_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
 
    bslib::accordion(
      id = ns("manager_id_steps"),
      open = FALSE,
      bslib::accordion_panel(
        title = "1. Select variables",
        value = "select_vars_panel",
      ),
      bslib::accordion_panel(
        title = "2. Order variables",
        value = "order_vars_panel",
      ),
      bslib::accordion_panel(
        title = "3. Compose description",
        value = "compose_desc_panel",
      )
    )

  )
}
    
#' 3_complete_1_setup_2_clusters_3_manager_id Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_3_manager_id_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # load child module definitions
    # ==========================================================================


    # ==========================================================================
    # move from one accordion panel to the next
    # ==========================================================================

    # from computer ID to select
    gargoyle::on("save_computer_identify_clusters", {

      # open order
      bslib::accordion_panel_open(
        id = "manager_id_steps",
        value = "select_vars_panel"
      )

    })

    # from select to order
    gargoyle::on("save_manager_select_clusters", {

      # close select
      bslib::accordion_panel_close(
        id = "manager_id_steps",
        value = "select_vars_panel"
      )

      # open order
      bslib::accordion_panel_open(
        id = "manager_id_steps",
        value = "order_vars_panel"
      )

    })

    # from order to compose
    gargoyle::on("save_manager_order_clusters", {

      # close order
      bslib::accordion_panel_close(
        id = "manager_id_steps",
        value = "order_vars_panel"
      )

      # open compose
      bslib::accordion_panel_open(
        id = "manager_id_steps",
        value = "compose_desc_panel"
      )

    })

    # from compose to team workload
    # note: opening team workload is managed in complete report setup module
    gargoyle::on("save_manager_compose_clusters", {

      # close compose
      bslib::accordion_panel_close(
        id = "manager_id_steps",
        value = "compose_desc_panel"
      )

    })


  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_3_manager_id_ui("3_complete_1_setup_2_clusters_3_manager_id_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_3_manager_id_server("3_complete_1_setup_2_clusters_3_manager_id_1")
