#' 3_complete_1_setup_2_clusters_3_manager_id_2_order UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_3_manager_id_2_order_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::uiOutput(ns("cluster_vars_order_widget")),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 3_complete_1_setup_2_clusters_3_manager_id_2_order Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_3_manager_id_2_order_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # compute choices
    # initialize as the values stored in R6
    # if never set, the value will be null
    manager_id_vars <- shiny::reactiveValues(
      order = r6$manager_id_vars_order
    )

    # if cluster vars provided in session
    gargoyle::on("save_manager_select_clusters", {

      manager_id_vars$order <- r6$manager_id_vars_selected

    })

    # render the UI
    # note: this appears in the server since there is not a method for updating
    # the labels in the UI
    output$cluster_vars_order_widget <- shiny::renderUI({

      sortable::bucket_list(
        header = paste0(
          "Please sort the variables in the order in which they should appear ",
          "in reports for survey managers"
        ),
        sortable::add_rank_list(
          text = "Click and drag items to reorder them",
          labels = manager_id_vars$order,
          input_id = ns("cluster_vars_ordered")
        )
      )

    })

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # store user inputs in R6
      r6$manager_id_vars_order <- input$cluster_vars_ordered
      r6$cluster_var_order_provided <- TRUE

      # write R6 to disk
      r6$write()

      # signal that cluster ID variables have been selected
      gargoyle::trigger("save_manager_order_clusters")

    })

  })

}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_3_manager_id_2_order_ui("3_complete_1_setup_2_clusters_3_manager_id_2_order_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_3_manager_id_2_order_server("3_complete_1_setup_2_clusters_3_manager_id_2_order_1")
