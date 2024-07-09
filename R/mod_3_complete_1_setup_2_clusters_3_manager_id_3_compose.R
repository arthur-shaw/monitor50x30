#' 3_complete_1_setup_2_clusters_3_manager_id_3_compose UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_2_clusters_3_manager_id_3_compose_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::textInput(
      inputId = ns("cluster_vars_template"),
      label = bslib::popover(
        # where popover is invoked
        trigger = list(
          "Compose the text to identify clusters",
          bsicons::bs_icon("info-circle")
        ),
        # content of popover text
        paste0(
          "This text provides a template for how a PSU should appear in the ",
          "completeness report.",
          "As a conveniene, it has been constructed from the variables that ",
          "where previously selected and ordered.",
          "The text can be edited as needed and saved. ",
          "The text in curly brackets are variable names (e.g., `{s00_q01}`). ",
          "They will be replaced with the value of the variable .",
          "The text outside before the curly braces are variable labels ",
          "(e.g., `District`)"
        )
      ),
      value = ""
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 3_complete_1_setup_2_clusters_3_manager_id_3_compose Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_2_clusters_3_manager_id_3_compose_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # initialize page
    # ==========================================================================

    if (is.null(r6$cluster_template_provided)) {

      cluster_template_txt <- paste(r6$manager_id_vars_order, collapse = " , ")

      shiny::updateTextInput(
        inputId = "cluster_vars_template",
        value = cluster_template_txt
      )

    } else if (!is.null(r6$cluster_template_provided)) {

      shiny::updateTextInput(
        inputId = "cluster_vars_template",
        value = r6$cluster_template_txt
      )

    }

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # capture inputs in R6
      r6$cluster_template_txt <- input$cluster_vars_template
      r6$cluster_template_provided <- TRUE

      # write R6 to disk
      r6$write()

      # signal that cluster ID variables have been selected
      gargoyle::trigger("save_manager_compose_clusters")

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_2_clusters_3_manager_id_3_compose_ui("3_complete_1_setup_2_clusters_3_manager_id_3_compose_1")
    
## To be copied in the server
# mod_3_complete_1_setup_2_clusters_3_manager_id_3_compose_server("3_complete_1_setup_2_clusters_3_manager_id_3_compose_1")
