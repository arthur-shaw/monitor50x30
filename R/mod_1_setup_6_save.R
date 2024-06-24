#' 1_setup_6_save UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_setup_6_save_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::tags$p(
      "
      The app stores your project settings in a special folder on your device.
      Those settings will be automatically restored from that file each time 
      you use the app.
      However, this file can also be downloaded and saved elsewhere for safe
      keeping.
      There are at three main reasons this may make sense for you:
      ",
      shiny::tags$li(
        shiny::tags$strong("Sharing."),
        "
        You may want to share settings among a team performing data 
        quality management tasks for the survey project.
        To do this, download the settings file and share it with collaborators.
        "
      ),
      shiny::tags$li(
        shiny::tags$strong("Switching."),
        "
        You may need to move between several survey projects, each with its own
        settings file.
        To do so, download a settings file for each project.
        "
      ),
      shiny::tags$li(
        shiny::tags$strong("Upgrading."),
        "
        You need to update the app. 
        The settings file exists in folder that is specific to each version 
        of the application.
        "
      )
    ),
    shiny::downloadButton(
      outputId = ns("download"),
      label = "Download"
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )
 
  )
}
    
#' 1_setup_6_save Server Functions
#'
#' @noRd 
mod_1_setup_6_save_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # react to download button
    # ==========================================================================

    output$download <- shiny::downloadHandler(
      filename = "saved_params.rds",
      content = function(file) {
        fs::file_copy(
          path = fs::path(r6$app_dir, "saved_params.rds"),
          new_path = file
        )
      }
    )

    # ==========================================================================
    # react to save button
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # write settings status to R6
      r6$core_settings_saved <- TRUE

      # write settings status to disk
      r6$write()

      # signal that settings saved
      gargoyle::trigger("save_settings")

    })

  })
}
    
## To be copied in the UI
# mod_1_setup_6_save_ui("1_setup_6_save_1")
    
## To be copied in the server
# mod_1_setup_6_save_server("1_setup_6_save_1")
