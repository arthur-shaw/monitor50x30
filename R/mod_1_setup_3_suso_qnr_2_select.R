#' 1_setup_3_suso_qnr_2_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_setup_3_suso_qnr_2_select_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
 
    reactable::reactableOutput(
      outputId = ns("main_qnr")
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save - select"
    )

  )
}
    
#' 1_setup_3_suso_qnr_2_select Server Functions
#'
#' @noRd 
mod_1_setup_3_suso_qnr_2_select_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # ==========================================================================
    # initialize page
    # ==========================================================================

    if (!is.null(r6$qnr_selected)) {

      if (r6$qnr_selected == TRUE) {

        output$main_qnr <- reactable::renderReactable({
          reactable::reactable(
            data = r6$matching_qnr_tbl,
            selection = "single",
            defaultSelected = r6$qnr_selected_index,
            columns = list(
              questionnaireId = reactable::colDef(show = FALSE)
            )
          )
        })

      }

    } else {

      output$main_qnr <- reactable::renderReactable({
        reactable::reactable(
          data = r6$matching_qnr_tbl,
          selection = "single",
          columns = list(
            questionnaireId = reactable::colDef(show = FALSE)
          )
        )
      })

    }

    # ==========================================================================
    # update page
    # ==========================================================================

    # when questionaires updated change
    gargoyle::on("save_suso_qnr_identify", {

      output$main_qnr <- reactable::renderReactable({
        reactable::reactable(
          data = r6$matching_qnr_tbl,
          selection = "single",
          columns = list(
            questionnaireId = reactable::colDef(show = FALSE)
          )
        )
      })

    })

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # get index of selected row
      qnr_selected_index <- reactable::getReactableState(
        outputId = "main_qnr",
        name = "selected"
      )

      # react to whether or not a selection was made
      if (is.null(qnr_selected_index)) {

        shinyFeedback::showToast(
          type = "error",
          message = "No questionnaire selected."
        )

      } else if (!is.null(qnr_selected_index)) {

        # save params to R6
        r6$qnr_selected_index <- qnr_selected_index
        selected_qnr <- r6$qnr_selected_suso_id <- r6$matching_qnr_tbl |>
          dplyr::filter(dplyr::row_number() == qnr_selected_index)
        r6$qnr_selected_suso_id <- selected_qnr |>
          dplyr::pull(rlang::.data$questionnaireId)
        r6$qnr_selected_suso_version <- dplyr::pull(selected_qnr, rlang::.data$version)
        r6$qnr_selected <- TRUE

        # write R6 to disk
        r6$write()

        # send signal that questionnaire selected and questionnaire work done
        gargoyle::trigger("save_suso_qnr_select")
        gargoyle::trigger("save_suso_qnr")

      }

    })

  })
}
    
## To be copied in the UI
# mod_1_setup_3_suso_qnr_2_select_ui("1_setup_3_suso_qnr_2_select_1")
    
## To be copied in the server
# mod_1_setup_3_suso_qnr_2_select_server("1_setup_3_suso_qnr_2_select_1")
