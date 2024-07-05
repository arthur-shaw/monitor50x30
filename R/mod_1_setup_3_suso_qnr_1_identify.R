#' 1_setup_3_suso_qnr_1_identify UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_setup_3_suso_qnr_1_identify_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::textInput(
      inputId = ns("qnr_string"),
      label = bslib::popover(
        trigger = list(
          "Provide a string that identifies the questionnaire(s) of interest",
          bsicons::bs_icon("info-circle")
        ),
        'To do so, provide either a substring (e.g., "ILP", "post-planting")',
        ' or a ', 
        htmltools::a(
          'regular expression',
          href = "https://regexlearn.com/"
        ),
        ' (e.g., "[Pp]ost-[Pp]lanting")'
      )
    ),
    shiny::actionButton(
      inputId = ns("search"),
      label = "Search"
    ),
    reactable::reactableOutput(
      outputId = ns("qnrs")
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 1_setup_3_suso_qnr_1_identify Server Functions
#'
#' @noRd 
mod_1_setup_3_suso_qnr_1_identify_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # load info from past session, if applicable
    if (!is.null(r6$qnrs_identified)) {

      if (r6$qnrs_identified == TRUE) {

        shiny::updateTextInput(
          inputId = "qnr_string",
          value = r6$qnr_string
        )

        output$qnrs <- reactable::renderReactable({
          reactable::reactable(
            data = r6$matching_qnr_tbl,
            columns = list(
              questionnaireId = reactable::colDef(show = FALSE)
            )
          )
        })

      }

    }
    # otherwise, load list of all questionnaires from server after creds set
    gargoyle::on("save_creds", {
      # fetch all questionnaires
      qnrs <- susoapi::get_questionnaires(
        server = r6$server,
        workspace = r6$workspace,
        user = r6$user,
        password = r6$password
      ) |>
        dplyr::select(
          .data$title, .data$version, .data$variable,
          .data$questionnaireId
        )

      # show them
      output$qnrs <- reactable::renderReactable({
        reactable::reactable(
          data = qnrs,
          columns = list(
            questionnaireId = reactable::colDef(show = FALSE)
          )
        )
      })
    })

    # ==========================================================================
    # react to search button
    # ==========================================================================

    # create a reactive container for the table
    # so that its values are accessible outside of the observeEvent scope
    matching_qnrs <- shiny::reactiveValues(df = r6$matching_qnr_tbl)

    shiny::observeEvent(input$search, {

      matching_qnrs$df <- susoapi::get_questionnaires(
        server = r6$server,
        workspace = r6$workspace,
        user = r6$user,
        password = r6$password
      ) |>
        dplyr::filter(
          grepl(
            x = .data$title,
            pattern = input$qnr_string
          )
        ) |>
        dplyr::select(
          .data$title, .data$version, .data$variable,
          .data$questionnaireId
        )

      output$qnrs <- reactable::renderReactable({
        reactable::reactable(
          data = matching_qnrs$df,
          columns = list(
            questionnaireId = reactable::colDef(show = FALSE)
          )
        )

      })

    })

    # ==========================================================================
    # react to save button
    # ==========================================================================


    shiny::observeEvent(input$save, {

      # write parameters to R6
      r6$qnr_string <- input$qnr_string
      r6$matching_qnr_tbl <- matching_qnrs$df
      r6$qnrs_identified <- TRUE

      # write R6 object to disk
      r6$write()

      # send signal that SuSo questionnaires have been identified
      gargoyle::trigger("save_suso_qnr_identify")

    })

  })
}
    
## To be copied in the UI
# mod_1_setup_3_suso_qnr_1_identify_ui("1_setup_3_suso_qnr_1_identify_1")
    
## To be copied in the server
# mod_1_setup_3_suso_qnr_1_identify_server("1_setup_3_suso_qnr_1_identify_1")
