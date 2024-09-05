#' 1_setup_3_suso_qnr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_setup_3_suso_qnr_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    bslib::accordion(
      id = ns("suso_qnr_accordion"),
      open = FALSE,
      class = "accordion-flush",

      bslib::accordion_panel(
        id = "identify_qnrs_panel",
        title = "Identify all",
        value = "identify_qnrs_panel",
        mod_1_setup_3_suso_qnr_1_identify_ui(
          ns("1_setup_3_suso_qnr_1_identify_1")
        )
      ),

      bslib::accordion_panel(
        title = "Select main",
        value = "select_qnr_panel",
        mod_1_setup_3_suso_qnr_2_select_ui(
          ns("1_setup_3_suso_qnr_2_select_1")
        )
      )

    )

  )
}

#' 1_setup_3_suso_qnr Server Functions
#'
#' @noRd
mod_1_setup_3_suso_qnr_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # load child module definitions
    mod_1_setup_3_suso_qnr_1_identify_server(
      "1_setup_3_suso_qnr_1_identify_1",
      parent = session,
      r6 = r6
    )
    mod_1_setup_3_suso_qnr_2_select_server(
      "1_setup_3_suso_qnr_2_select_1",
      parent = session,
      r6 = r6
    )

    # move from one accordion panel to the next

    # from SuSo credentials to SuSo questionnaire
    gargoyle::on("save_creds", {

      # open next
      bslib::accordion_panel_open(
        id = "suso_qnr_accordion",
        value = "identify_qnrs_panel"
      )

    })

    # from identify to select
    gargoyle::on("save_suso_qnr_identify", {

      # close current
      bslib::accordion_panel_close(
        id = "suso_qnr_accordion",
        value = "identify_qnrs_panel"
      )
      # open next
      bslib::accordion_panel_open(
        id = "suso_qnr_accordion",
        value = "select_qnr_panel"
      )

    })

    # from select to instrument templates
    # note: opening of instrument templates is managed in setup module
    gargoyle::on("save_suso_qnr_select", {

      # close current
      bslib::accordion_panel_close(
        id = "suso_qnr_accordion",
        value = "select_qnr_panel"
      )

    })

  })
}

## To be copied in the UI
# mod_1_setup_3_suso_qnr_ui("1_setup_3_suso_qnr_1")

## To be copied in the server
# mod_1_setup_3_suso_qnr_server("1_setup_3_suso_qnr_1")
