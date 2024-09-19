#' 1_setup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_1_setup_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    selectInput(
      inputId = ns("selected_language"),
      label = i18n$t("Change language"),
      choices = setNames(
        i18n$get_languages(),
        c("English", "French")
      ),
      selected = "fr"
    ),

    bslib::accordion(
      id = ns("setup"),
      icon = fontawesome::fa(name = "cogs"),
      bslib::accordion_panel(
        title = i18n$t("Load setup file"),
        value = "load_setup_panel",
        mod_1_setup_1_load_file_ui(ns("1_setup_1_load_file_1"))
      ),
      bslib::accordion_panel(
        title = i18n$t("Provide server credentials"),
        value = "provide_creds_panel",
        mod_1_setup_2_suso_creds_ui(ns("1_setup_2_suso_creds_1"))
      ),
      bslib::accordion_panel(
        title = i18n$t("Survey Solutions questionnaire"),
        value = "suso_qnr_panel",
        # class = "p-0",
        mod_1_setup_3_suso_qnr_ui(ns("1_setup_3_suso_qnr_1"))
      ),
      # bslib::accordion(
      #   id = ns("give_oth_details_accordion"),
        open = FALSE,
        bslib::accordion_panel(
          title = i18n$t("Survey instrument template"),
          value = "survey_instrument_panel",
          open = FALSE,
          mod_1_setup_4_template_ui(ns("1_setup_4_template_1"))
        ),
        bslib::accordion_panel(
          title = i18n$t("Survey visit"),
          value = "survey_visit_panel",
          mod_1_setup_5_visit_ui(ns("1_setup_5_visit_1"))
        ),
        bslib::accordion_panel(
          title = i18n$t("Save settings"),
          value = "save_settings_panel",
          mod_1_setup_6_save_ui(ns("1_setup_6_save_1"))
        # )
      )

    )
  )
}

#' 1_setup Server Functions
#'
#' @noRd
mod_1_setup_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Change the language according to user input
    observeEvent(input$selected_language, {
      update_lang(input$selected_language)
    })


    # load server logic of child modules
    # note: parent param passes the session down to descendent modules
    mod_1_setup_1_load_file_server(
      id = "1_setup_1_load_file_1",
      parent = session,
      r6 = r6
    )
    mod_1_setup_2_suso_creds_server(
      id = "1_setup_2_suso_creds_1",
      parent = session,
      r6 = r6
    )
    mod_1_setup_3_suso_qnr_server(
      id = "1_setup_3_suso_qnr_1",
      parent = session,
      r6 = r6
    )
    mod_1_setup_4_template_server(
      id = "1_setup_4_template_1",
      parent = session,
      r6 = r6
    )
    mod_1_setup_5_visit_server(
      id = "1_setup_5_visit_1",
      parent = session,
      r6 = r6
    )
    mod_1_setup_6_save_server(
      id = "1_setup_6_save_1",
      parent = session,
      r6 = r6
    )

    # move focus to the next accordion panel

    # from setup file to SuSo credentials
    gargoyle::on("need_setup", {

      # close current
      bslib::accordion_panel_close(
        id = "setup",
        value = "load_setup_panel"
      )
      # open next
      bslib::accordion_panel_open(
        id = "setup",
        value = "provide_creds_panel"
      )

    })

    # from SuSo credentials to SuSo questionnaire
    gargoyle::on("save_creds", {

      # close current
      bslib::accordion_panel_close(
        id = "setup",
        value = "provide_creds_panel"
      )
      # open next
      bslib::accordion_panel_open(
        id = "setup",
        value = "suso_qnr_panel"
      )
      gargoyle::trigger("open_suso_qnr")

    })


    # from SuSo questionnaire to survey instrument template
    gargoyle::on("save_suso_qnr_select", {

      # close current
      # - done in mod_1_setup_3_suso_qnr.R
      # open next
      bslib::accordion_panel_open(
        id = "setup",
        value = "survey_instrument_panel"
      )

    })

    gargoyle::on("save_suso_qnr", {

      # close the panel contaiing sub-panels
      bslib::accordion_panel_close(
        id = "setup",
        values = "suso_qnr_panel"
      )

      # open next sibling panel
      bslib::accordion_panel_open(
        id = "setup",
        values =  "survey_instrument_panel"
      )

    })

    # from survey instrument to survey visit
    gargoyle::on("save_template", {

      # close current
      bslib::accordion_panel_close(
        id = "setup",
        value = "survey_instrument_panel"
      )

      # open next
      bslib::accordion_panel_open(
        id = "setup",
        value = "survey_visit_panel"
      )

    })

    # from visit to save settings
    gargoyle::on("save_visit", {

      # close current
      bslib::accordion_panel_close(
        id = "setup",
        value = "survey_visit_panel"
      )

      # open next
      bslib::accordion_panel_open(
        id = "setup",
        value = "save_settings_panel"
      )

    })

    # from save settings to data
    gargoyle::on("save_settings", {

      # close save settings
      bslib::accordion_panel_close(
        id = "setup",
        value = "save_settings_panel"
      )

      # move focus to the data tab
      # NOTE: found in app_server.R

    })


  })
}

## To be copied in the UI
# mod_1_setup_ui("1_setup_1")

## To be copied in the server
# mod_1_setup_server("1_setup_1")
