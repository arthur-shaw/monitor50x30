#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#

library(shiny.i18n)

# Folder that contains csv translation files
i18n <- Translator$new(translation_csvs_path = "translations_data")

# Default language of the app
i18n$set_translation_language("fr")

app_ui <- function(request) {
  shiny::tagList(

    shiny.i18n::usei18n(i18n),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "monitor50x30",
      id = "navbar",
      underline = TRUE,
      theme = bslib::bs_theme(version = 5),
      bslib::nav_panel(
        title = "About",
        value = "about",
        mod_0_about_ui("0_about_1")
      ),
      bslib::nav_panel(
        title = "Setup",
        value = "setup",
        mod_1_setup_ui("1_setup_1")
      ),
      bslib::nav_panel(
        title = "Data",
        value = "data",
        mod_2_data_ui("2_data_1")
      ),
      bslib::nav_panel(
        title = "Completeness",
        value = "completeness",
        mod_3_complete_ui("3_complete_1")
      ),
      bslib::nav_panel(
        title = "Quality",
        value = "quality",
        mod_4_quality_ui("4_quality_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "monitor50x30"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    waiter::use_waiter(),
    shiny.i18n::usei18n(i18n),
  )
}
