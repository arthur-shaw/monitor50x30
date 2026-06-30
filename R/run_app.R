#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom quarto quarto_path
#' @importFrom cli cli_abort cli_inform
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  # check for system dependencies
  # Quarto
  if (is.null(quarto::quarto_path())) {
    cli::cli_abort(
      message = c(
        "x" = "Quarto cannot be found. Please install it.",
        "i" = "To create reports, this application needs Quarto installed.",
        "!" = paste(
          "To install it, follow the instructions here:",
          "https://quarto.org/docs/get-started/"
        )
      )
    )
  } else {
    cli::cli_inform(
      message = c(
        "v" = "Quarto found."
      )
    )
  }

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
