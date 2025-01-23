#' Construct user app data directory path
#'
#' @description
#' Consists of three pieces of information:
#'
#' - System location
#' - App name
#' - App version
#'
#' @importFrom rappdirs user_data_dir
#' @importFrom desc desc_get
#'
#' @return Character. Path to user's app data directory
#'
#' @noRd
construct_user_data_path <- function() {

  # given that app_sys() will behave differently based on
  # wether or not we are in dev mode, we can use `{desc}`
  # and find the correct DESCRIPTION here, using the
  # internal app_sys()
  app_description_path <- app_sys("DESCRIPTION")

  dir <- rappdirs::user_data_dir(
    appname = desc::desc_get(
      "Package",
      file = app_description_path
    ),
    appauthor = NULL,
    version = desc::desc_get(
      "Version",
      file = app_description_path
    )
  )

  return(dir)
}

#' Create user application directory for persistent storage
#'
#' @return Character. Path where app directory was created/exists
#'
#' @importFrom fs dir_exists dir_create
#' @importFrom glue glue
#'
#' @noRd
create_user_app_dir <- function() {

  # create user app data directory
  # determine where it should be
  app_dir <- construct_user_data_path()

  # create directory if it doesn't exist
  if (!fs::dir_exists(app_dir)) {
    fs::dir_create(app_dir)
  }

  # confirm that user app data directory exists
  app_dir_exists <- fs::dir_exists(app_dir)
  if (app_dir_exists == TRUE) {
    cat(glue::glue("App directory created at {app_dir}"))
    return(app_dir)
  } else {
    stop("App data directory could not be created")
  }
}

#' Enable navbar element
#' 
#' @param id Character. Shiny id given to navbar element.
#' 
#' @return Side-effect of enabling target navbar element.
#' 
#' @importFrom shinyjs enable
#' @importFrom glue glue
#'
#' @noRd
enable_navbar_element <- function(id) {

  shinyjs::enable(selector = glue::glue("a[data-value='{id}']"))

}

#' Disable navbar element
#' 
#' @param id Character. Shiny id given to navbar element.
#' 
#' @return Side-effect of disabling target navbar element.
#' 
#' @importFrom shinyjs enable
#' @importFrom glue glue
#'
#' @noRd
disable_navbar_element <- function(id) {

  shinyjs::disable(selector = glue::glue("a[data-value='{id}']"))

}

#' Create team options for UI
#'
#' @description
#' Construct team UI choices from team composition file saved on disk.
#'
#' @param path Character. Path to team composition Stata file.
#'
#' @return Character vector of team choices for UI
#'
#' @importFrom haven read_dta
#' @importFrom dplyr distinct pull
#'
#' @noRd
create_team_choices <- function(
  path,
  all_teams_txt = "All teams"
) {

  teams_from_df <- path |>
    haven::read_dta() |>
    # data frame of supervisors and their interviewers
    # reduced to distinct supervisors (aka teams)
    dplyr::distinct(SupervisorName) |>
    dplyr::pull(SupervisorName)

  # pre-pend "All teams" option
  teams <- c(all_teams_txt, teams_from_df)

  return(teams)

}

#' Perform Quarto report rendering workflow
#'
#' @description
#' Workflow:
#' - Determine which report template to use
#' - Copy that template, ana associated resources, from the package to the app
#' - Render the report where the template and resources have been copied
#' - Move the rendered report to a user-facing folder
#'
#' @param report_type Character. One of: completeness, quality
#' @param proj_dir Character. Path to the root of the app directory
#' @param params List. Parameters for Quarto report.
#'
#' @importFrom glue glue
#' @importFrom dplyr case_when
#' @importFrom fs path path_package file_copy file_move
#' @importFrom quarto quarto_render
#'
#' @return Side-effect of producing a rendered report in a certain directory
render_report <- function(
  report_type,
  proj_dir,
  params
) {

  # ============================================================================
  # set paths as a function of project path and report type
  # ============================================================================

  # ----------------------------------------------------------------------------
  # app paths
  # ----------------------------------------------------------------------------

  report_name <- glue::glue("report_{report_type}.qmd")

  report_dir <- dplyr::case_when(
    report_type == "completeness" ~ "01_completeness",
    report_type == "quality" ~ "02_quality",
    TRUE ~ ""
  )

  # top-level report-specific directory
  report_app_dir <- fs::path(
    proj_dir, "02_reports", report_dir
  )

  # where template should be copied
  template_app_path <- fs::path(
    report_app_dir, "resources", report_name
  )

  # ----------------------------------------------------------------------------
  # package paths
  # ----------------------------------------------------------------------------

  pkg_path <- fs::path_package(
    package = "monitor50x30",
    "quarto", "templates"
  )

  template_pkg_path <- fs::path(
    pkg_path, glue::glue("{report_type}"), report_name
  )

  # ============================================================================
  # copy resources from package to app
  # ============================================================================

  fs::file_copy(
    path = template_pkg_path,
    new_path = template_app_path,
    overwrite = TRUE
  )

  # TODO: add files/revise approach as needed

  # ============================================================================
  # render report in situ
  # ============================================================================

  quarto::quarto_render(
    input = template_app_path,
    execute_params = params
  )

  # ============================================================================
  # move report to user-facing report dir
  # ============================================================================

  fs::file_move(
    path = fs::path(
      report_app_dir, "resources", glue::glue("report_{report_type}.html")
    ),
    new_path = fs::path(report_app_dir, glue::glue("report_{report_type}.html"))
  )

}
