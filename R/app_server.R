#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # create application directory
  app_dir <- create_user_app_dir()

  # create file system for storing data, reports, and other app artifacts
  dirs <- create_app_file_system(app_dir = app_dir)

  # initialize R6 object
  # setting directories so that r6's read and write methods have a target path
  r6 <- r6$new()
  r6$app_dir <- app_dir
  r6$dirs <- dirs

  # handle R6 object as a function of (past) app state
  # case 1: app never used => ; past R6 not found as RDS in local storage
  # write new R6 to local storage
  settings_file_exists <- fs::file_exists(fs::path(app_dir, "saved_params.rds"))
  if (!settings_file_exists) {
    r6$write()
  # case 2: app used => RDS exists in app's user data folder
  # restore R6 from past session by reading its values from RDS to R6
  } else {
    r6$read()
  }

  # initialize gargoyle listeners
  # setup
  gargoyle::init("load_setup")
  gargoyle::init("need_setup")
  gargoyle::init("save_creds")
  gargoyle::init("open_suso_qnr")
  gargoyle::init("save_suso_qnr_identify")
  gargoyle::init("save_suso_qnr_select")
  gargoyle::init("save_suso_qnr")
  gargoyle::init("save_template")
  gargoyle::init("save_visit")
  gargoyle::init("save_settings")
  # data
  gargoyle::init("download_data")
  gargoyle::init("select_action")
  # completeness
  gargoyle::init("save_domains")
  gargoyle::init("save_quantify_clusters")
  gargoyle::init("save_computer_identify_clusters")
  gargoyle::init("save_manager_select_clusters")
  gargoyle::init("save_manager_order_clusters")
  gargoyle::init("save_manager_compose_clusters")
  gargoyle::init("save_clusters")
  gargoyle::init("save_workloads")
  gargoyle::init("save_completeness_setup")

  # load module server logic
  mod_1_setup_server("1_setup_1", r6 = r6)
  mod_2_data_server("2_data_1", r6 = r6)
  mod_3_complete_server("3_complete_1", r6 = r6)
  mod_4_quality_server("4_quality_1", r6 = r6)

  # ============================================================================
  # initialize UI
  # ============================================================================

  # select about or setup tab as a function of whether SuSo creds provided
  # needed a way to determine whether any settings had been set
  # SuSo credentials are the first
  # also, there seems to be some race condition that prevents making this
  # determination based on the existence or not of an rds file
  if (is.null(r6$suso_creds_provided)) {

    bslib::nav_select(
      id = "navbar",
      selected = "about"
    )

  } else if (!is.null(r6$suso_creds_provided)) {

    if (r6$suso_creds_provided == TRUE) {

      bslib::nav_select(
        id = "navbar",
        selected = "setup"
      )

    }

  }

  # data download tab
  if (is.null(r6$core_settings_saved)) {

    disable_navbar_element(id = "data")

  } else if (!is.null(r6$core_settings_saved)) {

    if (r6$core_settings_saved == TRUE) {

      enable_navbar_element(id = "data")

    }

  }

  # data-dependent tabs
  if (is.null(r6$data_downloaded)) {

    disable_navbar_element(id = "completeness")
    disable_navbar_element(id = "quality")

  } else if (!is.null(r6$data_downloaded)) {

    if (r6$data_downloaded == TRUE) {

      enable_navbar_element(id = "data")
      enable_navbar_element(id = "completeness")
      enable_navbar_element(id = "quality")

    }

  }


  # ============================================================================
  # react to completing settings
  # ============================================================================

  # move between modules
  # from settings to data
  gargoyle::on("save_settings", {

    bslib::nav_select(
      id = "navbar",
      selected = "data"
    )

  })

  # ============================================================================
  # react to getting data
  # ============================================================================

  # enable data-driven tabs
  gargoyle::on("download_data", {

    enable_navbar_element(id = "completeness")
    enable_navbar_element(id = "quality")

  })

  # ============================================================================
  # react to selecting an action after getting data
  # ============================================================================

  gargoyle::on("select_action", {

    if (r6$selected_action == "Data completeness report") {

      bslib::nav_select(
        id = "navbar",
        selected = "completeness"
      )

    } else if (r6$selected_action == "Data quality report") {

      bslib::nav_select(
        id = "navbar",
        selected = "quality"
      )

    }

  })

}
