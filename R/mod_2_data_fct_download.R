#' Create folder system for data
#'
#' @description
#' Creates folder system for data storage, if folders do not already exist.
#'
#' @return List of paths to created data directories:
#' - `microdata_dir`. Root directory of microdata data storage.
#' - `micro_dl_dir`. Where data are downloaded.
#' - `micro_combine_dir`. Where combined data are stored.
#' - `sync_dir`. Where tablet sync data are stored.
#' - `team_dir`. Where team compisition data frame is stored.
#' - `qnr_dir`. Where questionnaire JSON and its derivatives are stored.
#'
#' @param app_dir Character. Application directory where params stored.
#'
#' @importFrom fs dir_create path file_exists
#'
#' @noRd
create_data_dirs <- function(app_dir) {

  # microdata
  microdata_dir <- fs::path(app_dir, "01_interview_microdata")
  micro_dirs <- c(
    microdata_dir,
    fs::path(microdata_dir, "01_downloaded/"),
    fs::path(microdata_dir, "02_combined/")
  )
  fs::dir_create(path = micro_dirs)

  # sync data
  sync_dir <- fs::path(app_dir, "02_suso_sync_dates")
  fs::dir_create(sync_dir)

  # team composition
  team_dir <- fs::path(app_dir, "03_team_composition")
  fs::dir_create(team_dir)

  # questionnaire metadata
  qnr_dir <- fs::path(app_dir, "04_qnr_metadata")
  fs::dir_create(qnr_dir)

  # collect paths to created directories
  dirs <- list(
    microdata_dir = micro_dirs[1],
    micro_dl_dir = micro_dirs[2],
    micro_combine_dir = micro_dirs[3],
    sync_dir = sync_dir,
    team_dir = team_dir,
    qnr_dir = qnr_dir
  )

  return(dirs)

}

#' Delete all stale data files inside target directory
#'
#' Delete files, directories, and files in those directories.
#'
#' @param dir Character. Path to target directory
#'
#' @return Side-effect of deleting stale files.
#'
#' @importFrom fs dir_exists dir_ls dir_delete file_delete
#'
#' @noRd
delete_stale_data <- function(dir) {

  # if folder exists, purge its contents
  if (fs::dir_exists(dir)) {

    # list directories
    directories <- fs::dir_ls(
      path = dir,
      type = "directory",
      recurse = FALSE
    )

    # remove directories, if they exist
    fs::dir_delete(directories)

    # remove all files
    files <- fs::dir_ls(
      path = dir,
      type = "file",
      all = TRUE
    )
    fs::file_delete(files)

  } else {
    print(
      paste0(
        "Diectory not deleted.",
        paste0(dir, " does not exist"),
        collapse = "\n"
      )
    )
  }

}

#' Unpack single zip file to a folder bearing its name
#'
#' Rather than unpack a file to the directory in which the file sits,
#' create a folder with the file's name (minus extension) and
#' unpack its contents there.
#'
#' @param zipfile Character. Full file path of the zip file.
#'
#' @return Side-effect of creating a folder and unpacking zip contents there.
#'
#' @importFrom fs path_dir path_file path_ext_remove
#' @importFrom zip unzip
#'
#' @noRd
unpack_zip_to_dir <- function(zipfile) {

  parent_dir <- fs::path_dir(zipfile)
  file_name <- fs::path_file(zipfile)
  unpack_name <- fs::path_ext_remove(file_name)
  unpack_dir <- fs::path(parent_dir, unpack_name)

  zip::unzip(
    zipfile = zipfile,
    exdir = unpack_dir
  )

}

#' Unpack all zip files found in directory to same-named sub-directory
#'
#' Applies `unpack_zip_to_dir()` to all zip files in directory
#'
#' @param dir Character. Directory where zip files can be found
#'
#' @return Side-effect of creating a folder for each zip and
#' unpacking its contents there.
#'
#' @importFrom fs dir_ls
#' @importFrom purrr walk
#'
#' @noRd
unpack_all_zip_to_dir <- function(dir) {

  # obtain list of zip files
  files <- fs::dir_ls(
    path = dir,
    type = "file",
    regexp = "\\.zip$",
    recurse = FALSE
  )

  # unpack all identified zip files
  purrr::walk(
    .x = files,
    .f = ~ unpack_zip_to_dir(.x)
  )

}

#' Combine and save Stata data files
#'
#' @param file_info_df Data frame. Return value of `fs::file_info()` that
#' contains an additioal column `file_name`.
#' @param name Character. Name of the file (with extension) to ingest from
#' all folders where it is found.
#' @param dir Character. Directory where combined data will be saved.
#'
#' @return Side-effect of creating data frame objects in the global environment
#' with the name `name`.
#'
#' @importFrom rlang `.data`
#' @importFrom dplyr filter pull
#' @importFrom purrr map_dfr
#' @importFrom haven read_dta
#' @importFrom fs path_ext_remove path
#'
#' @noRd
combine_and_save_dta <- function(
  file_info_df,
  name,
  dir
) {

  # file paths
  # so that can locate data files to combine
  file_paths <- file_info_df |>
    dplyr::filter(.data$file_name == name) |>
    dplyr::pull(.data$path)

  # data frame
  # so that can assign this value to a name
  df <- purrr::map_dfr(
    .x = file_paths,
    .f = ~ haven::read_dta(file = .x)
  )

  # save to destination directory
  haven::write_dta(data = df, path = fs::path(dir, name))

}

#' Combine and save all downloaded `.dta` files
#'
#' Compile list of `dta` files. Combine all files of same name
#'
#' @param dir_in Character. Root directory whose sub-directories
#' contain `.dta` files
#' @param dir_out Chracter. Directory where
#'
#' @return Side-effect of combining each
#'
#' @importFrom fs dir_ls path_file
#' @importFrom purrr map_dfr walk
#' @importFrom dplyr mutate distinct pull
#' @importFrom rlang `.data`
#'
#' @noRd
combine_and_save_all_dta <- function(
  dir_in,
  dir_out
) {

  # obtain list of all directories of unpacked zip files
  dirs <- fs::dir_ls(
    path = dir_in,
    type = "directory",
    recurse = FALSE
  )

  # compile list of all Stata files in all directories
  files_df <- dirs |>
    purrr::map_dfr(
      .f = ~ fs::dir_info(
        path = .x,
        recurse = FALSE,
        type = "file",
        regexp = "\\.dta$"
      )
    ) |>
    dplyr::mutate(file_name = fs::path_file(.data$path))

  # extract a list of all unique files found in the directories
  file_names <- files_df |>
    dplyr::distinct(.data$file_name) |>
    dplyr::pull(.data$file_name)

  # combine and save all same-named Stata files
  purrr::walk(
    .x = file_names,
    .f = ~ combine_and_save_dta(
      file_info_df = files_df,
      name = .x,
      dir = dir_out
    )
  )

}

#' Get JSON representation of the questionnaire
#'
#' Performs these steps:
#'
#' - Rename previously downloaded JSON file, if any
#' - Download new JSON file, if needed
#'
#' @param qnr_id Character. SuSo questionnaire GUID.
#' @param qnr_version Numeric. SuSo questionnaire verion number.
#'
#' @importFrom fs path file_exists file_move
#' @importFrom susoapi get_questionnaire_document
#'
#' @return Side-effect of renaming old file and downloading new
#'
#' @noRd
get_qnr_json <- function(
  qnr_id,
  qnr_version,
  qnr_dir,
  server,
  workspace,
  user,
  password
) {

  # questionnaire file paths
  qnr_file <- fs::path(qnr_dir, "document.json")
  old_qnr_file <- fs::path(qnr_dir, "old_document.json")

  # rename old file, if it exists
  if (fs::file_exists(qnr_file)) {
    fs::file_move(
      path = qnr_file,
      new_path = old_qnr_file
    )
  }

  # download (new) file
  susoapi::get_questionnaire_document(
    qnr_id = qnr_id,
    qnr_version = qnr_version,
    path = qnr_dir,
    server = server,
    workspace = workspace,
    user = user,
    password = password
  )

}

#' Decide whether to parse questionnaire JSON file
#'
#' @param qnr_dir Character. Directory where questionnaire file is located
#'
#' @return Boolean. `TRUE`` if JSON files should be parsed; `FALSE` otherwise.
#'
#' @importFrom fs file_exists
#'
#' @noRd
decide_whether_to_parse_json <- function(qnr_dir) {

  # questionnaire file paths
  qnr_file <- paste0(qnr_dir, "document.json")
  old_qnr_file <- paste0(qnr_dir, "old_document.json")

  # check conditions for parsing downloaded JSON file

  # case 1: no old file exists
  old_file_exists <- fs::file_exists(old_qnr_file)

  # case 2: new file has different md5sum than old one
  md5sum_old_file <- tools::md5sum(old_qnr_file)
  md5sum_new_file <- tools::md5sum(qnr_file)
  new_file_same <- md5sum_old_file == md5sum_new_file

  parse_file <- (old_file_exists == FALSE | new_file_same == FALSE)

  return(parse_file)

}

#' Create data frame of variables
#'
#' @param df Data frame returned by `suspara::parse_questionnaire()`
#'
#' @return Data frame of variables. Columns will be:
#' - `name`. Variable name in SuSo Designer
#' - `description`. Variable description.
#' Details for the `description`` column.
#' If the label is available, use this 80-character descriptor.
#' Otherwise, use the much longer question text
#'
#' @importFrom dplyr filter select mutate if_else
#' @importFrom rlang `.data`
#'
#' @noRd
extract_vars_metadata <- function(df) {

  # create data frame of variables
  vars_df <- df  |>
    # filter to objects that are variables
    # namely, entities that have a `question_type`
    dplyr::filter(!is.na(.data$question_type)) |>
    # select the variable name, variable label, and question text
    dplyr::select(
      .data$varname, .data$variable_label, .data$question_text, .data$type
    ) |>
    # create question description that is preferably the label, but question
    # text if the label is empty
    dplyr::mutate(
      variable_description = dplyr::if_else(
        condition = (is.na(.data$variable_label) | .data$variable_label == ""),
        true = .data$question_text,
        false = .data$variable_label,
        missing = .data$variable_label
      )
    )|>
    dplyr::select(
      .data$varname, .data$variable_label, .data$variable_description,
      .data$type
    )

  return(vars_df)

}

#' Get last sync date by user
#'
#' Performs this sequence of actions:
#' - Get action logs.
#' - Compute last sync date by user.
#' - Save raw logs and computed sync files as labelled Stata files
#'
#' @param start_date Character. Start date for action logs using
#' ISO 8601 format.
#' @param end_date Character. End date for action logs using ISO 8601 format.
#' @param dir Character. Directory where output files should be saved.
#' @inheritParams susoflows::download_matching
#'
#' @return Side-effect of saving
#'
#' @importFrom susoflows get_all_user_logs calc_last_user_sync
#' @importFrom labelled var_label
#' @importFrom haven write_dta
#' @importFrom fs path
#'
#' @noRd
get_sync_data <- function(
  start_date,
  end_date = as.character(Sys.Date()),
  dir,
  server,
  workspace,
  user,
  password
) {

  # get tablet action logs
  action_logs <- susoflows::get_all_user_logs(
    start = start_date,
    end = end_date,
    server = server,
    workspace = workspace,
    user = user,
    password = password
  )

  # compute last sync date from action logs
  last_sync_date_by_user <- susoflows::calc_last_user_sync(action_logs)

  # save to disk
  # ... action logs
  labelled::var_label(action_logs) <- list(
    Time = "Timestamp",
    Message = "Action description",
    UserId = "GUID of user who took the action"
  )
  haven::write_dta(
    data = action_logs,
    path = fs::path(dir, "action_logs.dta")
  )

  # ... last sync date
  labelled::var_label(last_sync_date_by_user) <- list(
    last_sync_date = "Last date user's device synchronized",
    UserId = "GUID of user"
  )
  haven::write_dta(
    data = last_sync_date_by_user,
    path = fs::path(dir, "last_sync_date_by_user.dta")
  )

}

#' Get team composition
#'
#' First, fetch composition. Then, write to labelled Stata file.
#'
#' @param dir Character. Directory where data should be stored.
#' @inheritParams susoflows::download_matching
#'
#' @importFrom susoapi get_interviewers
#' @importFrom labelled var_label
#' @importFrom haven write_dta
#' @importFrom fs path
#'
#' @noRd
get_team_composition <- function(
  dir,
  server,
  workspace,
  user,
  password
) {

  # construct team composition
  team_composition <- susoapi::get_interviewers(
    server = server,
    workspace = workspace,
    user = user,
    password = password
  )

  # label columns for easier comprehension
  labelled::var_label(team_composition) <- list(
    UserId = "Interviewer GUID",
    UserName = "Interviewer user name",
    SupervisorId = "Supervisor user GUID",
    SupervisorName = "Supervisor user name",
    Role = "Role: Interviewer, Supervisor"
  )

  # write to disk
  haven::write_dta(
    data = team_composition,
    path = fs::path(dir, "team_composition.dta")
  )

}

##
#' Check if the combined data exists
#'
#'
#' @param microdata_path The path of the combined microdata. Obtain it using create_data_dirs(r6$app_dir)$micro_combine_dir
#'
#' @importFrom fs path
#'
#' @noRd
combined_data_exists = function(microdata_path){

  if(fs::file_exists(microdata_path)){
    TRUE
  }else{
    FALSE
  }
}

#' Get last date when data was downloaded
#'
#'
#' @param microdata_path The path of the combined microdata. Obtain it using create_data_dirs(r6$app_dir)$micro_combine_dir
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_glue
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom fs path
#'
#' @noRd
get_last_data_download_date <- function(microdata_path){

  if(combined_data_exists(microdata_path)){

  last_data_download_date <- fs::dir_info(
    path = microdata_path,
    type = "file",
    recurse = TRUE
  ) %>%
    dplyr::distinct(modification_time) %>%
    dplyr::pull()



  last_data_download_date_raw <- max(last_data_download_date)
  last_data_download_date_formatted <- stringr::str_glue("{format(as.Date(last_data_download_date_raw),
                                            format = '%A, %B %d %Y')}{' at '}{format(as.POSIXct(last_data_download_date_raw),
                                            format = '%H:%M:%S %Z')}")
  }else{
    last_data_download_date_raw = NULL
    last_data_download_date_formatted = NULL
}

  to_return = list(last_data_download_date_raw, last_data_download_date_formatted)
  return(to_return)
}






