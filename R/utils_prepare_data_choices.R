#' Make data choices
#'
#' @description
#' Construct choices of the data set to select
#'
#' @param dir Character. Path to the directory where data are stored.
#'
#' @return Character vector. Names of data files, without file extension.
#'
#' @importFrom fs dir_ls path_file path_ext_remove
#' @importFrom purrr discard
make_data_choices <- function(dir) {

  data_choices <- dir |>
    # create a vector of paths to matching data
    fs::dir_ls(
      type = "file",
      regexp = "\\.dta",
      recurse = FALSE
    ) |>
    # prepare data options for presentation
    fs::path_file() |>
    fs::path_ext_remove() |>
    # remove system-generated data files from the vector
    purrr::discard(
      .p = ~ .x %in% c(
        "assignment__actions",
        "interview__actions",
        "interview__diagnostics",
        "interview__errors",
        "interview__comments"
      )
    )

  return(data_choices)

}

#' Convert export variable names into their original Designer names
#'
#' @description
#' When data are exported, variables that capture several pieces of information
#' are split into one variable for each piece of information, where each
#' resulting variable is composed of the core variable name, as it appears
#' in Designer, plus a suffix that indicates its index or its component.
#'
#' This function converts a vector of variable names from an export file into
#' a vector of variable names as they would appear in Designer and, importantly,
#' in the questionnaire JSON file.
#'
#' @param vars Character vector. Names of variables drawn from export file.
#'
#' @return Character vector of Designer variable names.
#'
#' @importFrom glue glue_collapse
#' @importFrom stringr str_replace_all
convert_vars_from_export_to_designer <- function(vars) {

  # construct the regular expression for selecting variable suffixes
  var_suffix_regex <-  glue::glue_collapse(
    x = c(
      # starts with double underscore
      "__",
      # followed by one of the following
      "(",
      # numbers or n[0-9]+, if the number is negative
      "[n]*[0-9]+|",
      # components of GPS questions
      "Longitude|_Latitude|Accuracy|Altitude|Timestamp",
      ")",
      # ends with this suffix
      "$",
      sep = ""
    )
  )

  designer_vars <- vars |>
    # remove suffix
    stringr::str_replace_all(
      pattern = var_suffix_regex,
      replacement = ""
    ) |>
    # keep only unique entries in the vector
    # that is, drop duplicates created by removing suffixes of variables
    # that are mutli-part in export
    base::unique()

  return(designer_vars)

}

#' Construct data variable choices
#'
#' @param data_path Character. Path to the export data file.
#' @param vars_df Data frame. Variable metadata data frame.
#' @param var_type Atomic character vector. Type(s) of variable to select.
#' One of: "categorical", "numeric", "gps", "all"
#'
#' @return Character vector. Entries are of the form:
#' `{varname} : {variable_description}`
#'
#' @importFrom dplyr case_when select left_join filter pull
#' @importFrom haven read_dta
#' @importFrom tibble enframe
make_data_var_choices <- function(
  data_path,
  vars_df,
  var_type = c("categorical", "numeric", "gps", "all")
) {

  # translate user-facing variable types into JSON-friendly types
  if (var_type == "categorical") {
    var_type_json <- c("MultyOptionsQuestion", "SingleQuestion")
  } else if (var_type == "numeric") {
    var_type_json <- "NumericQuestion" 
  } else if (var_type == "numeric") {
     "NumericQuestion"
  } else if (var_type == "gps") {
    var_type_json <- "GpsCoordinateQuestion"
  } else if (var_type == "all") {
    var_type_json <- c(
      "SingleQuestion",
      "MultyOptionsQuestion",
      "NumericQuestion",
      "GpsCoordinateQuestion"
    )
  }

  data_var_choices <- data_path |>
    # read variable names from the file
    haven::read_dta(n_max = 0) |>
    names() |>
    # get a vector of Designer-friendly variable names
    convert_vars_from_export_to_designer() |>
    # construct a variable name data frame for joining with variable metadata
    tibble::enframe() |>
    dplyr::select(varname = value) |>
    # join in metadata on variable type and other attributes
    dplyr::left_join(
      y = vars_df,
      by = "varname"
    ) |>
    # exclude linked questions
    dplyr::filter(is_linked == FALSE) |>
    # UNTIL SUSOMETA READS REUSABLE CATEGORIES...
    # ... drop questions that use categorical answers
    dplyr::filter(uses_reusable_categories == FALSE) |>
    # select only desired variable types
    dplyr::filter(type %in% var_type_json) |>
    # prepare option text for display
    dplyr::mutate(
      # santize variable description,
      # removing HTML and replacing roster title
      variable_description = sanitize_suso_text(variable_description),
      # compose the variable display text as
      # `{varname} : {variable_description}`
      to_display = paste(variable_description,
        paste0("{", varname, "}"), sep = " : "
      )
    ) |>
    dplyr::pull(to_display)

}

#' Extract variable names from variable choices
#'
#' @param vars Character vector. Variable choices of the following form:
#' `{varname} : {variable_description}`
#'
#' @return Character vector. Variable names contained in variable choices.
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_extract
extract_var_names <- function(vars) {

  vars_extracted <- purrr::map_chr(
    .x = vars,
    .f = ~ stringr::str_extract(
      string = .x,
      pattern = "(?<=\\{).+(?=\\})"
    )
  )

  return(vars_extracted)

}

#' Construct variable value options for selection in UI
#'
#' @param qnr_df Data frame. Of the form returned by
#' `susometa::parse_questionnaire()`.
#' @param varname Character. Name of the target variable.
#'
#' @return Named numeric vector. Values are codes. Names are value labels.
#'
#' @importFrom rlang sym
#' @importFrom susometa get_answer_options
#' @importFrom glue glue
make_val_options <- function(
  qnr_df,
  varname
) {

  options <- susometa::get_answer_options(
    qnr_df = qnr_df,
    varname = !!rlang::data_sym(varname)
  )

  options_as_chars <- glue::glue("[{unname(options)}] {names(options)}")

  return(options_as_chars)

}