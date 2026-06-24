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
#' One of: "categorical", "multi-select", "single-select", "numeric", "gps",
#' "all".
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
  var_type = c(
    "categorical", "multi-select", "single-select",
    "numeric", "gps",
    "all"
  )
) {

  # translate user-facing variable types into JSON-friendly types
  if (var_type == "categorical") {
    var_type_json <- c("MultyOptionsQuestion", "SingleQuestion")
    calc_var_type_json <- c(
      1, # Long Integer
      2, # Double
      3 # Boolean
    )
  } else if (var_type == "multi-select") {
    var_type_json <- "MultyOptionsQuestion"
    calc_var_type_json <- NA
  } else if (var_type == "single-select") {
    var_type_json <- "SingleQuestion"
    calc_var_type_json <- c(
      1, # Long Integer
      2, # Double
      3 # Boolean
    )
  } else if (var_type == "numeric") {
    var_type_json <- "NumericQuestion"
    calc_var_type_json <- c(
      1, # Long Integer
      2, # Double
      3 # Boolean
    )
  } else if (var_type == "gps") {
    var_type_json <- "GpsCoordinateQuestion"
    calc_var_type_json <- NA
  } else if (var_type == "all") {
    var_type_json <- c(
      "SingleQuestion",
      "MultyOptionsQuestion",
      "NumericQuestion",
      "GpsCoordinateQuestion"
    )
    calc_var_type_json <- c(
      1, # Long Integer
      2, # Double
      3, # Boolean
      4, # Date/Time
      5 # String
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
    # select only desired variable types
    dplyr::filter(
      # questions
      type %in% var_type_json |
      # computed variables
      (all(!is.na(calc_var_type_json)) & (type_variable %in% calc_var_type_json))
    ) |>
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

#' Make variable ID choices
#'
#' @param path Atomic character. Path to target data file.
#'
#' @return Character vector. Entries are of the form:
#' `{varname} : {variable_description}`
#'
#' @importFrom haven read_dta
#' @importFrom labelled look_for
#' @importFrom dplyr mutate pull
#' @importFrom glue glue
make_id_var_choices <- function(path) {

  id_var_choices <- path |>
    haven::read_dta() |>
    labelled::look_for(
      "__id",
      labels = TRUE,
      values = FALSE
    ) |>
    dplyr::mutate(choices = glue::glue("{variable} : {label}")) |>
    dplyr::pull(choices)

  return(id_var_choices)

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

#' Extract varname names from ID variable choices
#'
#' @param vars Character vector. Variable choices of the following form:
#' `{varname} : {variable_description}`
#'
#' @return Character vector. Contains only variable names.
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_extract
extract_id_var_names <- function(vars) {

  vars_extracted <- purrr::map_chr(
    .x = vars,
    .f = ~ stringr::str_extract(
      string = .x,
      pattern = "^.+(?= :)"
    )
  )

  return(vars_extracted)

}

#' Construct variable value options for selection in UI
#'
#' @param json_path Character. Path to the questionnaire JSON.
#' @param categories_dir Character. Path to the directory containing
#' reusable categories.
#' @param varname Character. Name of the target variable.
#'
#' @return Named numeric vector. Values are codes. Names are value labels.
#'
#' @importFrom rlang sym
#' @importFrom susometa get_answer_options
#' @importFrom glue glue
make_val_options <- function(
  json_path,
  categories_dir,
  varname
) {

  options <- tryCatch(
    # case 1: answers available; extract them
    expr = {

      # extract answer options
      susometa::get_answer_options(
        json_path = json_path,
        categories_dir = categories_dir,
        varname = !!rlang::sym(varname)
      )

    },
    # case 2: no answers available; return `NULL`
    error = function(cnd) {

      NULL

    }

  )

  # transform answer options into structured choices
  # if answer options found, a character vector of structured choices
  # if answer options not found, return an emtpy character vector
  # NOTE: this doesn't work with base's `ifelse()`
  if (length(options) >= 1) {
    options_as_chars <- glue::glue("[{unname(options)}] {names(options)}")
  } else {
    options_as_chars <- character(0)
  }

  return(options_as_chars)

}

#' Make value choices for ID variables
#' 
#' @param dir Character. Path to the directory where data are stored.
#' @param varname Character. Name of the target variable in the exported data.
#' (e.g., `crop__id`)
#'
#' @return Character vector. Variable values this form:
#' `[{value}] Value label text`
#'
#' @importFrom haven read_dta
#' @importFrom labelled get_value_labels
#' @importFrom purrr pluck
#' @importFrom tibble enframe
#' @importFrom dplyr mutate pull
#' @importFrom glue glue
make_id_val_options <- function(
  path,
  varname
) {

  val_lbls <- path |>
    # ingest data
    # selecting target column and
    # keeping only header row
    haven::read_dta(
      col_select = !!rlang::sym(varname),
      n_max = 0
    ) |>
    # extract the value labels in the data frame
    labelled::get_value_labels() |>
    # since 👆 returns a list of labelled vectors, select the 1st list element
    # which is also the only element
    purrr::pluck(1)

  # if the labells are null, pass an empty character vector
  if (is.null(val_lbls)) {

    id_val_options <- character(0)

  # otherwise, construct options as a structured character vector
  } else {

    id_val_options <- val_lbls |>
      tibble::enframe() |>
      dplyr::mutate(choices = glue::glue("[{value}] : {name}")) |>
      dplyr::pull(choices)

  }

  return(id_val_options)

}

#' Extract variable values from selections
#'
#' @param vals Character vector. Variable value selections of this form:
#' `[{value}] Value label text`
#'
#' @return Numeric vector. Values extracted as numbers.
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_extract
extract_var_values <- function(vals) {

  vals_extracted <- vals |>
    purrr::map_chr(
      .f = ~ stringr::str_extract(
        string = .x,
        pattern = "(?<=\\[)[-]*[0-9]+(?=\\] )"
      )
    ) |>
    as.numeric()

  return(vals_extracted)

}
