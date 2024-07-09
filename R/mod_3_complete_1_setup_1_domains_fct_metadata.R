#' Make display variable options
#'
#' @description
#' Compose domain as `{varname} : {variable_description}`
#'
#' @param path Character. Path to questionnaire variables file (qnr_vars.rds).
#' @param var_types Character. Types of Survey Solutions variable to include.
#' Use types from SuSo JSON file.
#'
#' @return Character vector. Set of variable descriptions
#'
#' @importFrom dplyr filter mutate pull
#' @importFrom stringr str_remove_all str_replace_all
make_vars_options <- function(
  path,
  var_types
) {

  # compose list of allowed colors
  suso_colors <- c(
    "red",
    "blue",
    "green",
    "black",
    "gray",
    "cyan",
    "magenta",
    "yellow",
    "lightgray",
    "darkgray",
    "grey",
    "lightgrey",
    "darkgrey",
    "aqua",
    "fuchsia",
    "lime",
    "maroon",
    "navy",
    "olive",
    "purple",
    "silver",
    "teal",
    "brown"
  )
  # convert into regex expression of colors delimited by the or operator
  font_colors_expr <- glue::glue(
    "({glue::glue_collapse(x = suso_colors, sep = '|')})"
  )

  q_vars_out <-
    # load data frame of questionnaire metadata
    readRDS(file = path) |>
    # exclude any questionnaire objects that do not have `variable_description`
    dplyr::filter(!is.na(variable_description) & variable_description != "") |>
    # select questions that are single-select
    dplyr::filter(type %in% var_types) |>
    # remove HTML tags from variable description
    dplyr::mutate(
      # remove font tag
      variable_description = stringr::str_remove_all(
        string = variable_description,
        pattern = paste0('<font color[ ]*=[ ]*\"', font_colors_expr, '\">')
      ),
      variable_description = stringr::str_remove_all(
        string = variable_description,
        pattern = '</font>'
      ),
      # remove line break tag
      variable_description = stringr::str_replace_all(
        string = variable_description,
        pattern = "<br>",
        replacement = ""
      ),
      # remove roster text substitution
      variable_description = stringr::str_replace_all(
        string = variable_description,
        pattern = "%rostertitle%",
        replacement = "[ITEM]"
      )
    ) |>
    # compose the variable display text as `{varname} : {variable_description}`
    dplyr::mutate(
      to_display = paste(variable_description,
        paste0("{", varname, "}"), sep = " : "
      )
    ) |>
    dplyr::pull(to_display)

  return(q_vars_out)

}

#' Create data frame of domain var values combinations
#'
#' @param path Path. Path to the raw questionnaire metadata data frame.
#' @param domain_vars Character vector. Names of domain vars.
#'
#' @return Data frame.
#' Columns are domain variables.
#' Contents are options.
#' Extent is the combination of all answer options for all domain variables.
#'
#' @importFrom purrr map
#' @importFrom susometa get_answer_options
#' @importFrom tibble tibble
create_domain_var_val_df <- function(
  path,
  domain_vars
) {

  # read in the data
  qnr_df <- readRDS(path)

  # extract answer options for a given variable
  answer_options <- purrr::map(
    .x = domain_vars,
    .f = ~ susometa::get_answer_options(
      qnr_df = qnr_df,
      varname = .data[[.x]]
    )
  )

  # create a data frame that is a combination of these answer option values
  domains_df <- tidyr::expand_grid(
    # note: use rlang splicing operator to "convert" list to set of arguments
    !!!answer_options
  )

  # rename the columns to match the variable names
  names(domains_df) <- domain_vars

  # add an observations column
  domains_df <- tibble::tibble(
    domains_df,
    Obs = NA_integer_
  )

  return(domains_df)

}
