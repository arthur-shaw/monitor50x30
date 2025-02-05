#' table_selection 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

#' Set table choices
#'
#' @description
#' Factory to build server functions for table choices.
#' Takes a list of tables and their parameters for the server function.
#'
#' @param tbls List of lists. Inner lists are parameters for table selections
#' to show/remove in UI.
#' @param session Shiny session object
#' @param r6 R6 object
#'
#' @return Server functions with appropriate set of parameters.
#'
#' @importFrom purrr walk
set_table_choices <- function(tbls, session, r6) {

  purrr::walk(
    .x = tbls,
    .f = function(tbl) {
      mod_4_quality_1_setup_1_tables_details_server(
        id = tbl$id,
        parent = session,
        r6 = r6,
        show = tbl$show,
        remove = tbl$remove,
        tbl_id = tbl$tbl_id,
        tbl_desc = tbl$tbl_desc
      )
    }
  )

}

#' Set target tables in list to be removed
#'
#' @description
#' Modify named elements of list in two ways:
#'
#' - Set `show` to `FALSE`
#' - Set `remove` to `TRUE`
#'
#' This will have the effect of instructing the server function to remove
#' entries for target tables from the UI.
#'
#' @param tbls List of lists. Inner lists are parameters for table selections
#' to show/remove in UI.
#' @param names Names of inner lists whose contents to mutate.
#'
#' @return List of lists where target inner lists have modified contents.
#'
#' @importFrom purrr imap
set_tbls_to_remove <- function(
  tbls,
  names
) {

  tbls_mod <- purrr::imap(
    .x = tbls,
    .f = function(x, name) {
      if (x$id %in% names) {
        x$show <- FALSE
        x$remove <- TRUE
      }
      return(x)
    }
  )

  return(tbls_mod)

}
