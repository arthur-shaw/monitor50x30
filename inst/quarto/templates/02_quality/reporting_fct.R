#' Construct input data frame for data quality table functions
#'
#' @description
#' Pipeline for constructing data needed for tbls50x30 functions.
#'
#' @param data_path Character. Directory where combined microdata are found.
#' @param analysis_df_name Character. Name in params RDS of data frame selection.
#' @param interview_meta_df Data frame. Interviews with metadata on supervisor
#' and team responsible
#'
#' @return Data frame.
#'
#' @importFrom fs path
#' @importFrom haven read_dta
#' @importFrom dplyr left_join
create_df_for_table <- function(
  data_path,
  analysis_df_name,
  interview_meta_df
) {

  analysis_df <- fs::path(
    data_path,
    paste0(params_df[[analysis_df_name]], ".dta")
  ) |>
	haven::read_dta()

  df_for_table <- interview_meta_df |>
    dplyr::left_join(
      y = analysis_df,
      by = c("interview__id", "interview__key")
    )

  return(df_for_table)

}
