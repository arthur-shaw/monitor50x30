#' Quarto template dependencies
#'
#' @description
#' These packages are required at runtime by the Quarto templates bundled
#' with the application under `inst/quarto/templates/`.
#'
#' They are deliberately listed in `Imports` even though they are not
#' called directly by functions in `R/`. Quarto renders these templates
#' in response to actions initiated by the application, so these packages
#' are considered core runtime dependencies.
#'
#' @importFrom knitr knit_child
#' @importFrom readr read_file
#' @importFrom crosstalk SharedData bscols filter_select
#' @importFrom reactablefmtr data_bars
#' @import tbls50x30
#'
#' @noRd
NULL
