#' Create a popover to explain the label for input fields
#'
#' @param lbl Character. Input label.
#' @param icon Character. Name of the
#' [Bootstrap icon](https://icons.getbootstrap.com/).
#' @param desc Character. Longer description.
#'
#' @importFrom bslib popover
#' @importFrom bsicons bs_icon
info_popover <- function(
  lbl,
  icon = "info-circle",
  desc
) {

  bslib::popover(
    trigger = list(
      lbl,
      bsicons::bs_icon(icon)
    ),
    desc
  )

}
