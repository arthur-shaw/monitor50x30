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

#' Create a tooltip attached to the label of Shiny inputs
#'
#' @inheritParams info_popover
#' @parm placement Character. Takes values from `bslib::tooltip()`.
#'
#' @importFrom bslib tooltip
#' @importFrom bsicons bs_icon
label_tooltip <- function(
  lbl,
  desc,
  icon = "info-circle",
  placement = "auto"
) {

  bslib::tooltip(
    trigger = list(
      lbl,
      bsicons::bs_icon(icon)
    ),
    desc,
    placement = placement
  )

}
