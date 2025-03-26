#' Manage state of table selector toggles based on questionnaire details
#'
#' @description
#' Manages state in two complementary ways:
#' 
#' - Shows/hides the target tables
#' - Resets the table selection value for hidden tables in the R6 object
#'
#' @param r6 R6 object.
#'
#' @noRd
manage_tbl_toggles <- function(r6) {

  # ============================================================================
  # list all tables
  # ============================================================================

  pp_tbls <- c(
    "parcels_per_hhold",
    "parcel_gps",
    "plots_per_parcel",
    "plot_use",
    "plot_gps",
    "crops_per_plot",
    "crop_types"
  )

  ph_tbls <- c(
    "temp_crop_harvest",
    "temp_crop_sales",
    "perm_crop_harvest",
    "perm_crop_sales",
    "livestock_owership",
    "cow_displacement",
    "hen_displacement",
    "milk_prod_sales",
    "egg_prod_sales",
    "egg_prod_sales",
    "fisheries_prod_sales",
    "aquaculture_prod_sales",
    "forestry_prod_sales",
    "process_crop_prod",
    "crop_labor",
    "livestock_labor_tbl",
    "fisheries_labor",
    "aquaculture_labor",
    "forestry_labor",
    "income_sources"

  )

  all_tbls <- c(pp_tbls, ph_tbls)

  # ============================================================================
  # show/hide tables as a function of the questionnaire/visit
  # ============================================================================

  # ----------------------------------------------------------------------------
  # CORE-AG
  # ----------------------------------------------------------------------------

  if ("CORE-AG" %in% r6$qnr_templates) {

    if (r6$svy_current_visit == "Single visit") {

      core_ag_1v_hh_hide <- c(
        # PP
        # none
        # PH
        "process_crop_prod", "crop_labor", "livestock_labor_tbl",
        "fisheries_labor", "aquaculture_labor", "forestry_labor"
      )
      core_ag_1v_hh_show <- all_tbls[!all_tbls %in% core_ag_1v_hh_hide]

      # show
      set_tbl_visibility(tbl = core_ag_1v_hh_show, action = "show")

      # hide
      set_tbl_visibility(tbl = core_ag_1v_hh_hide, action = "hide")
      # reset values of hidden fields
      reset_tbl_val(r6 = r6, tbl = core_ag_1v_hh_hide)
      r6$write()

    }

    if (r6$svy_current_visit == "Post-planting") {

      # show
      set_tbl_visibility(
        tbl = pp_tbls,
        action = "show"
      )

      # hide
      set_tbl_visibility(
        tbl = ph_tbls,
        action = "hide"
      )
      # reset values of hidden fields
      reset_tbl_val(r6 = r6, tbl = ph_tbls)
      r6$write()

    } else if (r6$svy_current_visit == "Post-harvest") {

      core_ag_2v_hide <- c(
        # PP
        pp_tbls,
        # PH
        "process_crop_prod", "crop_labor", "livestock_labor_tbl",
        "fisheries_labor", "aquaculture_labor", "forestry_labor"
      )

      core_ag_2v_show <- ph_tbls[!ph_tbls %in% core_ag_2v_hide]

      # show
      set_tbl_visibility(
        tbl = core_ag_2v_show,
        action = "show"
      )

      # hide
      set_tbl_visibility(
        tbl = core_ag_2v_hide,
        action = "hide"
      )
      # reset values of hidden fields
      reset_tbl_val(r6 = r6, tbl = core_ag_2v_hide)
      r6$write()

    }


  }

  # ----------------------------------------------------------------------------
  # ILP
  # ----------------------------------------------------------------------------

  if ("ILP" %in% r6$qnr_templates) {

    if (r6$svy_current_visit == "Post-planting") {

      # show
      set_tbl_visibility(
        tbl = pp_tbls,
        action = "show"
      )

      # hide
      set_tbl_visibility(
        tbl = ph_tbls,
        action = "hide"
      )
      # reset values of hidden fields
      reset_tbl_val(r6 = r6, tbl = ph_tbls)
      r6$write()

    } else if (r6$svy_current_visit == "Post-harvest") {

      # show
      set_tbl_visibility(
        tbl = ph_tbls,
        action = "show"
      )

      # hide
      set_tbl_visibility(
        tbl = pp_tbls,
        action = "hide"
      )
      # reset values of hidden fields
      reset_tbl_val(r6 = r6, tbl = ph_tbls)
      r6$write()

    } else if (r6$svy_current_visit == "Single visit") {

      ilp_1v_hide <- c(
        # PP
        "parcels_per_hhold", "parcel_gps", "plot_gps",
        # PH
        "crop_labor", "livestock_labor_tbl",
        "fisheries_labor", "aquaculture_labor", "forestry_labor"
      )

      ilp_1v_show <- all_tbls[!all_tbls %in% ilp_1v_hide]

      # show
      set_tbl_visibility(
        id = ilp_1v_show,
        action = "show"
      )

      # hide
      set_tbl_visibility(
        id = ilp_1v_hide,
        action = "hide"
      )
      # reset values of hidden fields
      reset_tbl_val(r6 = r6, tbl = ilp_1v_hide)
      r6$write()

    }

  }

  # ----------------------------------------------------------------------------
  # PME
  # ----------------------------------------------------------------------------

  if (r6$qnr_templates == "PME") {

    if (r6$svy_current_visit == "Post-planting") {

      # show
      set_tbl_visibility(
        tbl = pp_tbls,
        action = "show"
      )

      # hide
      set_tbl_visibility(
        tbl = ph_tbls,
        action = "hide"
      )
      # reset values of hidden fields
      reset_tbl_val(r6 = r6, tbl = ph_tbls)
      r6$write()

    } else if (r6$svy_current_visit == "Post-harvest") {

      pme_2v_ph_hide <- c(
        # PP
        pp_tbls,
        # PH
        "crop_labor", "livestock_labor_tbl",
        "fisheries_labor", "aquaculture_labor", "forestry_labor"
      )

      pme_2v_ph_show <- ph_tbls[!ph_tbls %in% pme_2v_ph_hide]

      # show
      set_tbl_visibility(
        tbl = pme_2v_ph_show,
        action = "show"
      )

      # hide
      set_tbl_visibility(
        tbl = pme_2v_ph_hide,
        action = "hide"
      )
      # reset values of hidden fields
      reset_tbl_val(r6 = r6, tbl = pme_2v_ph_hide)
      r6$write()

    }

  }

  # ----------------------------------------------------------------------------
  # MEA
  # ----------------------------------------------------------------------------

  if (r6$qnr_templates == "MEA") {

    mea_2v_ph_hide <- c(
      # PP
      pp_tbls,
      # PH
      "process_crop_prod"
    )

    mea_2v_ph_show <- ph_tbls[!ph_tbls %in% mea_2v_ph_hide]

    # show
    set_tbl_visibility(
      tbl = mea_2v_ph_show,
      action = "show"
    )

    # hide
    set_tbl_visibility(
      tbl = mea_2v_ph_hide,
      action = "hide"
    )
    # reset values of hidden fields
    reset_tbl_val(r6 = r6, tbl = mea_2v_ph_hide)
    r6$write()

  }


}

#' Set the visibility of the table toggle div
#'
#' @param tbl Character. Name of the table.
#' @param action Character. Either `show` or `hide`.
#'
#' @return Side effect of showing or hiding div container in the UI
#'
#' @importFrom purrr walk
#' @importFrom shinyjs show hide
#' @importFrom glue glue
#'
#' @noRd
set_tbl_visibility <- function(
  tbl,
  action
) {


  if (action == "show") {

    purrr::walk(
      .x = tbl,
      .f = ~ shinyjs::show(
        # since namepace resolution is a bit tricky
        # use a selector for IDs ending in the table name
        # plus the name of the div containing the child module's UI
        selector = glue::glue('[id$="{.x}-tbl_toggle"]')
      )
    )

  } else if (action == "hide") {

    purrr::walk(
      .x = tbl,
      .f = ~ shinyjs::hide(
        selector = glue::glue('[id$="{.x}-tbl_toggle"]')
      )
    )

  }

}

#' Reset table values in R6 and in UI
#'
#' @param r6 R6 object.
#' @param tbl Character vector. Name of the target table(s).
#'
#' @return Side-effect of changing values in R6 and updating switch in the UI.
#'
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom bslib update_switch
#'
#' @noRd
reset_tbl_val <- function(r6, tbl) {

  reset_val <- function(r6, tbl) {

    # compose names of R6 field based on `tbl_id`
    # avoids recomputation of the same in a few places
    r6_field_name <- glue::glue("use_{tbl}")

    # reset the value of the table toggle if the table is hidden
    if (!is.null(r6[[r6_field_name]])) {
      r6[[r6_field_name]] <- FALSE
    }

  }

  # reset in R6
  purrr::walk(
    .x = tbl,
    .f = ~ reset_val(
      r6 = r6,
      tbl = .x
    )
  )
  r6$write()

  # reset in UI
  purrr::walk(
    .x = tbl,
    .f = ~ bslib::update_switch(
      id = glue::glue("4_quality_1_setup_1_tables_details_{.x}-use"),
      value = FALSE
    )
  )

}
