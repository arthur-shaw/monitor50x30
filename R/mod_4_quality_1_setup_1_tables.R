#' 4_quality_1_setup_1_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_1_tables_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(

    # load UI functions for all possible tables choices to be shown

    # post-planting
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_parcels_per_hhold")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_parcel_gps")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_plots_per_parcel")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_plot_use")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_plot_gps")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_crops_per_plot")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_crop_types")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_temp_crop_harvest")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_temp_crop_sales")
    ),

    # post-harvest
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_perm_crop_harvest")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_perm_crop_sales")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_livestock_owership")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_cow_displacement")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_hen_displacement")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_anim_prod_sales")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_other_prod_sales")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_process_crop_prod")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_crop_labor")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_livestock_labor_tbl")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_sector_labor")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_income_sources")
    ),

    # save button to close accordion panel
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}

#' 4_quality_1_setup_1_tables Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_1_tables_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # define lists of table specs for post-planting and post-harvest
    # ==========================================================================

    # --------------------------------------------------------------------------
    # post-planting tables
    # --------------------------------------------------------------------------

    pp_tlbs <- list(
      parcels_per_hhold = list(
        id = "4_quality_1_setup_1_tables_details_parcels_per_hhold",
        show = TRUE,
        remove = FALSE,
        tbl_id = "parcels_per_hhold",
        tbl_desc = "parcels per household"
      ),
      parcel_gps = list(
        id = "4_quality_1_setup_1_tables_details_parcel_gps",
        show = TRUE,
        remove = FALSE,
        tbl_id = "parcel_gps",
        tbl_desc = "parcel GPS measurement"
      ),
      plots_per_parcel = list(
        id = "4_quality_1_setup_1_tables_details_plots_per_parcel",
        show = TRUE,
        remove = FALSE,
        tbl_id = "plots_per_parcel",
        tbl_desc = "plots per parcel"
      ),
      plot_use = list(
        id = "4_quality_1_setup_1_tables_details_plot_use",
        show = TRUE,
        remove = FALSE,
        tbl_id = "plot_use",
        tbl_desc = "plot use"
      ),
      plot_gps = list(
        id = "4_quality_1_setup_1_tables_details_plot_gps",
        show = TRUE,
        remove = FALSE,
        tbl_id = "plot_gps",
        tbl_desc = "plot GPS measurement"
      ),
      crops_per_plot = list(
        id = "4_quality_1_setup_1_tables_details_crops_per_plot",
        show = TRUE,
        remove = FALSE,
        tbl_id = "crops_per_plot",
        tbl_desc = "crops per plot"
      ),
      crop_types = list(
        id = "4_quality_1_setup_1_tables_details_crop_types",
        show = TRUE,
        remove = FALSE,
        tbl_id = "crop_types",
        tbl_desc = "crop types"
      )
    )

    # --------------------------------------------------------------------------
    # post-harvest tables
    # --------------------------------------------------------------------------

    ph_tlbs <- list(
      temp_crop_harvest = list(
        id = "4_quality_1_setup_1_tables_details_temp_crop_harvest",
        show = TRUE,
        remove = FALSE,
        tbl_id = "temp_crop_harvest",
        tbl_desc = "temporary crop harvest"
      ),
      temp_crop_sales = list(
        id = "4_quality_1_setup_1_tables_details_temp_crop_sales",
        show = TRUE,
        remove = FALSE,
        tbl_id = "temp_crop_sales",
        tbl_desc = "temporary crop sales"
      ),
      perm_crop_harvest = list(
        id = "4_quality_1_setup_1_tables_details_perm_crop_harvest",
        show = TRUE,
        remove = FALSE,
        tbl_id = "perm_crop_harvest",
        tbl_desc = "permanent crop harvest"
      ),
      perm_crop_sales = list(
        id = "4_quality_1_setup_1_tables_details_perm_crop_sales",
        show = TRUE,
        remove = FALSE,
        tbl_id = "perm_crop_sales",
        tbl_desc = "permanent crop sales"
      ),
      livestock_owership = list(
        id = "4_quality_1_setup_1_tables_details_livestock_owership",
        show = TRUE,
        remove = FALSE,
        tbl_id = "livestock_owership",
        tbl_desc = "livestock owership"
      ),
      cow_displacement = list(
        id = "4_quality_1_setup_1_tables_details_cow_displacement",
        show = TRUE,
        remove = FALSE,
        tbl_id = "cow_displacement",
        tbl_desc = "cow displacement"
      ),
      hen_displacement = list(
        id = "4_quality_1_setup_1_tables_details_hen_displacement",
        show = TRUE,
        remove = FALSE,
        tbl_id = "hen_displacement",
        tbl_desc = "hen displacement"
      ),
      anim_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_anim_prod_sales",
        show = TRUE,
        remove = FALSE,
        tbl_id = "anim_prod_sales",
        tbl_desc = "sales and production of milk or eggs"
      ),
      other_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_other_prod_sales",
        show = TRUE,
        remove = FALSE,
        tbl_id = "other_prod_sales",
        tbl_desc = "production and sales of other animal products"
      ),
      process_crop_prod = list(
        id = "4_quality_1_setup_1_tables_details_process_crop_prod",
        show = TRUE,
        remove = FALSE,
        tbl_id = "process_crop_prod",
        tbl_desc = "processing of crop production"
      ),
      crop_labor = list(
        id = "4_quality_1_setup_1_tables_details_crop_labor",
        show = TRUE,
        remove = FALSE,
        tbl_id = "crop_labor",
        tbl_desc = "crop labor"
      ),
      livestock_labor_tbl = list(
        id = "4_quality_1_setup_1_tables_details_livestock_labor_tbl",
        show = TRUE,
        remove = FALSE,
        tbl_id = "livestock_labor_tbl",
        tbl_desc = "livestock labor"
      ),
      sector_labor = list(
        id = "4_quality_1_setup_1_tables_details_sector_labor",
        show = TRUE,
        remove = FALSE,
        tbl_id = "sector_labor",
        tbl_desc = "sector labor"
      ),
      income_sources = list(
        id = "4_quality_1_setup_1_tables_details_income_sources",
        show = TRUE,
        remove = FALSE,
        tbl_id = "income_sources",
        tbl_desc = "income sources"
      )
    )

    # ==========================================================================
    # create table lists for specific questionnaires from template lists above
    # ==========================================================================

    # --------------------------------------------------------------------------
    # post-planting
    # --------------------------------------------------------------------------

    core_ag_2v_pp <- pp_tlbs
    core_ag_1v_hh_pp <- pp_tlbs
    ilp_2v_pp <- pp_tlbs
    pme_2v_pp <- pp_tlbs

    core_ag_1v_non_hh_pp <- set_tbls_to_remove(
      tbls = pp_tlbs,
      names = c("parcels_per_hhold", "parcel_gps")
    )

    ilp_1v_pp <- set_tbls_to_remove(
      tbls = pp_tlbs,
      names = c("parcels_per_hhold", "parcel_gps", "plot_gps")
    )

    # --------------------------------------------------------------------------
    # post-harvest
    # --------------------------------------------------------------------------

    core_ag_2v_ph <- set_tbls_to_remove(
      tbls = ph_tlbs,
      names = c(
        "process_crop_prod", "crop_labor", "livestock_labor_tbl", "sector_labor"
      )
    )

    core_ag_1v_hh_ph <- core_ag_2v_ph
    core_ag_1v_non_hh_ph <- core_ag_2v_ph

    core_ag_minor <- set_tbls_to_remove(
      tbls = ph_tlbs,
      names = c(
        "perm_crop_harvest", "perm_crop_sales", "livestock_ownership",
        "cow_displacement", "hen_displacement", "anim_prod_sales",
        "other_prod_sales", "process_crop_prod",
        "crop_labor", "livestock_labor_tbl", "sector_labor", "income_sources"
      )
    )

    ilp_2v_ph <- ph_tlbs
    ilp_1v_ph <- set_tbls_to_remove(
      tbls = ph_tlbs,
      names = c("crop_labor", "livestock_labor_tbl", "sector_labor")
    )
    pme_2v_ph <- ilp_1v_ph

    mea_2v_ph <- set_tbls_to_remove(
      tbls = ilp_1v_ph,
      names = "process_crop_prod"
    )

    # --------------------------------------------------------------------------
    # combine post-planting and post-harvest lists of single-visit cases
    # --------------------------------------------------------------------------

    core_ag_1v_hh <- c(core_ag_1v_hh_pp, core_ag_1v_hh_ph)
    core_ag_1v_non_hh <- c(core_ag_1v_non_hh_pp, core_ag_1v_non_hh_ph)
    ilp_1v <- c(ilp_1v_pp, ilp_1v_ph)

    # ==========================================================================
    # initialize page, by loading server logic of relevant child modules
    # ==========================================================================

    if (!is.null(r6$core_settings_saved)) {

      shiny::req(r6$qnr_templates)
      shiny::req(r6$svy_current_visit)

      # -----------------------------------------------------------------------
      # CORE-AG
      # -----------------------------------------------------------------------

      if ("CORE-AG" %in% r6$qnr_templates) {

  # TODO: differentiate between household and non-household sector
  # when done, disable parcels_per_hhold and parcel_gps
        if (r6$svy_current_visit == "Single visit") {

          set_table_choices(tbls = core_ag_1v_hh, session = session, r6 = r6)

        }

        if (r6$svy_current_visit == "Post-planting") {

          set_table_choices(tbls = core_ag_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          set_table_choices(tbls = core_ag_2v_ph, session = session, r6 = r6)

        }

      }

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      # ILP
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      if ("ILP" %in% r6$qnr_templates) {

        if (r6$svy_current_visit == "Post-planting") {

          set_table_choices(tbls = ilp_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          set_table_choices(tbls = ilp_2v_ph, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Single visit") {

          set_table_choices(tbls = ilp_1v, session = session, r6 = r6)

        }

      }

      # -----------------------------------------------------------------------
      # PME
      # -----------------------------------------------------------------------

      if (r6$qnr_templates == "PME") {

        if (r6$svy_current_visit == "Post-planting") {

          set_table_choices(tbls = pme_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          set_table_choices(tbls = pme_2v_ph, session = session, r6 = r6)

        }

      }

      # -----------------------------------------------------------------------
      # MEA
      # -----------------------------------------------------------------------

      if (r6$qnr_templates == "MEA") {

        set_table_choices(tbls = mea_2v_ph, session = session, r6 = r6)

      }

    }

    # ==========================================================================
    # update page when new settings provided
    # ==========================================================================

    gargoyle::on("save_settings", {

      # -----------------------------------------------------------------------
      # CORE-AG
      # -----------------------------------------------------------------------

      if ("CORE-AG" %in% r6$qnr_templates) {

  # TODO: differentiate between household and non-household sector
  # when done, disable parcels_per_hhold and parcel_gps
        if (r6$svy_current_visit == "Single visit") {

          set_table_choices(tbls = core_ag_1v_hh, session = session, r6 = r6)

        }

        if (r6$svy_current_visit == "Post-planting") {

          set_table_choices(tbls = core_ag_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          set_table_choices(tbls = core_ag_2v_ph, session = session, r6 = r6)

        }

      }

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      # ILP
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      if ("ILP" %in% r6$qnr_templates) {

        if (r6$svy_current_visit == "Post-planting") {

          set_table_choices(tbls = ilp_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          set_table_choices(tbls = ilp_2v_ph, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Single visit") {

          set_table_choices(tbls = ilp_1v, session = session, r6 = r6)

        }

      }

      # -----------------------------------------------------------------------
      # PME
      # -----------------------------------------------------------------------

      if (r6$qnr_templates == "PME") {

        if (r6$svy_current_visit == "Post-planting") {

          set_table_choices(tbls = pme_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          set_table_choices(tbls = pme_2v_ph, session = session, r6 = r6)

        }

      }

      # -----------------------------------------------------------------------
      # MEA
      # -----------------------------------------------------------------------

      if (r6$qnr_templates == "MEA") {

        set_table_choices(tbls = mea_2v_ph, session = session, r6 = r6)

      }

    })

    # ==========================================================================
    # react to save buttons
    # ==========================================================================

    shiny::observeEvent(input$save, {

      gargoyle::trigger("save_tables")

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_1_tables_ui("4_quality_1_setup_1_tables_1")
    
## To be copied in the server
# mod_4_quality_1_setup_1_tables_server("4_quality_1_setup_1_tables_1")
