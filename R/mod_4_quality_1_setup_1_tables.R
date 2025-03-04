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
      id = ns("4_quality_1_setup_1_tables_details_milk_prod_sales")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_egg_prod_sales")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_fisheries_prod_sales")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_aquaculture_prod_sales")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_forestry_prod_sales")
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
      id = ns("4_quality_1_setup_1_tables_details_fisheries_labor")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_aquaculture_labor")
    ),
    mod_4_quality_1_setup_1_tables_details_ui(
      id = ns("4_quality_1_setup_1_tables_details_forestry_labor")
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

    pp_tbls <- list(
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

    ph_tbls <- list(
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
      milk_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_milk_prod_sales",
        show = TRUE,
        remove = FALSE,
        tbl_id = "milk_prod_sales",
        tbl_desc = "sales and production of milk"
      ),
      egg_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_egg_prod_sales",
        show = TRUE,
        remove = FALSE,
        tbl_id = "egg_prod_sales",
        tbl_desc = "sales and production of eggs"
      ),
      fisheries_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_fisheries_prod_sales",
        show = TRUE,
        remove = FALSE,
        tbl_id = "fisheries_prod_sales",
        tbl_desc = "production and sales of fisheries products"
      ),
      aquaculture_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_aquaculture_prod_sales",
        show = TRUE,
        remove = FALSE,
        tbl_id = "aquaculture_prod_sales",
        tbl_desc = "production and sales of aquaculture products"
      ),
      forestry_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_forestry_prod_sales",
        show = TRUE,
        remove = FALSE,
        tbl_id = "forestry_prod_sales",
        tbl_desc = "production and sales of forestry products"
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
      fisheries_labor = list(
        id = "4_quality_1_setup_1_tables_details_fisheries_labor",
        show = TRUE,
        remove = FALSE,
        tbl_id = "fisheries_labor",
        tbl_desc = "fisheries labor"
      ),
      aquaculture_labor = list(
        id = "4_quality_1_setup_1_tables_details_aquaculture_labor",
        show = TRUE,
        remove = FALSE,
        tbl_id = "aquaculture_labor",
        tbl_desc = "aquaculture labor"
      ),
      forestry_labor = list(
        id = "4_quality_1_setup_1_tables_details_forestry_labor",
        show = TRUE,
        remove = FALSE,
        tbl_id = "forestry_labor",
        tbl_desc = "forestry labor"
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
    # all tables
    # --------------------------------------------------------------------------

    all_tbls <- c(pp_tbls, ph_tbls)

    # --------------------------------------------------------------------------
    # post-planting
    # --------------------------------------------------------------------------

    # create a table list where all PP tables are shown and all PH ones removed
    pp_visit_tbls <- set_tbls_to_remove(
      tbls = all_tbls,
      names = names(ph_tbls)
    )

    # define tables for questionnaires that use all PP tables
    core_ag_2v_pp <- pp_visit_tbls
    ilp_2v_pp <- pp_visit_tbls
    pme_2v_pp <- pp_visit_tbls

    # create CORE-AG 1-visit from all PP tables and remove a few
    core_ag_1v_non_hh_pp <- set_tbls_to_remove(
      tbls = pp_visit_tbls,
      names = c("parcels_per_hhold", "parcel_gps")
    )

    # --------------------------------------------------------------------------
    # post-harvest
    # --------------------------------------------------------------------------

    # create a table list where all PH tables are shown and all PP ones removed
    ph_visit_tbls <- set_tbls_to_remove(
      tbls = all_tbls,
      names = names(pp_tbls)
    )

    # specify tables to remove for 2-visit CORE-AG
    # shared by 1-visit CORE-AG variants
    core_ag_2v_to_remove <- c(
        "process_crop_prod", "crop_labor", "livestock_labor_tbl",
        "fisheries_labor", "aquaculture_labor", "forestry_labor"
      )

    core_ag_2v_ph <- set_tbls_to_remove(
      tbls = ph_visit_tbls,
      names = core_ag_2v_to_remove
    )

    core_ag_minor <- set_tbls_to_remove(
      tbls = ph_visit_tbls,
      names = c(
        "perm_crop_harvest", "perm_crop_sales", "livestock_ownership",
        "cow_displacement", "hen_displacement",
        "milk_prod_sales", "egg_prod_sales",
        "fisheries_prod_sales", "aquaculture_prod_sales", "forestry_prod_sales",
        "process_crop_prod",
        "crop_labor", "livestock_labor_tbl",
        "fisheries_labor", "aquaculture_labor", "forestry_labor",
        "income_sources"
      )
    )

    ilp_2v_ph <- ph_visit_tbls

    # specify tables to remove for ILP 1-visit
    # shared by PME and MEA
    ilp_1v_ph_to_remove <- c(
        "crop_labor", "livestock_labor_tbl",
        "fisheries_labor", "aquaculture_labor", "forestry_labor"
      )

    pme_2v_ph <- set_tbls_to_remove(
      tbls = ph_visit_tbls,
      names = ilp_1v_ph_to_remove
    )

    mea_2v_ph <- set_tbls_to_remove(
      tbls = pme_2v_ph,
      names = "process_crop_prod"
    )

    # --------------------------------------------------------------------------
    # single-visit
    # --------------------------------------------------------------------------

    core_ag_1v_hh <- set_tbls_to_remove(
      tbls = all_tbls,
      names = c(
        # remove from PP
        # none
        # remove from PH
        "process_crop_prod", "crop_labor", "livestock_labor_tbl",
        "fisheries_labor", "aquaculture_labor", "forestry_labor"
      )
    )

    core_ag_1v_non_hh <- set_tbls_to_remove(
      tbls = all_tbls,
      names = c(
        # remove from PP
        "parcels_per_hhold", "parcel_gps",
        # remove from PH
        "process_crop_prod", "crop_labor", "livestock_labor_tbl",
        "fisheries_labor", "aquaculture_labor", "forestry_labor"
      )
    )

    # create ILP 1-visit from all PP tables and remove a few
    ilp_1v <- set_tbls_to_remove(
      tbls = all_tbls,
      names = c(
        # remove from PP
        "parcels_per_hhold", "parcel_gps", "plot_gps",
        # remove from PH
        ilp_1v_ph_to_remove
      )
    )

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

          shiny::req(ilp_2v_pp)
          set_table_choices(tbls = ilp_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          shiny::req(ilp_2v_ph)
          set_table_choices(tbls = ilp_2v_ph, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Single visit") {

          shiny::req(ilp_1v)
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

      # parameters
      shiny::req(r6$qnr_templates)
      shiny::req(r6$svy_current_visit)

      # calcultations of table specs
      shiny::req(pp_tbls)
      shiny::req(ph_tbls)

      # -----------------------------------------------------------------------
      # CORE-AG
      # -----------------------------------------------------------------------

      if ("CORE-AG" %in% r6$qnr_templates) {

  # TODO: differentiate between household and non-household sector
  # when done, disable parcels_per_hhold and parcel_gps
        if (r6$svy_current_visit == "Single visit") {

          shiny::req(core_ag_1v_hh)
          set_table_choices(tbls = core_ag_1v_hh, session = session, r6 = r6)

        }

        if (r6$svy_current_visit == "Post-planting") {

          shiny::req(core_ag_2v_pp)
          set_table_choices(tbls = core_ag_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          shiny::req(core_ag_2v_ph)
          set_table_choices(tbls = core_ag_2v_ph, session = session, r6 = r6)

        }

      }

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      # ILP
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      if ("ILP" %in% r6$qnr_templates) {

        if (r6$svy_current_visit == "Post-planting") {

          shiny::req(ilp_2v_pp)
cat("\nSwitched to ILP post-planting")
# cat("\nPost-planting's table spec")
# str(ilp_2v_pp)
          shiny::removeUI(
            selector = '[id$="insert_reference"]',
            multiple = TRUE
          )
          shiny::removeUI(
            selector = '[id$="inserted_ui"]',
            multiple = TRUE
          )

          set_table_choices(tbls = ilp_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          shiny::req(ilp_2v_ph)
cat("\nSwitched to ILP post-harvest")
# cat("\nPost-planting's table spec")
# str(ilp_2v_ph)
          shiny::removeUI(
            selector = '[id$="insert_reference"]',
            multiple = TRUE
          )
          shiny::removeUI(
            selector = '[id$="inserted_ui"]',
            multiple = TRUE
          )
          set_table_choices(tbls = ilp_2v_ph, session = session, r6 = r6)
# cat("\nPost-planting's table spec")
# str(ilp_2v_ph)

        } else if (r6$svy_current_visit == "Single visit") {

          shiny::req(ilp_1v)
cat("\nSwitched to single-visit ILP")
# cat("\nSingle-visit's table spec")
# str(ilp_1v)
          shiny::removeUI(
            selector = '[id$="insert_reference"]',
            multiple = TRUE
          )
          shiny::removeUI(
            selector = '[id$="inserted_ui"]',
            multiple = TRUE
          )
          set_table_choices(tbls = ilp_1v, session = session, r6 = r6)
# cat("\nSingle-visit's table spec")
# str(ilp_1v)
        }

      }

      # -----------------------------------------------------------------------
      # PME
      # -----------------------------------------------------------------------

      if (r6$qnr_templates == "PME") {

        if (r6$svy_current_visit == "Post-planting") {

          shiny::req(pme_2v_pp)
          set_table_choices(tbls = pme_2v_pp, session = session, r6 = r6)

        } else if (r6$svy_current_visit == "Post-harvest") {

          shiny::req(pme_2v_ph)
          set_table_choices(tbls = pme_2v_ph, session = session, r6 = r6)

        }

      }

      # -----------------------------------------------------------------------
      # MEA
      # -----------------------------------------------------------------------

      if (r6$qnr_templates == "MEA") {

        shiny::req(mea_2v_ph)
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
