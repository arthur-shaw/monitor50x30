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
        tbl_id = "parcels_per_hhold",
        tbl_desc = "parcels per household"
      ),
      parcel_gps = list(
        id = "4_quality_1_setup_1_tables_details_parcel_gps",
        tbl_id = "parcel_gps",
        tbl_desc = "parcel GPS measurement"
      ),
      plots_per_parcel = list(
        id = "4_quality_1_setup_1_tables_details_plots_per_parcel",
        tbl_id = "plots_per_parcel",
        tbl_desc = "plots per parcel"
      ),
      plot_use = list(
        id = "4_quality_1_setup_1_tables_details_plot_use",
        tbl_id = "plot_use",
        tbl_desc = "plot use"
      ),
      plot_gps = list(
        id = "4_quality_1_setup_1_tables_details_plot_gps",
        tbl_id = "plot_gps",
        tbl_desc = "plot GPS measurement"
      ),
      crops_per_plot = list(
        id = "4_quality_1_setup_1_tables_details_crops_per_plot",
        tbl_id = "crops_per_plot",
        tbl_desc = "crops per plot"
      ),
      crop_types = list(
        id = "4_quality_1_setup_1_tables_details_crop_types",
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
        tbl_id = "temp_crop_harvest",
        tbl_desc = "temporary crop harvest"
      ),
      temp_crop_sales = list(
        id = "4_quality_1_setup_1_tables_details_temp_crop_sales",
        tbl_id = "temp_crop_sales",
        tbl_desc = "temporary crop sales"
      ),
      perm_crop_harvest = list(
        id = "4_quality_1_setup_1_tables_details_perm_crop_harvest",
        tbl_id = "perm_crop_harvest",
        tbl_desc = "permanent crop harvest"
      ),
      perm_crop_sales = list(
        id = "4_quality_1_setup_1_tables_details_perm_crop_sales",
        tbl_id = "perm_crop_sales",
        tbl_desc = "permanent crop sales"
      ),
      livestock_owership = list(
        id = "4_quality_1_setup_1_tables_details_livestock_owership",
        tbl_id = "livestock_owership",
        tbl_desc = "livestock owership"
      ),
      cow_displacement = list(
        id = "4_quality_1_setup_1_tables_details_cow_displacement",
        tbl_id = "cow_displacement",
        tbl_desc = "cow displacement"
      ),
      hen_displacement = list(
        id = "4_quality_1_setup_1_tables_details_hen_displacement",
        tbl_id = "hen_displacement",
        tbl_desc = "hen displacement"
      ),
      milk_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_milk_prod_sales",
        tbl_id = "milk_prod_sales",
        tbl_desc = "sales and production of milk"
      ),
      egg_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_egg_prod_sales",
        tbl_id = "egg_prod_sales",
        tbl_desc = "sales and production of eggs"
      ),
      fisheries_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_fisheries_prod_sales",
        tbl_id = "fisheries_prod_sales",
        tbl_desc = "production and sales of fisheries products"
      ),
      aquaculture_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_aquaculture_prod_sales",
        tbl_id = "aquaculture_prod_sales",
        tbl_desc = "production and sales of aquaculture products"
      ),
      forestry_prod_sales = list(
        id = "4_quality_1_setup_1_tables_details_forestry_prod_sales",
        tbl_id = "forestry_prod_sales",
        tbl_desc = "production and sales of forestry products"
      ),
      process_crop_prod = list(
        id = "4_quality_1_setup_1_tables_details_process_crop_prod",
        tbl_id = "process_crop_prod",
        tbl_desc = "processing of crop production"
      ),
      crop_labor = list(
        id = "4_quality_1_setup_1_tables_details_crop_labor",
        tbl_id = "crop_labor",
        tbl_desc = "crop labor"
      ),
      livestock_labor_tbl = list(
        id = "4_quality_1_setup_1_tables_details_livestock_labor_tbl",
        tbl_id = "livestock_labor_tbl",
        tbl_desc = "livestock labor"
      ),
      fisheries_labor = list(
        id = "4_quality_1_setup_1_tables_details_fisheries_labor",
        tbl_id = "fisheries_labor",
        tbl_desc = "fisheries labor"
      ),
      aquaculture_labor = list(
        id = "4_quality_1_setup_1_tables_details_aquaculture_labor",
        tbl_id = "aquaculture_labor",
        tbl_desc = "aquaculture labor"
      ),
      forestry_labor = list(
        id = "4_quality_1_setup_1_tables_details_forestry_labor",
        tbl_id = "forestry_labor",
        tbl_desc = "forestry labor"
      ),
      income_sources = list(
        id = "4_quality_1_setup_1_tables_details_income_sources",
        tbl_id = "income_sources",
        tbl_desc = "income sources"
      )
    )

    # ==========================================================================
    # load module definitions
    # ==========================================================================

    # combine post-planting and post-harvest table lists
    tbls <- c(pp_tbls, ph_tbls)

    # load module definitions, iterating through each list element
    purrr::walk(
      .x = tbls,
      .f = function(tbl) {
        mod_4_quality_1_setup_1_tables_details_server(
          id = tbl$id,
          parent = session,
          r6 = r6,
          tbl_id = tbl$tbl_id,
          tbl_desc = tbl$tbl_desc
        )
      }
    )

    # ==========================================================================
    # show/hide depending on questionnaire details provided
    # ==========================================================================

    # --------------------------------------------------------------------------
    # load past values
    # --------------------------------------------------------------------------

    if (!is.null(r6$table_selections_saved)) {

      manage_tbl_toggles(r6 = r6)

    }

    # --------------------------------------------------------------------------
    # update upon setting questionnaire details
    # --------------------------------------------------------------------------

    gargoyle::on("save_settings", {

      manage_tbl_toggles(r6 = r6)

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
