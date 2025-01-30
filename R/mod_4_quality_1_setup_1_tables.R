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
    # initialize page
    # ==========================================================================

    # mod_4_quality_1_setup_1_tables_1_parcels_per_hhold_server(
    #   id = "4_quality_1_setup_1_tables_1_parcels_per_hhold_1",
    #   parent = session,
    #   r6 = r6,
    #   show = TRUE
    # )

    # ==========================================================================
    # Load server logic of child modules
    # ==========================================================================

    # post-planting
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_parcels_per_hhold",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "parcels_per_hhold",
      tbl_desc = "parcels per household"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_parcel_gps",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "parcel_gps",
      tbl_desc = "parcel GPS measurement"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_plots_per_parcel",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "plots_per_parcel",
      tbl_desc = "plots per parcel"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_plot_use",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "plot_use",
      tbl_desc = "plot use"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_plot_gps",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "plot_gps",
      tbl_desc = "plot GPS measurement"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_crops_per_plot",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "crops_per_plot",
      tbl_desc = "crops per plot"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_crop_types",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "crop_types",
      tbl_desc = "crop types"
    )

    # post-harvest
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_temp_crop_harvest",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "temp_crop_harvest",
      tbl_desc = "temporary crop harvest"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_temp_crop_sales",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "temp_crop_sales",
      tbl_desc = "temporary crop sales"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_perm_crop_harvest",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "perm_crop_harvest",
      tbl_desc = "permanent crop harvest"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_perm_crop_sales",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "perm_crop_sales",
      tbl_desc = "permanent crop sales"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_livestock_owership",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "livestock_owership",
      tbl_desc = "livestock owership"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_cow_displacement",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "cow_displacement",
      tbl_desc = "cow displacement"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_hen_displacement",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "hen_displacement",
      tbl_desc = "hen displacement"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_anim_prod_sales",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "anim_prod_sales",
      tbl_desc = "sales and production of milk or eggs"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_other_prod_sales",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "other_prod_sales",
      tbl_desc = "production and sales of other animal products"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_process_crop_prod",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "process_crop_prod",
      tbl_desc = "processing crop production"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_crop_labor",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "crop_labor",
      tbl_desc = "crop labor"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_livestock_labor_tbl",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "livestock_labor_tbl",
      tbl_desc = "livestock labor"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_sector_labor",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "sector_labor",
      tbl_desc = "sector labor"
    )
    mod_4_quality_1_setup_1_tables_details_server(
      id = "4_quality_1_setup_1_tables_details_income_sources",
      parent = session,
      r6 = r6,
      show = TRUE,
      tbl_id = "income_sources",
      tbl_desc = "income sources"
    )

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_1_tables_ui("4_quality_1_setup_1_tables_1")
    
## To be copied in the server
# mod_4_quality_1_setup_1_tables_server("4_quality_1_setup_1_tables_1")
