#' 4_quality_1_setup_2_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_2_data_ui <- function(id) {
  ns <- NS(id)
  tagList(

    bslib::accordion(
      id = ns("table_data"),

      # ------------------------------------------------------------------------
      # post-planting tables
      # ------------------------------------------------------------------------

      bslib::accordion_panel(
        title = "Parcels per household",
        value = "parcels_per_hhold",
        mod_4_quality_1_setup_2_data_parcels_per_hhold_ui(
          id = ns("4_quality_1_setup_2_data_parcels_per_hhold_1")
        )
      ),
      bslib::accordion_panel(
        title = "Parcel GPS measurement",
        value = "parcel_gps",
        mod_4_quality_1_setup_2_data_parcel_gps_ui(
          id = ns("4_quality_1_setup_2_data_parcel_gps_1")
        )
      ),
      bslib::accordion_panel(
        title = "Plots per parcel",
        value = "plots_per_parcel",
        mod_4_quality_1_setup_2_data_plots_per_parcel_ui(
          id = ns("4_quality_1_setup_2_data_plots_per_parcel_1")
        )
      ),
      bslib::accordion_panel(
        title = "Plot use",
        value = "plot_use",
        mod_4_quality_1_setup_2_data_plot_use_ui(
          id = ns("4_quality_1_setup_2_data_plot_use_1")
        )
      ),
      bslib::accordion_panel(
        title = "Plot GPS measurement",
        value = "plot_gps",
        mod_4_quality_1_setup_2_data_plot_gps_ui(
          id = ns("4_quality_1_setup_2_data_plot_gps_1")
        )
      ),
      bslib::accordion_panel(
        title = "Crops per plot",
        value = "crops_per_plot",
        mod_4_quality_1_setup_2_data_crops_per_plot_ui(
          id = ns("4_quality_1_setup_2_data_crops_per_plot_1")
        )
      ),
      bslib::accordion_panel(
        title = "Crop types",
        value = "crop_types",
        mod_4_quality_1_setup_2_data_crop_types_ui(
          id = ns("4_quality_1_setup_2_data_crop_types_1")
        )
      ),

      # ------------------------------------------------------------------------
      # post-harvest tables
      # ------------------------------------------------------------------------

      bslib::accordion_panel(
        title = "Temporary crop harvest",
        value = "temp_crop_harvest",
        mod_4_quality_1_setup_2_data_temp_crop_harvest_ui(
          id = ns("4_quality_1_setup_2_data_temp_crop_harvest_1")
        )
      ),
      bslib::accordion_panel(
        title = "Temporary crop sales",
        value = "temp_crop_sales",
        mod_4_quality_1_setup_2_data_temp_crop_sales_ui(
          id = ns("4_quality_1_setup_2_data_temp_crop_sales_1")
        )
      ),
      bslib::accordion_panel(
        title = "Permanent crop harvest",
        value = "perm_crop_harvest",
        mod_4_quality_1_setup_2_data_perm_crop_harvest_ui(
          id = ns("4_quality_1_setup_2_data_perm_crop_harvest_1")
        )
      ),
      bslib::accordion_panel(
        title = "Permanent crop sales",
        value = "perm_crop_sales",
        mod_4_quality_1_setup_2_data_perm_crop_sales_ui(
          id = ns("4_quality_1_setup_2_data_perm_crop_sales_1")
        )
      ),
      bslib::accordion_panel(
        title = "Livestock owership",
        value = "livestock_owership",
        mod_4_quality_1_setup_2_data_livestock_owership_ui(
          id = ns("4_quality_1_setup_2_data_livestock_owership_1")
        )
      ),
      bslib::accordion_panel(
        title = "Cow displacement",
        value = "cow_displacement",
        mod_4_quality_1_setup_2_data_cow_displacement_ui(
          id = ns("4_quality_1_setup_2_data_cow_displacement_1")
        )
      ),
      bslib::accordion_panel(
        title = "Hen displacement",
        value = "hen_displacement",
        mod_4_quality_1_setup_2_data_hen_displacement_ui(
          id = ns("4_quality_1_setup_2_data_hen_displacement_1")
        )
      ),
      bslib::accordion_panel(
        title = "Sales and production of milk",
        value = "milk_prod_sales",
        mod_4_quality_1_setup_2_data_milk_prod_sales_ui(
          id = ns("4_quality_1_setup_2_data_milk_prod_sales_1")
        )
      ),
      bslib::accordion_panel(
        title = "Sales and production of eggs",
        value = "egg_prod_sales",
        mod_4_quality_1_setup_2_data_egg_prod_sales_ui(
          id = ns("4_quality_1_setup_2_data_egg_prod_sales_1")
        )
      ),
      bslib::accordion_panel(
        title = "Production and sales of fisheries products",
        value = "fisheries_prod_sales",
        mod_4_quality_1_setup_2_data_fisheries_prod_sales_ui(
          id = ns("4_quality_1_setup_2_data_fisheries_prod_sales_1")
        )
      ),
      bslib::accordion_panel(
        title = "Production and sales of aquaculture products",
        value = "aquaculture_prod_sales",
        mod_4_quality_1_setup_2_data_aquaculture_prod_sales_ui(
          id = ns("4_quality_1_setup_2_data_aquaculture_prod_sales_1")
        )
      ),
      bslib::accordion_panel(
        title = "Production and sales of forestry products",
        value = "forestry_prod_sales",
        mod_4_quality_1_setup_2_data_forestry_prod_sales_ui(
          id = ns("4_quality_1_setup_2_data_forestry_prod_sales_1")
        )
      ),
      bslib::accordion_panel(
        title = "Processing of crop production",
        value = "process_crop_prod",
        mod_4_quality_1_setup_2_data_process_crop_prod_ui(
          id = ns("4_quality_1_setup_2_data_process_crop_prod_1")
        )
      ),
      bslib::accordion_panel(
        title = "Crop labor",
        value = "crop_labor",
        mod_4_quality_1_setup_2_data_crop_labor_ui(
          id = ns("4_quality_1_setup_2_data_crop_labor_1")
        )
      ),
      bslib::accordion_panel(
        title = "Livestock labor",
        value = "livestock_labor_tbl",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Fisheries labor",
        value = "fisheries_labor",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Aquaculture labor",
        value = "aquaculture_labor",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Forestry labor",
        value = "forestry_labor",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Income sources",
        value = "income_sources",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      )

    ),

    # save button to close accordion panel
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 4_quality_1_setup_2_data Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_2_data_server <- function(id, parent, r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # collect table names
    # ==========================================================================

    accordion_panel_values <- c(
      # post-planting tables
      "parcels_per_hhold",
      "parcel_gps",
      "plots_per_parcel",
      "plot_use",
      "plot_gps",
      "crops_per_plot",
      "crop_types",
      # post-harvest tables
      "temp_crop_harvest",
      "temp_crop_sales",
      "perm_crop_harvest",
      "perm_crop_sales",
      "livestock_owership",
      "cow_displacement",
      "hen_displacement",
      "milk_prod_sales",
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

    # ==========================================================================
    # initialize page
    # ==========================================================================

    purrr::walk(
      .x = accordion_panel_values,
      .f = ~ set_table_accordion_state(
        r6 = r6,
        tbl_id = .x
      )
    )

    # ==========================================================================
    # update upon saving table selections
    # ==========================================================================

    gargoyle::on("save_table_selections", {

      purrr::walk(
        .x = accordion_panel_values,
        .f = ~ set_table_accordion_state(
          r6 = r6,
          tbl_id = .x
        )
      )

    })

    # ==========================================================================
    # load child module
    # ==========================================================================

    mod_4_quality_1_setup_2_data_parcels_per_hhold_server(
      id = "4_quality_1_setup_2_data_parcels_per_hhold_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_parcel_gps_server(
      id = "4_quality_1_setup_2_data_parcel_gps_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_plots_per_parcel_server(
      id = "4_quality_1_setup_2_data_plots_per_parcel_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_plot_use_server(
      id = "4_quality_1_setup_2_data_plot_use_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_plot_gps_server(
      id = "4_quality_1_setup_2_data_plot_gps_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_crops_per_plot_server(
      id = "4_quality_1_setup_2_data_crops_per_plot_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_crop_types_server(
      id = "4_quality_1_setup_2_data_crop_types_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_temp_crop_harvest_server(
      id = "4_quality_1_setup_2_data_temp_crop_harvest_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_temp_crop_sales_server(
      id = "4_quality_1_setup_2_data_temp_crop_sales_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_perm_crop_harvest_server(
      id = "4_quality_1_setup_2_data_perm_crop_harvest_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_perm_crop_sales_server(
      id = "4_quality_1_setup_2_data_perm_crop_sales_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_livestock_owership_server(
      id = "4_quality_1_setup_2_data_livestock_owership_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_cow_displacement_server(
      id = "4_quality_1_setup_2_data_cow_displacement_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_hen_displacement_server(
      id = "4_quality_1_setup_2_data_hen_displacement_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_milk_prod_sales_server(
      id = "4_quality_1_setup_2_data_milk_prod_sales_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_egg_prod_sales_server(
      id = "4_quality_1_setup_2_data_egg_prod_sales_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_fisheries_prod_sales_server(
      id = "4_quality_1_setup_2_data_fisheries_prod_sales_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_aquaculture_prod_sales_server(
      id = "4_quality_1_setup_2_data_aquaculture_prod_sales_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_forestry_prod_sales_server(
      id = "4_quality_1_setup_2_data_forestry_prod_sales_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_process_crop_prod_server(
      id = "4_quality_1_setup_2_data_process_crop_prod_1",
      parent = session,
      r6 = r6
    )
    mod_4_quality_1_setup_2_data_crop_labor_server(
      id = "4_quality_1_setup_2_data_crop_labor_1",
      parent = session,
      r6 = r6
    )

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_ui("4_quality_1_setup_2_data_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_server("4_quality_1_setup_2_data_1")
