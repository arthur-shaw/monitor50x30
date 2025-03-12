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
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Plots per parcel",
        value = "plots_per_parcel",
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Plot use",
        value = "plot_use",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Plot GPS measurement",
        value = "plot_gps",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Crops per plot",
        value = "crops_per_plot",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Crop types",
        value = "crop_types",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),

      # ------------------------------------------------------------------------
      # post-harvest tables
      # ------------------------------------------------------------------------

      bslib::accordion_panel(
        title = "Temporary crop harvest",
        value = "temp_crop_harvest",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Temporary crop sales",
        value = "temp_crop_sales",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Permanent crop harvest",
        value = "perm_crop_harvest",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Permanent crop sales",
        value = "perm_crop_sales",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Livestock owership",
        value = "livestock_owership",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Cow displacement",
        value = "cow_displacement",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Hen displacement",
        value = "hen_displacement",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Sales and production of milk",
        value = "milk_prod_sales",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Sales and production of eggs",
        value = "egg_prod_sales",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Production and sales of fisheries products",
        value = "fisheries_prod_sales",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Production and sales of aquaculture products",
        value = "aquaculture_prod_sales",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Production and sales of forestry products",
        value = "forestry_prod_sales",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Processing of crop production",
        value = "process_crop_prod",
        # TODO: insert UI function
        shiny::tags$p("TODO")
      ),
      bslib::accordion_panel(
        title = "Crop labor",
        value = "crop_labor",
        # TODO: insert UI function
        shiny::tags$p("TODO")
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

    gargoyle::on("save_tables", {

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

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_2_data_ui("4_quality_1_setup_2_data_1")
    
## To be copied in the server
# mod_4_quality_1_setup_2_data_server("4_quality_1_setup_2_data_1")
