#' 4_quality_1_setup_1_tables_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_quality_1_setup_1_tables_details_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(

    # provides a reference point for UI insertion
    # since `shiny::insertUI()` inserts relative to a selected element
    htmltools::div(
      id = ns("insert_reference")
    )

  )
}

#' 4_quality_1_setup_1_tables_details Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_1_tables_details_server <- function(
  id, parent, r6,
  show,
  tbl_id,
  tbl_desc
) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    if (show == TRUE) {

      # ========================================================================
      # initialize page
      # ========================================================================

      # ------------------------------------------------------------------------
      # draw UI and update it to stored value, if relevant
      # ------------------------------------------------------------------------

      shiny::insertUI(
        # construct selector of element relative to which to inject
        # concatenate `#` and namespaced element ID
        selector = paste0(
          "#", ns("insert_reference")
        ),
        where = "afterEnd",
        ui = shiny::tagList(
          bslib::layout_columns(
            bslib::input_switch(
              id = ns("use"),
              label = glue::glue("Use {tbl_desc} table"),
              # set the switch to the value stored in R6
              # if no value found, default to `FALSE`
              value = ifelse(
                test = !is.null(r6[[glue::glue("use_{tbl_id}")]]),
                yes = r6[[glue::glue("use_{tbl_id}")]],
                no = FALSE
              )
            ),
            shiny::actionButton(
              inputId = ns("describe"),
              label = "Describe"
            ),
            # since the area is 75% of Bootstrap's 12 points
            # allocate within the 9 points available for the sidebar
            col_widths = c(7, 2)
          )
        )
      )

      # ========================================================================
      # react to toggle
      # ========================================================================

      shiny::observeEvent(input$use, {

        # filter out events related to initializing/inserting the UI
        # require that the input be non-NULL to continue
        shiny::req(input$use)

        # store user inputs in R6
        r6[[glue::glue("use_{tbl_id}")]] <- input$use

        # write R6 to disk
        r6$write()

      })

      # ========================================================================
      # react to details button
      # ========================================================================

      shiny::observeEvent(input$describe, {

        shiny::showModal(
          shiny::modalDialog(
            title = stringr::str_to_sentence(tbl_desc),
            "some content",
            footer = shiny::modalButton("Close")
          )
        )

      })

    }

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_1_tables_details_ui("4_quality_1_setup_1_tables_details_1")
    
## To be copied in the server
# mod_4_quality_1_setup_1_tables_details_server("4_quality_1_setup_1_tables_details_1")
