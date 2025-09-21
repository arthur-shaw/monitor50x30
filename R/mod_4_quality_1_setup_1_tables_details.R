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

    # create a div containing the UI
    # so that the container and content can be shown/hidden
    # as a function of the questionnaire details provided
    htmltools::div(
      id = ns("tbl_toggle"),
      bslib::layout_columns(
        bslib::input_switch(
          id = ns("use"),
          label = "",
          value = FALSE,
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
}

#' 4_quality_1_setup_1_tables_details Server Functions
#'
#' @noRd 
mod_4_quality_1_setup_1_tables_details_server <- function(
  id, parent, r6,
  tbl_id,
  tbl_desc
) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # ========================================================================
    # initialize page: display stored values
    # ========================================================================

    # compose names of R6 field based on `tbl_id`
    # avoids recomputation of the same in a few places
    r6_field_name <- glue::glue("use_{tbl_id}")

    # extract value of switch to the value stored in R6
    # if no value found, default to `FALSE`
    switch_value <- ifelse(
      test = !is.null(r6[[r6_field_name]]),
      yes = r6[[r6_field_name]],
      no = FALSE
    )

    # update without triggering an event for listeners
    shiny::freezeReactiveValue(input, "use")
    bslib::update_switch(
      id = "use",
      label = glue::glue("{tbl_desc} table"),
      value = switch_value
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
          shiny::tags$img(
            src = glue::glue("www/{tbl_id}.png"),
            style = "max-width: 90%; height: auto;"
          ),
          size = "xl",
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )

    })

  })
}
    
## To be copied in the UI
# mod_4_quality_1_setup_1_tables_details_ui("4_quality_1_setup_1_tables_details_1")
    
## To be copied in the server
# mod_4_quality_1_setup_1_tables_details_server("4_quality_1_setup_1_tables_details_1")
