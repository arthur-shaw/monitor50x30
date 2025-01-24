#' 3_complete_1_setup_1_domains UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_1_domains_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::selectizeInput(
      inputId = ns("domain_vars"),
      label = paste0(
        "Which variable(s) describe the domains of the survey design",
        " (e.g., strata, region, urban/rural)?"
      ),
      choices = NULL,
      selected = NULL,
      multiple = TRUE
    ),
    shiny::tags$p("How many observations are expected per domain?"),
    rhandsontable::rHandsontableOutput(ns("n_per_domain")),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 3_complete_1_setup_1_domains Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_1_domains_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    # compute the choices based on data
    # initialize as the values stored in R6
    # if never set, the value will be null
    domain_vars <- shiny::reactiveValues(choices = r6$domain_var_choices)

    # when data are downloaded, compute the choices and update the choices
    gargoyle::on("download_data", {

      domain_vars$choices <- make_vars_options(
        path = fs::path(r6$dirs$qnr, "qnr_vars.rds"),
        var_types = "SingleQuestion"
      )

      shiny::updateSelectizeInput(
        inputId = "domain_vars",
        choices = domain_vars$choices
      )

    })

    # otherwise, load past selections from R6 and computed choices
    if (!is.null(r6$domain_details_provided)) {

      shiny::updateSelectizeInput(
        inputId = "domain_vars",
        choices = r6$domain_var_choices,
        selected = r6$domain_vars_selected
      )

    }

    # ==========================================================================
    # react to domain variable selection(s)
    # ==========================================================================

    output$n_per_domain <- rhandsontable::renderRHandsontable({

      if (is.null(input$domain_vars)) {

        NULL

      } else {

        # extract the variable column names from the selections
        domain_df_col_names <- stringr::str_extract(
          string = input$domain_vars,
          pattern = "(?<=\\{).+(?=\\})"
        )

        # provide a data frame of observations per domains selected
        # case 1: no domain details provided
        if (is.null(r6$domain_details_provided)) {

          # create a table of all possible values of selected variables
          obs_per_domain_df <- create_domain_var_val_df(
            path = fs::path(r6$dirs$qnr, "qnr_full.rds"),
            domain_vars = domain_df_col_names
          )

        # case 2: domain details previously provided
        } else if (!is.null(r6$domain_details_provided)) {

          # load previous values
          obs_per_domain_df <- r6$obs_per_domain

        }

        # compose interactive display table
        rhandsontable::rhandsontable(data = obs_per_domain_df) |>
          # dictate read and write access of columns
          # read only: values of selected variables
          # write:  `Obs`
          rhandsontable::hot_col(
            col = c(1:(ncol(obs_per_domain_df) - 1)),
            readOnly = TRUE
          ) |>
          # format inputs as whole nubmers
          rhandsontable::hot_col("Obs", format = "0") |>
          # highlight current row in focus
          rhandsontable::hot_table(highlightRow = TRUE) |>
          # allow column sorting and 
          rhandsontable::hot_cols(columnSorting = TRUE, colWidths = 110) |>
          # require inputs to be non-negative
          rhandsontable::hot_cols(
            validator = "
              function (value, callback) {
                setTimeout(function(){
                  callback(value >= 0 );
                }, 1000)
              }",
            allowInvalid = FALSE)

      }

    })

    # ==========================================================================
    # react to save
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # react to whether or not all fields are filled
      # if not, provide user feedback
      if (is.null(input$domain_vars)) {

        shinyFeedback::showToast(
          type = "error",
          message = "Domain variables not selected."
        )

      } else if  (is.null(input$n_per_domain)) {

        shinyFeedback::showToast(
          type = "error",
          message = "Observations per domain not provided."
        )

      # if all fields are filled
      # first, validate their content
      # then, store user inputs
      } else if (
        !is.null(input$domain_vars) &
        !is.null(input$n_per_domain)
      ) {

        # extract data frame from table
        n_per_domain <- rhandsontable::hot_to_r(input$n_per_domain)

        # check that an observation is provided for every domain
        all_domain_obs_provided <- all(!is.na(n_per_domain$Obs))

        if (all_domain_obs_provided == FALSE) {

          shinyFeedback::showToast(
            type = "error",
            message = "Observation count missing for at least one domain."
          )

        } else if (all_domain_obs_provided == TRUE) {

          # write user inputs to R6
          r6$domain_var_choices <- domain_vars$choices
          r6$domain_vars_selected <- input$domain_vars
          r6$obs_per_domain <- rhandsontable::hot_to_r(input$n_per_domain)
          r6$domain_details_provided <- TRUE

          # write R6 to disk
          r6$write()

          # write data to disk for use by report
          saveRDS(
            object = r6$obs_per_domain,
            file = fs::path(r6$dirs$obs_per_domain, "obs_per_domain.rds")
          )

          # signal that domain variables saved
          gargoyle::trigger("save_domains")

        }

      }

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_1_domains_ui("3_complete_1_setup_1_domains_1")
    
## To be copied in the server
# mod_3_complete_1_setup_1_domains_server("3_complete_1_setup_1_domains_1")
