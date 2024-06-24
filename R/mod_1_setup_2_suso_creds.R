#' 1_setup_2_suso_creds UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_setup_2_suso_creds_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    shiny::textInput(
      inputId = ns("server"),
      label = shiny::tags$p(
        "Server URL",
        bsicons::bs_icon("browser-chrome")
      )
    ),
    shiny::textInput(
      inputId = ns("workspace"),
      label = shiny::tags$p(
        "Workspace",
        bsicons::bs_icon("diagram-3-fill")
      ),
    ),
    shiny::textInput(
      inputId = ns("user"),
      label = shiny::tags$p(
        "API user name",
        fontawesome::fa(name = "user-shield")
      )
    ),
    shiny::passwordInput(
      inputId = ns("password"),
      label = shiny::tags$p(
        "API user's password",
        bsicons::bs_icon("unlock-fill")
      )
    ),
    shiny::actionButton(
      inputId = ns("save"),
      label = "Save"
    )

  )
}
    
#' 1_setup_2_suso_creds Server Functions
#'
#' @noRd 
mod_1_setup_2_suso_creds_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # initialize page
    # ==========================================================================

    if (!is.null(r6$suso_creds_provided)) {

      if (r6$suso_creds_provided == TRUE) {

        shiny::updateTextInput(
          inputId = "server",
          value = r6$server
        )
        shiny::updateTextInput(
          inputId = "workspace",
          value = r6$workspace
        )
        shiny::updateTextInput(
          inputId = "user",
          value = r6$user
        )
        shiny::updateTextInput(
          inputId = "password",
          value = r6$password
        )

      }

    }

    # ==========================================================================
    # react to save button
    # ==========================================================================

    shiny::observeEvent(input$save, {

      # check all inputs provided
      all_creds_provided <- shiny::reactive({
        !all(
          input$server    == "",
          input$workspace == "",
          input$user      == "",
          input$password  == ""
        )
      })

      if (all_creds_provided() == FALSE) {

        shinyFeedback::showToast(
          type = "error",
          title = "Missing input",
          message = "One or more component of the server credentials is missing"
        )

      } else if (all_creds_provided() == TRUE) {

        # set credentials
        susoapi::set_credentials(
          server = input$server,
          workspace = input$workspace,
          user = input$user,
          password = input$password
        )

        # check credentials are valid
        creds_ok <- shiny::reactive({
          susoapi::check_credentials(verbose = TRUE)
        })

        # react to validity of credentials
        if (creds_ok()) {

          # write credentials to R6
          r6$server               <- input$server
          r6$workspace            <- input$workspace
          r6$user                 <- input$user
          r6$password             <- input$password
          r6$suso_creds_provided  <- TRUE

          # save credentials to local storage
          r6$write()

          # send signal that credentials saved
          gargoyle::trigger("save_creds")

        } else {

          # inform the user credentials invalid
          shinyFeedback::showToast(
            type = "error",
            message = "Server credentials invalid"
          )

        }

      }

    })

  })
}
    
## To be copied in the UI
# mod_1_setup_2_suso_creds_ui("1_setup_2_suso_creds_1")
    
## To be copied in the server
# mod_1_setup_2_suso_creds_server("1_setup_2_suso_creds_1")
