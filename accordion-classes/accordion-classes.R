library(shiny)
library(bslib)

## In this dummy app, we have three accordions. Accordion 3 consists of nested accordions.
## We are trying to figure out a way to style the accordions differently, depending on whether they are nested or
## not.
## Please see the css code included in app_ui

mod_acc_UI <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::accordion(
      open = FALSE,
      id = ns("main_accordion"),
      icon = fontawesome::fa(name = "cogs"),
      ## Accordion 1
      bslib::accordion_panel(
        title = "Accordion 1",
        value = "accordion_1"
      ),
      ## Accordion 2
      bslib::accordion_panel(
        title = "Accordion 2",
        value = "accordion_2"
      ),
      ## Accordion 3 is a parent accordion that contains 3 nested accordions
      bslib::accordion_panel(
        title = "Accordion 3",
        value = "accordion_3",
        bslib::accordion(
          id = ns("accordion3_with_nested_accordions"),
          bslib::accordion_panel(
            title = "Nested accordion 1",
            value = "nested_accordion_1"
          ),
          bslib::accordion_panel(
            title = "Nested accordion 2",
            value = "nested_accordion_2"
          ),
          bslib::accordion_panel(
            title = "Nested accordion 3",
            value = "nested_accordion_3"
          )
        )
      )
    )
  )
}

mod_acc_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
    }
  )
}


app_ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        /* Parent accordions */

        /* Accordions should have a green background with white text when collapsed*/
          .accordion-button.collapsed{
            color: white;
            background-color: green;
            }

        /*Accordions should have white background with green text when not collapsed*/
          .accordion-button:not(.collapsed) {
            color: green;
            background-color: white;
          }

      /*Nested accordions */

      /* Accordions should have a maroon background with white text when collapsed*/
      .accordion-button.collapsed{
        color: white;
        background-color: maroon;
        }

      /*Accordions should have white background with maroon text when not collapsed*/
    .accordion-button:not(.collapsed) {
      color: maroon;
      background-color: white;

    }"
      )
    )
  ),
  theme = bslib::bs_theme(version = 5),
  mod_acc_UI("test")
)

app_server <- function(input, output, session) {
  mod_acc_Server("test")
}

shinyApp(
  ui = app_ui,
  server = app_server
)
