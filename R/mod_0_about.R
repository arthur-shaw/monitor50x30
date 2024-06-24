#' 0_about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_0_about_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::p("About")
 
  )
}
    
#' 0_about Server Functions
#'
#' @noRd 
mod_0_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_0_about_ui("0_about_1")
    
## To be copied in the server
# mod_0_about_server("0_about_1")
