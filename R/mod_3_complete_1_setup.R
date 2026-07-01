#' 3_complete_1_setup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_complete_1_setup_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(

    bslib::accordion(
      id = ns("setup"),
      bslib::accordion_panel(
        title = label_tooltip(
          lbl = "Domains",
          desc = paste(
            "The app plans to measure progress towards completion",
            "by survey domain.",
            "To do so, it needs to know two things:",
            "first, the variables that define a survey domain;",
            "second, the number of observations expected per domain."
          )
        ),
        value = "domains",
        mod_3_complete_1_setup_1_domains_ui(
          ns("3_complete_1_setup_1_domains_1")
        )
      ),
      bslib::accordion_panel(
        title = label_tooltip(
          lbl = "Clusters",
          desc = paste(
            "The app plans to check that all observations expected per cluster",
            "have been received.",
            "To do so, it needs information on the size of each cluster",
            "as well as the variables that define it, for a computer, and",
            "those that help identify it, for a human in a report."
          )
        ),
        value = "clusters",
        mod_3_complete_1_setup_2_clusters_ui(
          ns("3_complete_1_setup_2_clusters_1")
        )
      ),
      bslib::accordion_panel(
        title = label_tooltip(
          lbl = "Workloads",
          desc = paste(
            "The app plans to measure progress per survey team.",
            "To calculate the % complete, the app needs to know",
            "how many observations to expect per team."
          )
        ),
        value = "workloads",
        mod_3_complete_1_setup_3_team_workload_ui(
          ns("3_complete_1_setup_3_team_workload_1")
        )
      ),
      bslib::accordion_panel(
        title = "Save settings",
        value = "save_setup",
        mod_3_complete_1_setup_4_save_ui(
          ns("3_complete_1_setup_4_save_1")
        )
      )
    )
 
  )
}
    
#' 3_complete_1_setup Server Functions
#'
#' @noRd 
mod_3_complete_1_setup_server <- function(id, parent, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ==========================================================================
    # load server logic of child modules
    # ==========================================================================

    # note: parent param passes the session down to descendent modules
    mod_3_complete_1_setup_1_domains_server(
      id = "3_complete_1_setup_1_domains_1",
      parent = session,
      r6 = r6
    )
    mod_3_complete_1_setup_2_clusters_server(
      id = "3_complete_1_setup_2_clusters_1",
      parent = session,
      r6 = r6
    )
    mod_3_complete_1_setup_3_team_workload_server(
      id = "3_complete_1_setup_3_team_workload_1",
      parent = session,
      r6 = r6
    )
    mod_3_complete_1_setup_4_save_server(
      id = "3_complete_1_setup_4_save_1",
      parent = session,
      r6 = r6
    )

    # ==========================================================================
    # move focus from one top-level accordion to the next
    # ==========================================================================

    # from domains to clusters
    gargoyle::on("save_domains", {

      # close domains
      bslib::accordion_panel_close(
        id = "setup",
        value = "domains"
      )

      # open clusters
      bslib::accordion_panel_open(
        id = "setup",
        value = "clusters"
      )

    })

    # from clusters to workloads
    gargoyle::on("save_manager_compose_clusters", {

      # open workloads
      bslib::accordion_panel_open(
        id = "setup",
        value = "workloads"
      )

    })

    # from workloads to save
    gargoyle::on("save_workloads", {

      bslib::accordion_panel_close(
        id = "setup",
        value = "workloads"
      )

      bslib::accordion_panel_open(
        id = "setup",
        value = "save_setup"

      )

    })

  })
}
    
## To be copied in the UI
# mod_3_complete_1_setup_ui("3_complete_1_setup_1")
    
## To be copied in the server
# mod_3_complete_1_setup_server("3_complete_1_setup_1")
