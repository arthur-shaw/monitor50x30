#' Persistant storage of user inputs for Shiny
#' 
#' @description
#' Stores data in R6 object for intra-module communication and provides
#' methods for reading and writing fields to disk.
#'
#' @field app_dir Character, atomic. Path to Shiny app's user data directory
#' @field server Character, atomic. URL of SuSo server.
#' @field workspace Character, atomic. Name of SuSo workspace.
#' @field user Character, atomic. API user name.
#' @field password Character, atomic. API user's password.
#' @field suso_creds_provided Boolean, atomic. Whether user has provided valid
#' credentials for their Survey Solutions server
#' @field qnr_string Character, atomic. Questionnaire search string.
#' search string.
#' @field matching_qnr_tbl Data frame. Questionnaires that match `qnr_string`
#' @field qnrs_identified Boolean. Whether questionnaires identified with query
#' string.
#' @field qnr_selected_index Integer. Row number of the selected questionnaire.
#' @field qnr_selected_suso_id Character. GUID for the selected template
#' @field qnr_selected_suso_version Numeric. Version number for selected qnr.
#' @field qnr_selected Boolean. Whether main questionnaire selected.
#' @field qnr_templates Character vector. Name of template(s) used.
#' @field qnr_extensions Character vector. Name of extension(s) used.
#' @field template_details_provided Boolean. Whether template details provided.
#' @field svy_num_visits Character. Total number of visits to the household
#' captured as a character.
#' @field svy_current_visit Character. Current survey visit.
#' @field svy_start_date Character. Start date of survey data collection.
#' @field visit_details_provided Boolean. Whether all visit details provided.
#' @field core_settings_saved Boolean. Whether all core settings saved.
#' @field data_downloaded Boolean. Whether data downloaded.
#' @field selected_action Character. Next app action selected.
#' @field domain_var_choices Character vector. Domain variable choices to show.
#' @field domain_vars_selected Character vector. Domain variable(s) selected
#' @field obs_per_domain Data frame. Number of observations per domain.
#' @field domain_details_provided Boolean. Whether domain details provided.
#' @field n_clusters Numeric. Number of clusters in the survey sample.
#' @field n_per_cluster Numeric. Number of observations per cluster in the
#' survey design.
#' @field cluster_quantity_details_provided Boolean. Whether cluster number and
#' observations per cluster provided.
#'
#' @importFrom R6 R6Class
#' @importFrom fs path
#' @importFrom tibble tibble

r6 <- R6::R6Class(
  classname = "r6",
  public = list(

    # ==========================================================================
    # Fields
    # ==========================================================================

    app_dir = NULL,
    # server credentials
    server = NULL,
    workspace = NULL,
    user = NULL,
    password = NULL,
    suso_creds_provided = NULL,
    # questionanires identified
    qnr_string = NULL,
    matching_qnr_tbl = NULL,
    qnrs_identified = NULL,
    # questionnaire selected
    qnr_selected_index = NULL,
    qnr_selected_suso_id = NULL,
    qnr_selected_suso_version = NULL,
    qnr_selected = NULL,
    # template details
    qnr_templates = NULL,
    qnr_extensions = NULL,
    template_details_provided = NULL,
    # survey visit details
    svy_num_visits = NULL,
    svy_current_visit = NULL,
    svy_start_date = NULL,
    visit_details_provided = NULL,
    # survey settings checkpoint
    core_settings_saved = NULL,
    # data download
    data_downloaded = NULL,
    selected_action = NULL,
    # domains
    domain_var_choices = NULL,
    domain_vars_selected = NULL,
    obs_per_domain = NULL,
    domain_details_provided = NULL,
    # clusters
    n_clusters = NULL,
    n_per_cluster = NULL,
    cluster_quantity_details_provided = NULL,

    # ==========================================================================
    # Methods
    # ==========================================================================

    #' @description
    #' Read past R6 values from disk
    #'
    #' Perform the following tasks:
    #'
    #' - Read RDS on disk
    #' - Populate R6 fields with values from RDS
    #'
    #' @param path Character. Path to the RDS file containing R6 values.
    read = function(
      path = fs::path(self[["app_dir"]], "saved_params.rds")
    ) {

      # read setup file from disk
      input_file <- readRDS(path)

      # collect names of fields in setup file
      fields <- names(input_file)

      # populate the R6 object with the corresponding setup file value
      # data frame fields need to be extracted from a list
      # "scalar" fields can be extracted directly
      for (field in fields) {
        field_type <- typeof(input_file[[field]])
        if (field_type == "list") {
          self[[field]] <- input_file[[field]][[1]]
        } else {
          self[[field]] <- input_file[[field]]
        }
      }

    },

    #' @description
    #' Update fields in the R6 object
    #'
    #' Allows the app to update several fields in one call
    #'
    #' Each argument corresponds to a same-named field in the R6 object.
    #'
    #' Each non-NULL value provided will update the corresponding R6 field
    #'
    #' @param app_dir Character, atomic. Path to Shiny app's user data directory
    #' @param server Character, atomic. URL of SuSo server.
    #' @param workspace Character, atomic. Name of SuSo workspace.
    #' @param user Character, atomic. API user name.
    #' @param password Character, atomic. API user's password.
    #' @param suso_creds_provided Boolean, atomic. Whether user has provided
    #' valid credentials for their Survey Solutions server
    #' @param qnr_string Character, atomic. Questionnaire search string.
    #' @param matching_qnr_tbl Data frame. Questionnaires that
    #' match `qnr_string`
    #' @param qnrs_identified Boolean. Whether questionnaires identified
    #' with query
    #' @param qnr_selected_index Integer. Row number of the selected
    #' questionnaire.
    #' @param qnr_selected_suso_id Character. GUID for the selected template
    #' @param qnr_selected_suso_version Numeric. Version number for
    #' selected qnr.
    #' @param qnr_selected Boolean. Whether main questionnaire selected.
    #' @param qnr_templates Character vector. Name of template(s) used.
    #' @param qnr_extensions Character vector. Name of extension(s) used.
    #' @param template_details_provided Boolean. Whether template details
    #' provided.
    #' @param svy_num_visits Character. Total number of visits to the household
    #' captured as a character.
    #' @param svy_current_visit Character. Current survey visit.
    #' @param svy_start_date Character. Start date of survey data collection.
    #' @param visit_details_provided Boolean. Whether all visit details
    #' provided.
    #' @param core_settings_saved Boolean. Whether all core settings saved.
    #' @param data_downloaded Boolean. Whether data downloaded.
    #' @param domain_var_choices Character vector. Domain variable choices to
    #' show.
    #' @param domain_vars_selected Character vector. Domain var(s) selected.
    #' @param obs_per_domain Data frame. Number of observations per domain.
    #' @param domain_details_provided Boolean. Whether domain details provided.
    #' @param n_clusters Numeric. Number of clusters in the survey sample.
    #' @param n_per_cluster Numeric. Number of observations per cluster in the
    #' survey design.
    #' @param cluster_quantity_details_provided Boolean. Whether cluster number
    #' and observations per cluster provided.
    #'
    #' @noRd
    update = function(
      app_dir = NULL,
      # server credentials
      server = NULL,
      workspace = NULL,
      user = NULL,
      password = NULL,
      suso_creds_provided = NULL,
      # questionanires identified
      qnr_string = NULL,
      matching_qnr_tbl = NULL,
      qnrs_identified = NULL,
      # questionnaire selected
      qnr_selected_index = NULL,
      qnr_selected_suso_id = NULL,
      qnr_selected_suso_version = NULL,
      qnr_selected = NULL,
      # questionnaire template details
      qnr_templates = NULL,
      qnr_extensions = NULL,
      template_details_provided = NULL,
      # survey visit details
      svy_num_visits = NULL,
      svy_current_visit = NULL,
      svy_start_date = NULL,
      visit_details_provided = NULL,
      # survey settings checkpoint
      core_settings_saved = NULL,
      # data download
      data_downloaded = NULL,
      # domains
      domain_var_choices = NULL,
      domain_vars_selected = NULL,
      obs_per_domain = NULL,
      domain_details_provided = NULL,
      # clusters
      n_clusters = NULL,
      n_per_cluster = NULL,
      cluster_quantity_details_provided = NULL

    ) {

      # get function arguments as character vector
      args <- formalArgs(self[["update"]])

      # loop over each argument
      num_args <- length(args)
      for (i in seq(from = 1, to = num_args)) {

        # when the argument is non-null
        # assign user-provided value to the same-named R6 field
        if (!is.null(base::get(args[i]))) {

          self[[args[i]]] <- base::get(args[i])

        }

      }

    },

    #' @description
    #' Write R6 container to disk as RDS file
    #'
    #' Write all R6 fields to a single RDS file, from which they can be
    #' "restored" with the `read()` method above
    #'
    #' @param path Character. Path where RDS files should be written
    write = function(
      path = fs::path(self[["app_dir"]], "saved_params.rds")
    ) {

      # collect names of the fields to write

      # data frame fields
      df_fields <- c(
        "matching_qnr_tbl",
        "obs_per_domain"
      )

      # vector fields
      vctr_fields <- c(
        "qnr_templates",
        "qnr_extensions",
        "domain_var_choices",
        "domain_vars_selected"
      )

      # other fields not to write
      other_fields <- c("have_setup")

      # "scalar" fields
      # introspect to obtain vector fields and methods
      fields <-  names(self)

      # remove system components and methods
      fields <- fields[
        ! fields %in% c(
          # system components
          ".__enclos_env__", "clone",
          # methods
          "write", "update", "read",
          # omitting data frames and vectors
          df_fields, vctr_fields,
          # omitting other fields
          other_fields
        )
      ]

      # put fields in parameter data frame
      # create empty 1-row data frame
      df <- tibble::tibble(.rows = 1)

      # iternatively populate it with the value of all non-df fields
      for (field in fields) {
        df[[field]] <- self[[field]]
      }

      # change the order since the data is being saved in reverse order
      # from the latest field saved to the earliest

      order <- seq(ncol(df), 1, -1)
      df <- df[, order]

      # put data frames and vectors in parameter data frame
      non_atomic_fields <- c(df_fields, vctr_fields)

      for (non_atomic_field in non_atomic_fields) {
        df[[non_atomic_field]] <- list(self[[non_atomic_field]])
      }

      # write data frame to disk
      saveRDS(
        object = df,
        file = path
      )

    }

  )
)
