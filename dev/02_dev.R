# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

# install development dependencies
usethis::use_dev_package(package = "gargoyle", remote = "github::ColinFay/gargoyle")
usethis::use_dev_package(package = "susoapi", remote = "github::arthur-shaw/susoapi")
usethis::use_dev_package(package = "susoflows", remote = "github::arthur-shaw/susoflows")
usethis::use_dev_package(package = "susometa", remote = "lsms-worldbank/susometa")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "0_about")

# setup
golem::add_module(name = "1_setup")
golem::add_module(name = "1_setup_1_load_file")
golem::add_module(name = "1_setup_2_suso_creds")
golem::add_module(name = "1_setup_3_suso_qnr")
golem::add_module(name = "1_setup_3_suso_qnr_1_identify")
golem::add_module(name = "1_setup_3_suso_qnr_2_select")
golem::add_module(name = "1_setup_4_template")
golem::add_module(name = "1_setup_5_visit")
golem::add_module(name = "1_setup_6_save")

# data
golem::add_module(name = "2_data")

# completeness
golem::add_module(name = "3_complete")
golem::add_module(name = "3_complete_1_setup")
golem::add_module(name = "3_complete_1_setup_1_domains")
golem::add_module(name = "3_complete_1_setup_2_clusters")
golem::add_module(name = "3_complete_1_setup_2_clusters_1_quantify")
golem::add_module(name = "3_complete_1_setup_2_clusters_2_computer_id")
golem::add_module(name = "3_complete_1_setup_2_clusters_3_manager_id")
golem::add_module(name = "3_complete_1_setup_2_clusters_3_manager_id_1_select")
golem::add_module(name = "3_complete_1_setup_2_clusters_3_manager_id_2_order")
golem::add_module(name = "3_complete_1_setup_2_clusters_3_manager_id_3_compose")
golem::add_module(name = "3_complete_1_setup_3_team_workload")
golem::add_module(name = "3_complete_1_setup_4_save")
golem::add_module(name = "3_complete_2_report")


golem::add_module(name = "4_quality")
golem::add_module(name = "4_quality_1_setup")
golem::add_module(name = "4_quality_1_setup_1_tables")
golem::add_module(name = "4_quality_1_setup_1_tables_details")
golem::add_module(name = "4_quality_1_setup_2_data")
golem::add_module(name = "4_quality_1_setup_3_interviews")
golem::add_module(name = "4_quality_2_report")

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers")
golem::add_fct(
  name = "download",
  module = "2_data"
)
golem::add_fct(
  name = "metadata",
  module = "3_complete_1_setup_1_domains"
)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("monitor50x30")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
