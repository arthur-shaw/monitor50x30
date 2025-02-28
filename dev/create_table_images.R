# ==============================================================================
# Define table rows and functions
# ==============================================================================

# data set for scaffolding tables
table_df <- tibble::tribble(
    ~ team,
    "Overall", 
    "Team 1",
    "Team 2",
    "...",
    "Team N"
)

# add empty columns to df with names in vector of names

#' Add empty empty columns to a data rame
#'
#' @description
#' Create a column with the desired name whose contents are NA
#'
#' @param df Data frame.
#' @param names Character. Names of columns to create
#'
#' @return Data frame with columns in `names` added
add_cols <- function(
  df,
  names
) {

  for (i in names) {
    df[i] <- NA
  }

  return(df)

}

# create a gt table with standard options and column label inputs

#' Make a mock table
#'
#' @description
#' Create a mock table composed of the rows in `df` and
#' the columns in `column_labels`
#'
#' @param df Data frame.
#' @param column_labels Named list. Names become variable names; labels become
#' table column labels.
#'
#' @return An object of clase `gt_tbl`
make_mock_table <- function(
  df,
  column_labels # list2(name = "label")
) {

  mock_table <- df |>
    # add column with names from column_names
    add_cols(names = names(column_labels)) |>
    gt::gt(rowname_col = "team") |>
    gt::tab_stubhead(label = "Team") |>
    # set rowname column at 75 pixels width
    gt::cols_width(c(team) ~ px(75)) |>
    # replace NA with default missing marker: ---
    gt::sub_missing() |>
    # apply colors to header, column labels, and row groups
    gt::tab_options(
      heading.background.color = "#0F2B1D",
      column_labels.background.color = "#264535",
      row_group.background.color = "#516A5D"
    ) |>
    # apply column labels
    gt::cols_label(.list = column_labels)

}

# ==============================================================================
# Create tables
# ==============================================================================

# ------------------------------------------------------------------------------
# Post-planting
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# parcels_per_hhold
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

parcels <- rlang::list2(
  A = "Interviews (N)",
  B =  "Parcels (N)",
  C_1 = "Agricultural",
  C_2 = "Non-agricultural"
)

parcels_per_hhold_tbl <- table_df |>
  make_mock_table(column_labels = parcels) |>
  gt::tab_header(title = "Parcel per household, by team") |>
  gt::tab_spanner(
    label = "Parcels (%)",
    columns = c(C_1, C_2)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      "
      <b>Data needs:</b><br>
      - Data set of parcels<br>
      - Question on the parcel's main use(s) (optional)<br>
      - Values of the use question that denote agricultural use<br>
      <b>Objective:</b> 
      See if parcels per household conform to expectation (or sampling needs). 
      Identify indications that interviewers may be under-recording parcels 
      in order to minimize workload.
      "
    )
  )

gt::gtsave(
  data = parcels_per_hhold_tbl,
  filename = "parcels_per_hhold.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# parcel_gps
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

measurement <- rlang::list2(
  A = "Parcels (N)",
  B = "Measured (%)",
  a = "Refused",
  b = "Not accessible",
  c = "Other"
)

parcel_gps_tbl <- table_df |>
	make_mock_table(column_labels = measurement) |>
  gt::tab_header(title = "Parcel GPS area measurement, by team") |>
  gt::tab_spanner(
    label = "Why not measured (%)",
    columns = dplyr::matches("^[a-c]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Data set of parcels<br>
      - Question that captures the GPS measurement<br>
      - Value of the question that indicates the parcel was not measured<br>
      - Question the area was not measured (optional)<br>
      <b>Objective:</b> 
      See overall rate of measurement. See whether reasons for non-measurement 
      are reasonable, or indicate laziness.<br>
      <b>Note:</b> <font color ="red">The model questionnaire does not contain a 
      asking why the parcel was not measured and that could inform 
      the final 3 columns.</font>
      '
    )
  )

gt::gtsave(
  data = parcel_gps_tbl,
  filename = "parcel_gps.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# plots_per_parcel
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

plots <- rlang::list2(
  A = gt::html("Parcels<br>(N)"),
  B = gt::html("Plots<br>(N)"),
  C = gt::html("Mean plots per parcel<br>(N)")
)

plots_per_parcel_tbl <-  table_df |>
  make_mock_table(column_labels = plots) |>
  gt::tab_header(title = "Number of plots per parcel, by team") |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Data set of parcel-plots<br>
      - Parcel ID variable<br>
      <b>Objective:</b> See if plots per parcel conform to expectation
      (e.g., past estimates, sampling needs).
      Identify indications that interviewers may be under-recording plots in
      order to minimize workload.
      '
    )
  )

gt::gtsave(
  data = plots_per_parcel_tbl,
  filename = "plots_per_parcel.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# plot_use
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

plot_use <- rlang::list2(
  A = gt::html("Plots<br>(N)"),
  a = gt::html("Kitchen garden/farmyard"),
  b = gt::html("Cultivated"),
  c = gt::html("Left fallow")
)

plot_use_tbl <- table_df |>
  make_mock_table(column_labels = plot_use) |>
  gt::tab_header(title = "Plot use, by team") |>
  gt::tab_spanner(
    label = "Use (%)",
    columns = c(a, b, c)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Data set of plots<br>
      - Question on plot usage (e.g., kitchen garden, cultivated, fallow)<br>
      <b>Objective:</b> 
      See what percentage of plots are cropped (i.e., codes 1 & 2). 
      See if rates of non-cropping are realistic, or indicative of strategic 
      coding to avoid follow-up questions.
      '
    )
  )

gt::gtsave(
  data = plot_use_tbl,
  filename = "plot_use.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# plot_gps
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

plot_measurement <- rlang::list2(
  A = "Plots (N)",
  B = "Measured (%)",
  a = "Distance in district",
  b = "Distance outside district",
  c = "Refused",
  d = "Other"
)

plot_gps_tbl <- table_df |>
  make_mock_table(column_labels = plot_measurement) |>
  gt::tab_header(title = "Plot GPS area measurement, by team") |>
  gt::tab_spanner(
    label = "Why not measured (%)",
    columns = dplyr::matches("^[a-d]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Data set of plots<br>
      - Question that captures the GPS area mesurement<br>
      - Value of that question that indicates the plot was not measured<br>
      - Question on why the plot was not measured<br>
      <b>Objective:</b> 
      See overall rate of measurement. 
      See whether reasons for non-measurement are reasonable, 
      or indicate laziness or strategical behavior to minimize work.
      '
    )
  )

gt::gtsave(
  data = plot_gps_tbl,
  filename = "plot_gps.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# crops_per_plot
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

crops_per_plot <- rlang::list2(
  A = gt::html("Cultivated plots<br>(N)"),
  B = gt::html("Crops<br>(N)"),
  C = gt::html("Mean crops per plot<br>(N)")
)

crops_per_plot_tbl <-  table_df |>
  make_mock_table(column_labels = crops_per_plot) |>
  gt::tab_header(title = "Avg. crops per plot, by team") |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Data set of parcel-plot-crops<br>
      - Parcel ID variable<br>
      - Plot ID variable<br>
      <b>Objective:</b> 
      See if crops per parcel conform to expectation (sampling needs, if any). 
      Get a sense of whether interviewers may be under-recording crops in 
      order to minimize workload.
      '
    )
  )

gt::gtsave(
  data = crops_per_plot_tbl,
  filename = "crops_per_plot.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# crop_types
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

crop_types <- rlang::list2(
  A = gt::html("Unique crops<br>(N)"),
  B = gt::html("Temporary<br>(%)"),
  C = gt::html("Permanent<br>(%)")
)

crop_types_tbl <- table_df |>
  make_mock_table(column_labels = crop_types) |>
  gt::tab_header(title = "Crop types, by team") |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Data set of parcel-plot-crops<br>
      - Variable describing the crop type (i.e., temporary or tree/permanent)<br>
      - Value of the variable for temporary crops, typically 1<br>
      - Value of the variable for tree/permanent crops, typically 2<br>
      <b>Objective:</b> 
      See if over-reporting of permanent crops to avoid seed acquisition 
      questions in PP; 
      see if under-reporting of permanent crops to avoid seed acquisition 
      questions in PH.
      '
    )
  )

gt::gtsave(
  data = crop_types_tbl,
  filename = "crop_types.png",
  path = here::here("inst", "app", "www")
)

# ------------------------------------------------------------------------------
# Post-harvest
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# temp_crop_harvest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

harvest <- rlang::list2(
  A = gt::html("Planted parcel-plot-crops<br>(N)"),
  B = ("Yes"),
  C = ("No"),
  a = "DROUGHT",
  b = "FLOOD",
  c = "OTHER NATURAL EVENT",
  d = "PEST",
  e = "VIOLENCE",
  f = "THEFT",
  g = "LAND DISPUTE",
  h = "UNABLE TO WORK",
  i = "NO LABOR",
  j = "NOT HARVEST SEASON",
  k = "DELAYED HARVEST",
  l = "OTHER"
)

temp_crop_harvest_tbl <- table_df |>
  make_mock_table(df = , column_labels = harvest) |>
  gt::tab_header(title = "Prevalence of temporary crop harvest, by team") |>
  gt::tab_spanner(
    label = "Whether harvested (%)",
    columns = dplyr::matches("^[BC]$", ignore.case = FALSE)
  ) |>
  gt::tab_spanner(
    label = "Why not harvested (%)",
    columns = dplyr::matches("^[a-l]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      "
      <b>Data needs:</b><br>
      - Data set of parcel-plot-crops<br>
      - Crop ID variable<br>
      - Codes of crops that are temporary<br>
      - Question on whether the crop has been harvested<br>
      - Value of that question indicating harvest occured, typically 1<br>
      - Question on why the crop was not harvested<br>
      <b>Objective:</b> See if there is high level of non-harvest that may 
      indicate interviewers' avoidance of crop disposition module.
      "
    )
  )

gt::gtsave(
  data = temp_crop_harvest_tbl,
  filename = "temp_crop_harvest.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# temp_crop_sales
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

temp_crop_sales <- rlang::list2(
  a = gt::html("Crops harvested<br>(N)"),
  b = gt::html("Sold<br>(%)"),
  c = gt::html("Missing"),
  d = gt::html("DK")
)

temp_crop_sales_tbl <- table_df |>
  make_mock_table(column_labels = temp_crop_sales) |>
  gt::tab_header(title = "Temporary crop sales, by team") |>
  gt::tab_spanner(
    label = "Values (%)",
    columns = dplyr::matches("^[cd]$", , ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Data set of temporary crop sales<br>
      - Crop ID variable<br>
      - Question on whether the crop was sold<br>
      - Value of that question indicator the crop was sold<br>
      - Question on the revenue from sales<br>
      - Value of that question indicating the respondent does not know (DK)<br>
      <b>Objective:</b> See if high rates of missing or DK values.
      '
    )
  )

gt::gtsave(
  data = temp_crop_sales_tbl,
  filename = "temp_crop_sales.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# perm_crop_harvest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

perm_crops_harvested <- rlang::list2(
  A = gt::html("Crops planted<br>(N)"),
  D = gt::html("Produced<br>(%)"),
  B = gt::html("Yes"),
  C = gt::html("No")
)

perm_crop_harvest_tbl <- table_df |>
  make_mock_table(column_labels = perm_crops_harvested) |>
  gt::tab_header(
    title = "Prevalence of tree/permanent crop harvest, by team"
  ) |>
  gt::tab_spanner(
    label = "Harvested (%)",
    columns = dplyr::matches("^[BC]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Data set of parcel-plot-crops<br>
      - Crop ID variable<br>
      - Codes of crops that are tree/permanent<br>
      - Question on whether the crop has been harvested<br>
      - Value of that question indicating harvest occured, typically 1<br>
      - Question on why the crop was not harvested<br>
      <b>Objective:</b> 
      See if strategic under-reporting of production/harvesting to reduce 
      crop-level reporting.
      '
    )
  )

gt::gtsave(
  data = perm_crop_harvest_tbl,
  filename = "perm_crop_harvest.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# perm_crop_sales
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

perm_crop_sales <- rlang::list2(
  A = gt::html("Crops harvested<br>(N)"),
  B = gt::html("Sold<br>(%)"),
  a = gt::html("Missing"),
  b = gt::html("DK")
)

perm_crop_sales_tbl <- table_df |>
  make_mock_table(column_labels = perm_crop_sales) |>
  gt::tab_header(
    title = "Tree/permanent crop sales and value reporting, by team"
  ) |>
  gt::tab_spanner(
    label = "Value reported (%)",
    columns = dplyr::matches("^[ab]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Data set of tree/permanent crop sales<br>
      - Crop ID variable<br>
      - Question on whether the crop was sold<br>
      - Value of that question indicator the crop was sold<br>
      - Question on the revenue from sales<br>
      - Value of that question indicating the respondent does not know (DK)<br>
      <b>Objective:</b> 
      See if high rates of missing or DK values.
      '
    )
  )

gt::gtsave(
  data = perm_crop_sales_tbl,
  filename = "perm_crop_sales.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# livestock_ownership
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

raise_livestock <- rlang::list2(
  a = gt::html("Households<br>(N)"),
  b = gt::html("Raise livestock<br>(%)"),
  c = gt::html("Types of livestock<br>(AVG)")
)

livestock_ownership_tbl <-  table_df |>
  make_mock_table(column_labels = raise_livestock) |>
  gt::tab_header(title = "Livestock ownership, by team") |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - Variable that captures which livestock are owned<br>
      <b>Objective:</b> 
      Identify indications of under-reporting of livestock 
      in order to avoid follow-up question.
      '
    )
  )

gt::gtsave(
  data = livestock_ownership_tbl,
  filename = "livestock_ownership.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cow_displacement
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

cow_disp <- rlang::list2(
  a = gt::html("Have cattle<br>(N)"),
  b = gt::html("Bulls"),
  c = gt::html("Cows"),
  d = gt::html("Steers/heifers"),
  e = gt::html("Calves")
)

cow_displacement_tbl <- table_df |>
  make_mock_table(df = , column_labels = cow_disp) |>
  gt::tab_header(title = "Cow displacement, by team") |>
  gt::tab_spanner(
    label = "Types (%)",
    columns = matches("^[b-e]$", , ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household data set<br>
      - Question that captures which livestock are owned<br>
      - Code for bull<br>
      - Code for cow<br>
      - Code for steer/heifer<br>
      - Code for calf<br>
      <b>Objective:</b> 
      See if under-reporting of cows, displacement to other cattle types 
      (e.g., heifer), in order to avoid milk production module.
      '
    )
  )

gt::gtsave(
  data = cow_displacement_tbl,
  filename = "cow_displacement.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# hen_displacement
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

hen_disp <- rlang::list2(
  a = gt::html("Have chickens<br>(N)"),
  b = gt::html("Cocks/broilers"),
  c = gt::html("Hens/layers"),
  d = gt::html("Pullets")
)

hen_displacement_tbl <-  table_df |>
  make_mock_table(column_labels = hen_disp) |>
  gt::tab_header(title = "Hen displacement, by team") |>
  gt::tab_spanner(
    label = "Types (%)",
    columns = dplyr::matches("^[b-d]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household data set<br>
      - Question that captures which livestock are owned<br>
      - Code for cock<br>
      - Code for hen<br>
      - Code for pullet<br>
      <b>Objective:</b> 
      See if under-reporting of hens, displacement to other chicken types, 
      in order to avoid completing the egg production module.
      '
    )
  )

gt::gtsave(
  data = hen_displacement_tbl,
  filename = "hen_displacement.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# milk_prod_sales
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

milk_production <- rlang::list2(
  a = gt::html("Milk-producing livestock<br>(N)"),
  b = gt::html("Produce milk<br>(%)"),
  c = gt::html("Sell milk<br>(%)"),
  d = "Missing",
  e = "DK"
)

milk_prod_sales_tbl <- table_df |>
  make_mock_table(column_labels = milk_production) |>
  gt::tab_header(title = "Mik production and sales, by team") |>
  gt::tab_spanner(
    label = "Sales values (%)",
    columns = dplyr::matches("^[de]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data:</b><br>
      - Livestock data set<br>
      - Livestock ID variable<br>
      - Codes of livestock that produce milk<br>
      - Question on whether produced milk<br>
      - Value of that question indicating production<br>
      - Question on whether sold milk<br>
      - Value of that question indicating sales<br>
      - Question on value of sales revenue<br>
      - Value of that question indicating do not know (DK)<br>
      <b>Objective:</b> 
      See if possible under-reporting of production and/or sales in order to 
      minimize questions asked.
      '
    )
  )

gt::gtsave(
  data = milk_prod_sales_tbl,
  filename = "milk_prod_sales.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# egg_prod_sales
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

egg_production <- rlang::list2(
  a = gt::html("Egg-producing poultry<br>(N)"),
  b = gt::html("Produce eggs<br>(%)"),
  c = gt::html("Sell eggs<br>(%)"),
  d = "Missing",
  e = "DK"
)

egg_prod_sales_tbl <- table_df |>
  make_mock_table(column_labels = egg_production) |>
  gt::tab_header(title = "Egg production and sales, by team") |>
  gt::tab_spanner(
    label = "Sales values (%)",
    columns = dplyr::matches("^[de]$", , ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data:</b><br>
      - Livestock data set<br>
      - Livestock ID variable<br>
      - Codes of livestock that produce eggs<br>
      - Question on whether produced eggs<br>
      - Value of that question indicating production<br>
      - Question on whether sold eggs<br>
      - Value of that question indicating sales<br>
      - Question on value of sales revenue<br>
      - Value of that question indicating do not know (DK)<br>
      <b>Objective:</b> 
      (1) Identify potential under-reporting of production to avoid 
      follow-up questions.
      (2) Identify incomplete sales income data.
      '
    )
  )

gt::gtsave(
  data = egg_prod_sales_tbl,
  filename = "egg_prod_sales.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# fisheries_prod_sales
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

fish_production <- rlang::list2(
  a = gt::html("Total households<br>(N)"),
  b = gt::html("Practice fishing<br>(%)"),
  c = gt::html("Products caught<br>(AVG)"),
  d = gt::html("Sell<br>(%)"),
  e = "Missing",
  f = "DK"
)

fisheries_prod_sales_tbl <- table_df |>
  make_mock_table(column_labels = fish_production) |>
  gt::tab_header(title = "Fishery production and sales, by team") |>
  gt::tab_spanner(
    label = "Sales values (%)",
    columns = dplyr::matches("^[ef]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - Fisheries products data set<br>
      - Question on whether practice fisheries activities<br>
      - Value of that question indicating engagement in fisheries<br>
      - Question on fisheries items produced<br>
      - Question on whether sold fisheries production<br>
      - Value of that question indicating sales<br>
      - Question on sales revenue<br>
      - Value of that question indicating do not know (DK)<br>
      <b>Objective:</b> 
      (1) Identify potential under-reporting of fishing to avoid 
      follow-up questions. 
      (2) Identify incomplete sales income data.
      '
    )
  )

gt::gtsave(
  data = fisheries_prod_sales_tbl,
  filename = "fisheries_prod_sales.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# aquaculture_prod_sales
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

aqua_production <- rlang::list2(
  a = gt::html("Total households<br>(N)"),
  b = gt::html("Practice aquaculture<br>(%)"),
  c = gt::html("Products collected<br>(AVG)"),
  d = gt::html("Sell<br>(%)"),
  e = "Missing",
  f = "DK"
)

aquaculture_prod_sales_tbl <- table_df |>
  make_mock_table(column_labels = aqua_production) |>
  gt::tab_header(title = "Aquaculture production and sales, by team") |>
  gt::tab_spanner(
    label = "Sales values (%)",
    columns = dplyr::matches("^[ef]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - aquaculture products data set<br>
      - Question on whether practice aquaculture activities<br>
      - Value of that question indicating engagement in aquaculture<br>
      - Question on aquaculture items produced<br>
      - Question on whether sold aquaculture production<br>
      - Value of that question indicating sales<br>
      - Question on sales revenue<br>
      - Value of that question indicating do not know (DK)<br>
      <b>Objective:</b> 
      (1) Identify potential under-reporting of fishing to avoid 
      follow-up questions. 
      (2) Identify incomplete sales income data.
      '
    )
  )

gt::gtsave(
  data = aquaculture_prod_sales_tbl,
  filename = "aquaculture_prod_sales.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# forestry_prod_sales
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

forestry_production <- rlang::list2(
  a = gt::html("Total households<br>(N)"),
  b = gt::html("Practice forestry<br>(%)"),
  c = gt::html("Number products<br>(AVG)"),
  d = gt::html("Sell<br>(%)"),
  e = "Missing",
  f = "DK"
)

forestry_prod_sales_tbl <- table_df |>
  make_mock_table(column_labels = forestry_production) |>
  gt::tab_header(title = "Forestry production and sales, by team") |>
  gt::tab_spanner(
    label = "Sales values (%)",
    columns = dplyr::matches("^[ef]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - forestry products data set<br>
      - Question on whether practice forestry activities<br>
      - Value of that question indicating engagement in forestry<br>
      - Question on forestry items produced<br>
      - Question on whether sold forestry production<br>
      - Value of that question indicating sales<br>
      - Question on sales revenue<br>
      - Value of that question indicating do not know (DK)<br>
      <b>Objective:</b> 
      (1) Identify potential under-reporting of fishing to avoid 
      follow-up questions. 
      (2) Identify incomplete sales income data.
      '
    )
  )

gt::gtsave(
  data = forestry_prod_sales_tbl,
  filename = "forestry_prod_sales.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# process_crop_prod
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

crop_processing <- rlang::list2(
  a = gt::html("Have production<br(N)"),
  b = gt::html("Yes"),
  c = gt::html("No"),
  d = gt::html("Items produced<br>(AVG)"),
  e = gt::html("Sales<br>(%)"),
  f = gt::html("Missing"),
  g = gt::html("DK")
)

process_crop_prod_tbl <- table_df |>
  make_mock_table(column_labels = crop_processing) |>
  gt::tab_header(title = "Processing of crop production, by team") |>
  gt::tab_spanner(
    label = "Processing (%)",
    columns = dplyr::matches("^[bc]$", ignore.case = FALSE)
  ) |>
  gt::tab_spanner(
    label = "Sales values (%)",
    columns = dplyr::matches("^[fg]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - Indicator variable on whether any temporary crops were harvested<br>
      - Indicator variable on whether any permanent crops were harvested<br>
      - Indicator variable on whether crops were processed<br>
      - Question on which processed products were produced<br>
      - Produced products data set<br>
      - Question on whether the product was sold<br>
      - Value for that question of whether the product was sold<br>
      - Question(s) with sales values<br>
      - Value of do not know (DK) for sales<br>
      <b>Objective:</b> 
      (1) See if low rate of production to avoid lengthy questions. 
      (2) See if missing or DK sales values.
      '
    )
  )

gt::gtsave(
  data = process_crop_prod_tbl,
  filename = "process_crop_prod.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# crop_labor
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

crop_labor <- rlang::list2(
  a = gt::html("Grew crops<br>(N)"),
  b = gt::html("Household labor only<br>(%)"),
  c = gt::html("External labor only<br>(%)"),
  d = gt::html("Household"),
  e = gt::html("Paid"),
  f = gt::html("Free/exchange")
)

crop_labor_tbl <- table_df |>
	make_mock_table(column_labels = crop_labor) |>
	gt::tab_header(title = "Crop Labor") |>
	gt::tab_spanner(
    columns = c(d, e, f),
    label = "Labor types (%)",
    id = "sources"
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - Question that indicates whether grew crops<br>
      - Value of that question: grew crops<br>
      - Question that captures which categories of paid labor were used<br>
      - Question that captures which categories of free/exchange labor
      were used<br>
      - Household members level data set<br>
      - Question caputring whether a member worked to grow crops<br>
      - Value of that question: worked to grow crops<br>
      <b>Objective:</b> 
      See if under-reporting of labor to avoid follow-up questions.
      '
    )
  ) |>
  gt::tab_footnote(
    locations = gt::cells_column_spanners(spanners = "sources"),
    footnote = paste0(
      "Percentages may not sum to 100. ",
      "Households may have multiple sources of crop labor."
    )
  )

gt::gtsave(
  data = crop_labor_tbl,
  filename = "crop_labor.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# livestock_labor_tbl
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

livestock_labor <- rlang::list2(
  a = gt::html("Raise livestock<br>(N)"),
  b = gt::html("No labor of any type<br>(%)"),
  c = gt::html("Household<br>(%)"),
  d = gt::html("Free/exchange<br>(%)"),
  e = gt::html("Hired<br>(%)")
)

livestock_labor_tbl_tbl <- table_df |>
  make_mock_table(column_labels = livestock_labor) |>
  gt::tab_header(title = "Livestock labor, by team") |>
  gt::tab_spanner(
    label = "Labor type(s) (%)",
    columns = dplyr::matches("^[c-e]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - Indicator variable: whether raise livestock<br>
      - Value of that indicator indicating raising livestock<br>
      - Livestock labor data set<br>
      - Labor category ID variable<br>
      - Question on number of workers available<br>
      - Value indicating no workers<br>
      - Codes for household labor categories
      (e.g., "adult males (household members)", etc.)<br>
      - Codes for free/exchange labor categories<br>
      - Codes for paid labor categories<br>
      <b>Objective:</b> 
      See if under-reporting of labor to avoid follow-up questions.
      '
    )
  )

gt::gtsave(
  data = livestock_labor_tbl_tbl,
  filename = "livestock_labor_tbl.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# fisheries_labor
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

fisheries_labor <- rlang::list2(
  a = gt::html("Practice fishing<br>(N)"),
  b = gt::html("No labor reported<br>(%)"),
  d = "Household",
  e = "Free",
  f = "Hired"
)

fisheries_labor_tbl <- table_df |>
  make_mock_table(column_labels = fisheries_labor) |>
  gt::tab_header(title = "Fisheries labor, by team") |>
  gt::tab_spanner(
    label = "Labour source(s) (%)",
    columns = dplyr::matches("^[d-f]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - Indicator variable: whether engaged in fisheries<br>
      - Value of that indicator indicating engagement in the sector<br>
      - Question indicating the categories of labor used<br>
      - Codes for household labor<br>
      - Codes for free/exchange labor<br>
      - Codes for paid labor<br>
      <b>Objective:</b> 
      Identify cases with no labor reported.
      '
    )
  )

gt::gtsave(
  data = fisheries_labor_tbl,
  filename = "fisheries_labor.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# aquaculture_labor
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

aquaculture_labor <- rlang::list2(
  a = gt::html("Practice aquaculture<br>(N)"),
  b = gt::html("No labor reported<br>(%)"),
  d = "Household",
  e = "Free",
  f = "Hired"
)

aquaculture_labor_tbl <- table_df |>
  make_mock_table(column_labels = aquaculture_labor) |>
  gt::tab_header(title = "Aquaculture labor, by team") |>
  gt::tab_spanner(
    label = "Labour source(s) (%)",
    columns = dplyr::matches("^[d-f]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - Indicator variable: whether engaged in aquaculture<br>
      - Value of that indicator indicating engagement in the sector<br>
      - Question indicating the categories of labor used<br>
      - Codes for household labor<br>
      - Codes for free/exchange labor<br>
      - Codes for paid labor<br>
      <b>Objective:</b> 
      Identify cases with no labor reported.
      '
    )
  )

gt::gtsave(
  data = aquaculture_labor_tbl,
  filename = "aquaculture_labor.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# forestry_labor
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

forestry_labor <- rlang::list2(
  a = gt::html("Practice forestry<br>(N)"),
  b = gt::html("No labor reported<br>(%)"),
  d = "Household",
  e = "Free",
  f = "Hired"
)

forestry_labor_tbl <- table_df |>
  make_mock_table(column_labels = forestry_labor) |>
  gt::tab_header(title = "Forestry labor, by team") |>
  gt::tab_spanner(
    label = "Labour source(s) (%)",
    columns = dplyr::matches("^[d-f]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - Indicator variable: whether engaged in forestry<br>
      - Value of that indicator indicating engagement in the sector<br>
      - Question indicating the categories of labor used<br>
      - Codes for household labor<br>
      - Codes for free/exchange labor<br>
      - Codes for paid labor<br>
      <b>Objective:</b> 
      Identify cases with no labor reported.
      '
    )
  )

gt::gtsave(
  data = forestry_labor_tbl,
  filename = "forestry_labor.png",
  path = here::here("inst", "app", "www")
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# income_sources
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

income_sources <- rlang::list2(
  A = gt::html("Total households<br>(N)"),
  B = gt::html("Practice any ag<br>(N)"),
  b = gt::html("Temporary crop<br>(%)"),
  c = gt::html("Tree/permanent crop<br>(%)"),
  d = gt::html("Processed crop<br>(%)"),
  e = gt::html("Live animal<br>(%)"),
  f = gt::html("Slaughter<br>(%)"),
  g = gt::html("Milk<br>(%)"),
  h = gt::html("Egg<br>(%)"),
  i = gt::html("Other products<br>(%)"),
  j = gt::html("Aquaculture<br>(%)"),
  k = gt::html("Fishery<br>(%)"),
  l = gt::html("Forestry<br>(%)")
)

income_sources_tbl <- table_df |>
  make_mock_table(column_labels = income_sources) |>
  gt::tab_header(title = "Sources of income, by team") |>
  gt::tab_spanner(
    label = "Crop sales (%)",
    columns = matches("^[b-d]$", , ignore.case = FALSE)
  ) |>
  gt::tab_spanner(
    label = "Livestock sales (%)",
    columns = dplyr::matches("^[e-i]$", ignore.case = FALSE)
  ) |>
  gt::tab_spanner(
    label = "Other agriculture sales (%)",
    columns = dplyr::matches("^[j-l]$", ignore.case = FALSE)
  ) |>
  gt::tab_source_note(
    source_note = gt::html(
      '
      <b>Data needs:</b><br>
      - Household-level data set<br>
      - Indicator variable: grows crops<br>
      - Indicator variable: raises livestock<br>
      - Temporary crop disposition data set<br>
      - Indicator variable: sells temporary crops<br>
      - Value of that variable: sells temporary crops<br>
      - Indicator variable: sells tree/permanent crops<br>
      - Value of that variable: sells tree/permanent crops<br>
      - Processed crop product data set<br>
      - Indicator variable: sells processed crop products<br>
      - Livestock data set<br>
      - Indicator variable: sold live animals.<br>
      - Indicator variable: sold slaughtered animal.<br>
      - Value: sold slaughtered animal.<br>
      - Indicator variable: sold live poultry.<br>
      - Indicator variable: sold slaughtered poultry.<br>
      - Value: sold slaughtered poultry.<br>
      - Indicator variable: sold milk<br>
      - Value: sold milk.<br>
      - Indicator variable: sold eggs.<br>
      - Value: sold eggs.<br>
      - Data set of other animal products<br>
      - Indicator variable: sold other animal products.<br>
      - Value: sold other animal products.<br>
      <b>Objective:</b> Get an overview of income sources and assess likelihood.
      '
    )
  )

gt::gtsave(
  data = income_sources_tbl,
  filename = "income_sources.png",
  path = here::here("inst", "app", "www")
)
