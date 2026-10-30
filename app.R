# Launch the ShinyApp
# To deploy, run: rsconnect::deployApp()

options(shiny.autoreload = FALSE)

# Configure Quarto for report generation
# We use a bundled version of Quarto (1.5.57) to ensure compatibility on the NMFS Posit server,
# as newer versions (e.g. 1.9.37) have caused crashes.
quarto_base_dir <- file.path(getwd(), "quarto-1.5.57", "bin")
local_quarto <- file.path(quarto_base_dir, "quarto")

# Ensure all bundled binaries have executable permissions when running on Linux servers
if (dir.exists(quarto_base_dir)) {
  all_binaries <- list.files(
    quarto_base_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  Sys.chmod(all_binaries, mode = "0755")
}

# Point R to the local bundled version of Quarto
Sys.setenv(QUARTO_PATH = local_quarto)

# Load application package
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

# Run the app.
#
# To create a topic-specific app (pre-loaded database/categories):
# Provide BOTH database_file and categories_file paths. This skips the builder/viewer
# selection and automatically loads the viewer with the specified data and branding.
# You can place the specific .csv, .xlsx, and image files inside the app's directory.
# (If you provide only one file, the app will show an error and fall back to the standard app).
#
# To create a standard app (where the user uploads their own files):
# Simply comment out the assignments inside run_app().
run_app(
  # database_file = "data/mcdr-x-fisheries-lit-tag-database.csv",
  # categories_file = "data/mcdr-x-fisheries-lit-tag-categories.xlsx",
  # app_title = "mCDRxFisheries Literature Database",
  # app_icon = "www/crab2.jpg"
)

# To run the app with updated resources (i.e. contents of of inst/app/www) during development,
# run golem::run_dev() in the console
# also note that positron will not open the pdf user guides - you have to open in a browser
