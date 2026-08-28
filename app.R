# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Disable auto-reload globally for this session
options(shiny.autoreload = FALSE)

# QUARTO_PATH  -----------------------
# QUARTO_PATH must be set for reports

# This is the path used the NFMS Posit server
# There is some problem with using the most recent version on Quarto on NMFS server that causes a crash
#Sys.setenv(QUARTO_PATH = "/opt/quarto/latest/bin/quarto")

# Instead of using the quarto installed on the server,
# the code points to a copy of quarto version 1.5.57 installed within the shiny app.
# The version 1.5.57 is needed because the version 1.9.37 installed on the server causes the app to crash when rendering quarto
# 1. Define paths
quarto_base_dir <- file.path(getwd(), "quarto-1.5.57", "bin")
local_quarto <- file.path(quarto_base_dir, "quarto")
# 2. BULK FIX ALL PERMISSIONS:
# This recursively finds EVERY file inside the quarto bin folder
# (including deno, sass, pandoc, etc.) and makes them executable on the linex server.
if (dir.exists(quarto_base_dir)) {
  all_binaries <- list.files(
    quarto_base_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  Sys.chmod(all_binaries, mode = "0755")
}
# 3. Point R to the local version
Sys.setenv(QUARTO_PATH = local_quarto)

# This is the path on Paul's mac and can be used to set the env variable
# for rendering quarto on the local machine.
#Sys.setenv(QUARTO_PATH = "/usr/local/bin/quarto")

pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

# run the app
run_app()

# To run the app with updated resources (i.e. contents of of inst/app/www) during development,
# run golem::run_dev() in the console
# also note that positron will not open the pdf user guides - you have to open in a browser
