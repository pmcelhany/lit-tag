# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# QUARTO_PATH  -----------------------
# QUARTO_PATH must be set for reports

# Quarto path option 1
# This is the typical path on a Mac and can be used to set the env variable
# for rendering quarto on the local machine.
# Sys.setenv(QUARTO_PATH="/usr/local/bin/quarto")

#Quarto path option 2
# This is a path commonly used for Posit servers
# Sys.setenv(QUARTO_PATH="/opt/quarto/bin/")

# Quarto path option 3
# This code can be used if Quarto is embedded in the in the shiny app project
# The code points to a copy of quarto version 1.5.57 installed within the shiny app.
# Note: quarto 1.5.57 is not include in the github lit-tag repo because the Quarto file is too big
# If using an embedded version of Quarto, it should be installed in the local project folder
# The version 1.5.57 is needed because the version 1.9.37 installed on the server cause the app to crash when rendering quarto
# 1. Define paths
# quarto_base_dir <- file.path(getwd(), "quarto-1.5.57", "bin")
# local_quarto <- file.path(quarto_base_dir, "quarto")
# # 2. BULK FIX ALL PERMISSIONS:
# # This recursively finds EVERY file inside the quarto bin folder
# # (including deno, sass, pandoc, etc.) and makes them executable on the linex server.
# if (dir.exists(quarto_base_dir)) {
#   all_binaries <- list.files(quarto_base_dir, recursive = TRUE, full.names = TRUE)
#   Sys.chmod(all_binaries, mode = "0755")
# }
# 3. Point R to the local version
#Sys.setenv(QUARTO_PATH = local_quarto)

# load packages and set options
pkgload::load_all(export_all = TRUE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

# run the app
run_app()
