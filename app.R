# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# QUARTO_PATH !!!! -----------------------
# QUARTO_PATH must be set for reports
# This is not an issue on the local machine, but is not globally set on the server
# This is the path on the NWFSC Posit server for hosting shiny apps
Sys.setenv(QUARTO_PATH="/opt/quarto/latest/bin/quarto")
# This is the path on Paul's mac and can be used to reset the env variable
# if it get accidentally set for the server
# Sys.setenv(QUARTO_PATH="/usr/local/bin/quarto")

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
run_app() # add parameters here (if any)
