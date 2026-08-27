#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    uiOutput("module_ui_container")
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  add_resource_path(
    "reports",
    app_sys("report")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "littag"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

# golem_add_external_resources <- function() {
#   # Safe local absolute path in dev, app_sys() in production/server
#   www_path <- if (interactive()) {
#     here::here("inst/app/www")
#   } else {
#     app_sys("app/www")
#   }
#   report_path <- if (interactive()) {
#     here::here("inst/report")
#   } else {
#     app_sys("report")
#   }

#   add_resource_path("www", www_path)
#   add_resource_path("reports", report_path)

#   tags$head(
#     favicon(),
#     bundle_resources(
#       path = www_path,
#       app_title = "littag"
#     )
#   )
# }
