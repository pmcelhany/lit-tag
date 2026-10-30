#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @noRd
app_server <- function(input, output, session) {
  # Check for pre-loaded options
  golem_opts <- golem::get_golem_options()
  has_db <- !is.null(golem_opts$database_file)
  has_cat <- !is.null(golem_opts$categories_file)

  pre_load_active <- FALSE
  if (has_db && has_cat) {
    pre_load_active <- TRUE
  } else if (has_db || has_cat) {
    showNotification(
      "Both database_file and categories_file must be provided for pre-loading. Falling back to standard app.",
      type = "warning",
      duration = 10
    )
  }

  if (pre_load_active) {
    # Skip selection and load viewer directly
    output$module_ui_container <- renderUI({
      viewer_ui("viewer_instance")
    })
    viewer_server("viewer_instance")
  } else {
    # Standard app logic
    output$module_ui_container <- renderUI({
      page_navbar(
        ## App title --------------------------------
        title = div(
          img(src = "www/lit_tag_logo_1.png", height = "100px"),
          h1("Welcome to lit-tag!"),
          HTML("&nbsp; &nbsp;"),
          style = "color: brown; display: flex; align-items: center;"
        ),
        nav_panel(
          "",
          tags$head(
            tags$link(
              rel = "stylesheet",
              type = "text/css",
              href = "www/styles.css"
            )
          ),
          h3("Select the module for this lit-tag session."),
          h3("Use lit-tag-builder to create or modify a lit-tag database."),
          h3(
            "Use lit-tag-viewer to search, create reports, and generate graphics from an existing lit-tag database."
          ),
          h3(
            "You can open a seperate lit-tag window on your browser for simultaneous builder and viewer sessions."
          ),
          shinyWidgets::radioGroupButtons(
            "lit_tag_module",
            label = "",
            choices = c("lit-tag-builder", "lit-tag-viewer"),
            selected = character(0),
            individual = TRUE
          )
        )
      )
    })

    # When the user selects a module (builder or viewer),
    # display the appropriate ui and load server code
    observeEvent(input$lit_tag_module, {
      # get selected module
      selected_module <- input$lit_tag_module

      #load module ui
      output$module_ui_container <- renderUI({
        if (is.null(selected_module)) {} else if (
          selected_module == "lit-tag-builder"
        ) {
          builder_ui("builder_instance")
        } else if (selected_module == "lit-tag-viewer") {
          viewer_ui("viewer_instance")
        }
      })
      #load module server code
      if (selected_module == "lit-tag-builder") {
        builder_server("builder_instance")
      } else if (selected_module == "lit-tag-viewer") {
        viewer_server("viewer_instance")
      }

      #dismiss dialog
      removeModal()
    })
  }
}
