#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @noRd
app_server <- function(input, output, session) {

  # splash screen modal dialog to select module (builder or viewer)
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE,
               eventExpr = {}, {
                 showModal(modalDialog(
                   title = "Welcome to lit-tag!",
                   HTML("Select the module for this lit-tag session."),
                   HTML("Use lit-tag-builder to create or modify a lit-tag database."),
                   HTML("Use lit-tag-viewer to search, create reports, and generate graphics from an existing lit-tag database."),
                   shinyWidgets::radioGroupButtons("lit_tag_module", label = "",
                                     choices = c("lit-tag-builder",
                                                 "lit-tag-viewer"),
                                     selected = character(0),
                                     individual = TRUE),
                   footer = NULL
                 ))
               })

  # When the user selects a model (builder or viewer),
  # display the appropriate ui and load server code
  observeEvent(input$lit_tag_module, {

    # get selected module
    selected_module <- input$lit_tag_module

    #load module ui
    output$module_ui_container <- renderUI({
      if(is.null(selected_module)){
      } else if (selected_module == "lit-tag-builder") {
        # Render the UI for App A module
        builder_ui("builder_instance")
      } else if (selected_module == "lit-tag-viewer") {
        # Render the UI for App B module
        appBUI("app_b_instance")
      }
    })
    #load module server code
    if (selected_module == "lit-tag-builder") {
      #appAServer("app_a_instance")
    } else if (selected_module == "lit-tag-viewer") {
      appBServer("app_b_instance")
    }

    #dismiss dialog
    removeModal()
  })
}
