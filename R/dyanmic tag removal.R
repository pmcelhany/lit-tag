library(shiny)
library(bslib)
library(purrr)

ui <- page_fluid(
  actionButton("clear_all", "Remove All Panels"),
  navset_pill(
    id = "my_navset",
    nav_panel("Tab 1", "Content 1", value = "t1"),
    nav_panel("Tab 2", "Content 2", value = "t2"),
    nav_panel("Tab 3", "Content 3", value = "t3")
  )
)

server <- function(input, output, session) {
  observeEvent(input$clear_all, {
    # Define the IDs/values of the panels to be removed
    # Alternatively, use a reactive list to track current panels
    panel_ids <- c("t1", "t2", "t3")

    # Loop through and remove each
    walk(panel_ids, ~ nav_remove(id = "my_navset", target = .x))
  })
}

shinyApp(ui, server)
