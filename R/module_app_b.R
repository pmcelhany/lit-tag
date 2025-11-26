# Module App B UI
appBUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Module B: Iris Data Summary"),
    selectInput(ns("var"), "Select Variable:", names(iris)[1:4]),
    verbatimTextOutput(ns("summary"))
  )
}

# Module App B Server
appBServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$summary <- renderPrint({
      summary(iris[[input$var]])
    })
  })
}
