# Module App A UI
appAUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Module A: Geyser Data"),
    sliderInput(ns("bins"), "Number of bins:", min = 1, max = 50, value = 30),
    plotOutput(ns("distPlot"))
  )
}

# Module App A Server
appAServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$distPlot <- renderPlot({
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (mins)',
           main = 'Histogram of waiting times')
    })
  })
}
