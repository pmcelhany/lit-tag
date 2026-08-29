# R/viewer_server_tab_04_summary_table.R

viewer_server_tab_04_summary_table <- function() {
  ## Summary table ---------------------------------------------

  ### Show summary table change -------------------
  output$summary_table <- renderDT(
    {
      #### Use full data set or the filtered data set? -----------
      d <- NULL
      if (input$summary_data == "Full dataset") {
        d <- values$d_mcdr_tagged
      } else {
        d <- values$d_mcdr_filtered
      }

      if (!is.null(d)) {
        d <- d %>%
          select(input$summary_var)
      }

      req(d)
      d
    },
    selection = "none",
    extensions = 'ColReorder',
    callback = JS(newjs),
    options = list(
      dom = "t",
      pageLength = 10000,
      stateSave = TRUE,
      stateDuration = 0,
      order = list(),
      columnDefs = list(list(width = '200px', targets = "_all")),
      scrollX = TRUE,
      scrollY = TRUE,
      colReorder = TRUE
    ),
    rownames = FALSE,
    server = FALSE
  )

  ### Download summary csv ----------------------------
  output$download_summary <- downloadHandler(
    filename = function() {
      paste(input$summary_download_name, ".csv", sep = "")
    },
    content = function(file) {
      d <- NULL
      if (input$summary_data == "Full dataset") {
        d <- values$d_mcdr_tagged
      } else {
        d <- values$d_mcdr_filtered
      }

      # if the column order has not changes, select var in default order
      # if the order has changed, select user specifier order
      if (is.null(input$colOrder)) {
        d <- d %>%
          select(input$summary_var)
      } else {
        d <- d %>%
          select(input$colOrder)
      }

      write.csv(d, file, row.names = FALSE)
    }
  )
}
