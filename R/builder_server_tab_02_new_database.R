# R/builder_server_tab_02_new_database.R

#' @noRd
builder_server_tab_02_new_database <- function() {
  output$new_database <- downloadHandler(
    filename = function() {
      paste(input$new_db_name, ".csv", sep = "")
    },
    content = function(file) {
      d_zotero <- read_zotero(input$new_zotero_csv$datapath)
      load_categories(input$cat_new_db$datapath)
      d_new_db <- d_zotero
      d_new_db[values$tag_variables] <- NA

      output$nrow_new_db <- renderText(paste(
        "Number of papers in new db:",
        nrow(d_new_db)
      ))
      output$n_new_tags <-
        renderText(paste(
          "Number of tags (including notes) in new db:",
          length(values$tag_variables)
        ))

      write_csv(d_new_db, file)
    }
  )
}
