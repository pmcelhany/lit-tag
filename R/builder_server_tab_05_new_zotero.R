# R/builder_server_tab_05_new_zotero.R

#' @noRd
builder_server_tab_05_new_zotero <- function() {
  output$generate_ris <- downloadHandler(
    filename = function() {
      paste(str_remove(input$database_nz_csv$name, ".csv"), ".ris", sep = "")
    },
    content = function(file) {
      withProgress(message = "Generating RIS file", value = 0, {
        d_nz <- read_csv(input$database_nz_csv$datapath)

        if (input$all_or_unique_ris == "Unique titles") {
          d_nz <- d_nz %>%
            distinct(title, .keep_all = TRUE)
        }

        incProgress(1 / 4)
        output$n_old_key_db <- renderText(HTML(paste(
          "Number of papers in original (old keys) database: ",
          nrow(d_nz),
          sep = ""
        )))

        values$d_old_key_db <- d_nz
        incProgress(2 / 4)

        d_ris <- create_ris_dataframe(d_nz)

        incProgress(3 / 4)

        write_csv(d_ris, file, quote = "none")

        output$ris_generated <- renderText(
          HTML("RIS file generated from original database downloaded")
        )
        incProgress(4 / 4)
      })
    }
  )

  output$generate_new_keys_db <- downloadHandler(
    filename = function() {
      paste(
        str_remove(input$database_knz_csv$name, ".csv"),
        "_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      d_nkz <- read_csv(input$database_knz_csv$datapath) %>%
        clean_names() %>%
        select(key, publication_year, author, title)

      output$n_new_key_zotero <- renderText(
        HTML(paste(
          "Number of papers in zotero file with new keys:",
          nrow(d_nkz)
        ))
      )

      #d_new_key_db <- values$d_old_key_db %>%
      d_new_key_db <- read_csv(input$database_nz_csv$datapath) %>%
        select(-key) %>%
        left_join(d_nkz, join_by(publication_year, author, title)) %>%
        relocate(key)

      output$n_new_key_db <- renderText(
        HTML(paste(
          "Number of papers in new keys database:",
          nrow(d_new_key_db)
        ))
      )

      output$n_new_key_db_missing <- renderText(
        HTML(paste(
          "Number of papers with missing keys in new keys database:",
          sum(is.na(d_new_key_db$key))
        ))
      )

      write_csv(d_new_key_db, file)
    }
  )
}
