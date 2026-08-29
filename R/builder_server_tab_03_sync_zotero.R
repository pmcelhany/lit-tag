# R/builder_server_tab_03_sync_zotero.R

#' @noRd
builder_server_tab_03_sync_zotero <- function() {
  output$update_from_zotero <- downloadHandler(
    filename = function() {
      paste(
        str_remove(input$sync_database_csv$name, ".csv"),
        "_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      withProgress(message = "Updating from Zotero", value = 0, {
        ### Read zotero --------------------------------------

        d_zotero <- read_zotero(input$sync_zotero_csv$datapath)

        incProgress(1 / 4)

        ### Tag variables -------------------------------------------------

        load_categories(input$sync_categories_excel$datapath)
        tag_variables <- values$tag_variables

        incProgress(2 / 4)

        ### Set init database ------------------------------------------------
        #d_database <- values$d_mcdr_tagged
        d_database <- read_csv(input$sync_database_csv$datapath)

        ### Set Keys -----------------------------------------------
        # Intial, new and obsolete keys
        keys_db_init <- unique(d_database$key)
        keys_z <- unique(d_zotero$key)
        new_keys <- keys_z[!(keys_z %in% keys_db_init)]
        old_keys_in_zotero <- keys_db_init[keys_db_init %in% keys_z]
        # old_keys_not_in_zotero are also called "obsolete keys"
        old_keys_not_in_zotero <- keys_db_init[!(keys_db_init) %in% keys_z]

        output$n_init_db <- renderText(paste(
          "Inital papers in db:",
          length(keys_db_init)
        ))
        output$n_zotero <- renderText(paste(
          "Papers in Zotero file:",
          length(keys_z)
        ))
        output$n_new_keys <- renderText(paste(
          "New paper keys in Zotero:",
          length(new_keys)
        ))
        output$n_old_key <- renderText(paste(
          "Old paper keys in db but not Zotero:",
          length(old_keys_not_in_zotero)
        ))
        output$n_new_db <- renderText(paste(
          "Papers in new db:",
          length(keys_db_init) +
            length(new_keys)
        ))

        incProgress(3 / 4)

        ### Get current datetime -----------------------------------------
        current_datetime <- format(now("UTC"), "%Y_%m_%d_%H%M_UTC")

        ### Create updated database ----------------------------------------
        # if data file does not contain "key" column make new database
        # else append to new papers to existing db
        if (!("key" %in% names(d_database))) {
          d_updated_db <- d_zotero %>%
            mutate(
              date_time_added_db = current_datetime,
              date_time_obsolete_db = NA
            )
          d_updated_db[tag_variables] <- "NA"
        } else {
          d_new_zotero <- d_zotero %>%
            filter(key %in% new_keys) %>%
            mutate(
              date_time_added_db = current_datetime,
              date_time_obsolete_db = NA
            )
          d_new_db <- d_new_zotero %>%
            mutate(across(everything(), as.character))
          d_new_db[tag_variables] <- "NA"

          # papers in oringial db and the new zotero
          # update the db with any edits to the zotero variable
          d_updated_z_db <- d_database %>%
            filter(!(key %in% old_keys_not_in_zotero)) %>%
            left_join(d_zotero, join_by("key")) %>%
            select(-contains(".x")) %>%
            purrr::set_names(str_remove(names(.), "\\.y")) %>%
            mutate(across(everything(), as.character))

          # papers in original db but no in the new zotero
          d_obsolete_db <- d_database %>%
            filter(key %in% old_keys_not_in_zotero) %>%
            mutate(date_time_obsolete_db = current_datetime) %>%
            mutate(across(everything(), as.character))

          # combine all types of papers in one new db
          d_updated_db <- bind_rows(
            d_obsolete_db,
            d_updated_z_db,
            d_new_db
          ) %>%
            arrange(author, publication_year)
        }

        ### Write updated database to csv
        write_csv(d_updated_db, file)
        incProgress(4 / 4)
      })
    }
  )
}
