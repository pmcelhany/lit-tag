# R/builder_server_tab_04_database_maintenance.R

#' @noRd
tag_values_in_db <- function(d) {
  zotero_fields <- read_csv("data/zotero_fields.csv")$zotero_fields
  db_names <- names(d)
  tags_notes <- db_names[!(db_names %in% zotero_fields)]
  db_tags <- tags_notes[!str_detect(tags_notes, "notes")]
  db_notes <- tags_notes[str_detect(tags_notes, "notes")]

  tag_options <- db_tags %>%
    purrr::set_names() %>%
    map(\(x) as.character(unique(d[[x]])))

  tag_options_unique <- names(tag_options) %>%
    purrr::set_names() %>%
    map(\(x) {
      unique(str_trim(unlist(unlist(str_split(tag_options[[x]], ";")))))
    })

  tag_option_length <- tag_options_unique %>%
    map(\(x) length(x)) %>%
    unlist()

  d_tag <- data.frame(n_option = tag_option_length) %>%
    tibble::rownames_to_column("tags") %>%
    arrange(tags)

  return(list(
    d_tag = d_tag,
    tag_options_unique = tag_options_unique,
    db_notes = db_notes
  ))
}

#' @noRd
replace_tag_option <- function(d, tag, option, value) {
  dr <- d %>%
    mutate(!!sym(tag) := str_replace(.[[tag]], fixed(option), value))

  return(dr)
}

#' @noRd
read_as_char <- function(path) {
  zotero_fields <- read_csv("data/zotero_fields.csv")$zotero_fields
  d <- read_csv(path) %>%
    mutate(across(everything(), as.character))

  all_var <- c(
    zotero_fields,
    values$tag_variables,
    "date_time_added_db",
    "date_time_obsolete_db"
  )

  d_all_var <- add_cols_if_missing(d, all_var)

  d_complete <- d_all_var %>%
    select(-(setdiff(all_var, names(.))))

  return(d_complete)
}

#' @noRd
builder_server_tab_04_database_maintenance <- function() {
  output$replace_tag_name_download <- downloadHandler(
    filename = function() {
      paste(
        str_remove(input$edit_db$name, ".csv"),
        "_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      d_edit_db <- read_csv(input$edit_db$datapath)
      old_name <- input$old_tag_name
      new_name <- input$new_tag_name
      d_edit_complete <- d_edit_db %>%
        rename(!!sym(new_name) := old_name)

      write_csv(d_edit_complete, file)
    }
  )

  output$replace_option_name_download <- downloadHandler(
    filename = function() {
      paste(
        str_remove(input$edit_db$name, ".csv"),
        "_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      d_edit_db <- read_csv(input$edit_db$datapath)

      t_name <- input$tag_name
      old_opt_name <- input$old_option_name
      new_opt_name <- input$new_option_name

      d_edit_complete <- replace_tag_option(
        d_edit_db,
        t_name,
        old_opt_name,
        new_opt_name
      )

      write_csv(d_edit_complete, file)
    }
  )

  output$delete_tags_download <- downloadHandler(
    filename = function() {
      paste(
        str_remove(input$edit_db$name, ".csv"),
        "_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      d_edit_db <- read_csv(input$edit_db$datapath)
      delete_tags <- str_trim(str_split_1(input$delete_tags, ","))
      d_edit_complete <- d_edit_db %>%
        select(!delete_tags)

      write_csv(d_edit_complete, file)
    }
  )

  output$delete_tag_option_download <- downloadHandler(
    filename = function() {
      paste(
        str_remove(input$edit_db$name, ".csv"),
        "_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      d_edit_db <- read_csv(input$edit_db$datapath)
      delete_tag_options <- str_trim(str_split_1(
        input$delete_tag_options,
        ","
      ))

      tags <- word(delete_tag_options, sep = "/")
      options <- word(delete_tag_options, -1, sep = "/")

      d_edit_complete <- d_edit_db

      for (i in 1:length(tags)) {
        d_edit_complete <- replace_tag_option(
          d_edit_complete,
          tags[i],
          options[i],
          ""
        )
      }

      write_csv(d_edit_complete, file)
    }
  )

  output$delete_not_in_zotero_download <- downloadHandler(
    filename = function() {
      paste(
        str_remove(input$edit_db$name, ".csv"),
        "_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      d_edit_db <- read_csv(input$edit_db$datapath)
      zotero_keys <- read_csv(input$zotero_for_delete$datapath) %>%
        clean_names() %>%
        pull(key)

      d_edit_complete <- d_edit_db %>%
        filter(key %in% zotero_keys)

      write_csv(d_edit_complete, file)
    }
  )

  output$delete_papers_with_tag_option_download <- downloadHandler(
    filename = function() {
      paste(
        str_remove(input$edit_db$name, ".csv"),
        "_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      d_edit_db <- read_csv(input$edit_db$datapath)
      delete_papers_tag_options <-
        str_trim(str_split_1(input$delete_papers_with_tag_options, ","))

      tags <- word(delete_papers_tag_options, sep = "/")
      options <- word(delete_papers_tag_options, -1, sep = "/")

      d_edit_complete <- d_edit_db

      for (i in 1:length(tags)) {
        d_edit_complete <- d_edit_complete %>%
          filter(!!sym(tags[i]) != options[i])
      }

      write_csv(d_edit_complete, file)
    }
  )

  output$download_combined <- downloadHandler(
    filename = function() {
      paste0(
        input$combined_filename,
        "_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv"
      )
    },
    content = function(file) {
      withProgress(message = "Generating combined database", value = 0, {
        incProgress(1 / 4)

        load_categories(input$combine_cat$datapath)

        d_comb_db <- input$combine_dbs$datapath %>%
          map(\(x) read_as_char(x)) %>%
          list_rbind() %>%
          select(where(~ !all(is.na(.x))))

        incProgress(3 / 4)

        write_csv(d_comb_db, file)

        # for output text of number of papers
        np <- input$combine_dbs$datapath %>%
          map(\(x) nrow(read_csv(x))) %>%
          unlist()

        n_papers <- input$combine_dbs %>%
          mutate(n = np) %>%
          mutate(
            n_papers_text = paste0("Number of references in ", name, ": ", n)
          ) %>%
            pull(n_papers_text) %>%
            c(paste("Number of references in summed files:", sum(np))) %>%
            c(paste("Number of references in combined db:", nrow(d_comb_db)))

        output$n_combined <- renderUI({
          HTML(paste(n_papers, collapse = "<br>"))
        })

        incProgress(4 / 4)
      })
    }
  )
}
