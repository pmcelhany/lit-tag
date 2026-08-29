# R/builder_server_tab_05_new_zotero.R

#' @noRd
unescape_html <- function(str) {
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

#' @noRd
ris_tag_fun <- function(tag, paper, ris_tag_map) {
  value <- NA
  if (hasName(paper, ris_tag_map[tag])) {
    value <- pull(paper, ris_tag_map[tag])
  }
  if (!is.na(value)) {
    return(paste(tag, "-", value, sep = "  "))
  }
  return(NULL)
}

#' @noRd
ris_fun <- function(paper) {
  #paper <- d_nz[1,]
  # Zotero strips htlm code from ris imports
  # To prserve html rags, replace "<" and ">" with text codes
  paper <- paper %>%
    mutate(
      title = str_replace_all(title, "<", "&lt"),
      title = str_replace_all(title, ">", "&gt"),
      title = str_replace_all(title, "\"", "&quot")
    ) %>%
    mutate(
      abstract_note = str_replace_all(abstract_note, "<", "&lt"),
      abstract_note = str_replace_all(abstract_note, ">", "&gt"),
      abstract_note = str_replace_all(abstract_note, "\"", "&quot")
    ) %>%
    mutate(publication_year = as.character(publication_year))

  ris_tag_map <- c(
    PY = "publication_year",
    TI = "title",
    AB = "abstract_note",
    JF = "publication_title",
    JO = "journal_abbreviation",
    DO = "doi",
    IS = "issue",
    VO = "volume",
    PB = "publisher",
    ED = "editor",
    PP = "place",
    EP = "pages",
    ET = "edition",
    UR = "url",
    SN = "issn"
  )

  tag_value <- names(ris_tag_map) %>%
    map(\(x) ris_tag_fun(x, paper = paper, ris_tag_map = ris_tag_map)) %>%
    unlist()

  general_tags <- data.frame(tag_value)

  author_tags <- data.frame(
    tag_value = paste(
      "AU",
      "-",
      str_split_1(paper$author, ";"),
      sep = "  "
    )
  )

  record <- data.frame(
    tag_value = paste(
      "TY",
      "-",
      case_match(
        paper$item_type,
        "journalArticle" ~ "JOUR",
        "report" ~ "RPRT",
        "book" ~ "BOOK",
        "thesis" ~ "THES",
        "preprint" ~ "UNPB",
        "bookSection" ~ "CHAP",
        "conferencePaper" ~ "CPAPER",
        .default = "-99"
      ),
      sep = "  "
    )
  ) %>%
    bind_rows(author_tags) %>%
    bind_rows(general_tags) %>%
    bind_rows(data.frame(tag_value = "ER  -"))

  return(record)
}

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

        html_escape_pattern <- "&([a-zA-Z0-9]+|#[0-9]+|#x[0-9a-fA-F]+);"

        for (i in 1:nrow(d_nz)) {
          title <- d_nz$title[i]
          escapes_in_title <- unlist(regmatches(
            title,
            gregexpr(html_escape_pattern, title)
          ))
          replace_escapes_in_title <- sapply(escapes_in_title, unescape_html)
          if (length(escapes_in_title) > 0) {
            for (j in 1:length(escapes_in_title)) {
              title <- str_replace(
                title,
                escapes_in_title[j],
                replace_escapes_in_title[j]
              )
            }
          }
          d_nz$title[i] <- title
        }

        values$d_old_key_db <- d_nz

        incProgress(2 / 4)

        d_ris <- 1:nrow(d_nz) %>%
          map(\(x) ris_fun(d_nz[x, ])) %>%
          list_rbind()

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

      d_new_key_db <- values$d_old_key_db %>%
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
