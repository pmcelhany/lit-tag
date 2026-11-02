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
  # Zotero strips htlm code from ris imports
  # To prserve html rags, replace "<" and ">" with text codes
  paper <- paper %>%
    mutate(
      title = str_replace_all(title, "<", "&lt"),
      title = str_replace_all(title, ">", "&gt"),
      title = str_replace_all(title, "\"", "&quot")
    )
  if (hasName(paper, "abstract_note")) {
    paper <- paper %>%
      mutate(
        abstract_note = str_replace_all(abstract_note, "<", "&lt"),
        abstract_note = str_replace_all(abstract_note, ">", "&gt"),
        abstract_note = str_replace_all(abstract_note, "\"", "&quot")
      )
  }

  paper <- paper %>%
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
    tag_value = if (!is.na(paper$author)) {
      paste("AU", "-", str_split_1(paper$author, ";"), sep = "  ")
    } else {
      character(0)
    }
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
create_ris_dataframe <- function(d_nz) {
  html_escape_pattern <- "&([a-zA-Z0-9]+|#[0-9]+|#x[0-9a-fA-F]+);"

  for (i in seq_len(nrow(d_nz))) {
    title <- d_nz$title[i]
    escapes_in_title <- unlist(regmatches(
      title,
      gregexpr(html_escape_pattern, title)
    ))
    replace_escapes_in_title <- sapply(escapes_in_title, unescape_html)
    if (length(escapes_in_title) > 0) {
      for (j in seq_along(escapes_in_title)) {
        title <- str_replace(
          title,
          escapes_in_title[j],
          replace_escapes_in_title[j]
        )
      }
    }
    d_nz$title[i] <- title
  }

  d_ris <- seq_len(nrow(d_nz)) %>%
    map(\(x) ris_fun(d_nz[x, ])) %>%
    list_rbind()

  return(d_ris)
}
