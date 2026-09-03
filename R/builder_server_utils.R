# R/builder_server_utils.R

#' @noRd
is_invalid_numeric <- function(val) {
  if (is.null(val) || length(val) == 0) {
    return(FALSE)
  }
  val <- as.character(val)
  vals <- unlist(strsplit(val, ";", fixed = TRUE))
  vals <- trimws(vals)
  vals <- vals[
    !is.na(vals) &
      vals != "" &
      vals != "NA" &
      vals != "-" &
      vals != "-." &
      vals != "."
  ]
  if (length(vals) == 0) {
    return(FALSE)
  }
  any(vapply(
    vals,
    function(v) {
      suppressWarnings(is.na(as.numeric(v)))
    },
    FUN.VALUE = logical(1)
  ))
}

#' @noRd
add_cols_if_missing <- function(df, cols_to_add) {
  missing_cols <- cols_to_add[!cols_to_add %in% names(df)]
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA_character_
  }
  return(df)
}

#' @noRd
category_meta_fun <- function(d) {
  d_meta <- d[1, ] %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("cat_label") %>%
    rename(select_type = V1) %>%
    set_rownames(names(d %>% clean_names()))

  return(d_meta)
}

#' @noRd
category_remove_meta_fun <- function(d) {
  d_cat <- d %>%
    clean_names() %>%
    mutate(row_id = 1:nrow(.)) %>%
    filter(row_id > 1) %>%
    select(-row_id)

  return(d_cat)
}

#' @noRd
select_box_fun <- function(x, y, cat, meta) {
  box <- NULL
  choice_opts <- NULL
  if (meta[y, "select_type"] %in% c("check_box_single", "check_box_multiple")) {
    choice_opts <- cat %>%
      pluck(x) %>%
      pull(y) %>%
      sort() %>%
      na.omit()
    if ("not_applicable" %in% choice_opts) {
      choice_opts <- choice_opts[choice_opts != "not_applicable"]
      choice_opts <- c(choice_opts, "not_applicable")
    }
  }

  if (meta[y, "select_type"] == "check_box_single") {
    box <- radioButtons(
      inputId = ns(y),
      label = meta[y, "cat_label"],
      choices = choice_opts,
      selected = character(0)
    )
  }

  if (meta[y, "select_type"] == "check_box_multiple") {
    box <- checkboxGroupInput(
      inputId = ns(y),
      label = meta[y, "cat_label"],
      choices = choice_opts
    )
  }
  if (meta[y, "select_type"] == "text_box") {
    box <- textInput(inputId = ns(y), label = meta[y, "cat_label"])
  }

  if (meta[y, "select_type"] == "number") {
    box <- textInput(inputId = ns(y), label = meta[y, "cat_label"])
  }

  if (meta[y, "select_type"] == "date") {
    box <- dateInput(
      inputId = ns(y),
      label = meta[y, "cat_label"],
      value = NA
    )
  }

  return(box)
}

#' @noRd
remove_leading_special_char <- function(x) {
  x_no_leading_special_char <- data.frame(x = x) %>%
    mutate(
      x = if_else(
        str_sub(x, 1, 1) %in% c("-", "+", "="),
        str_sub(x, 2, -1),
        x
      )
    ) %>%
    pull(x)

  return(x_no_leading_special_char)
}

#' @noRd
load_categories <- function(filepath) {
  values$categories_with_meta <- filepath %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    map(\(x) read_excel(filepath, sheet = x))

  values$d_category_meta <- values$categories_with_meta %>%
    map(\(x) category_meta_fun(x)) %>%
    list_rbind()

  # create a list of data frames with the categories and response
  values$categories <- values$categories_with_meta %>%
    map(\(x) category_remove_meta_fun(x))

  # Set has_notes
  if ("notes" %in% row.names(values$d_category_meta)) {
    values$has_notes <- TRUE
  } else {
    values$has_notes <- FALSE
  }

  #vector of tag variables
  if (values$has_notes) {
    values$tag_variables <- c(
      row.names(values$d_category_meta)[
        row.names(values$d_category_meta) != "notes"
      ],
      values$categories$notes %>%
        pull("notes")
    )
  } else {
    values$tag_variables <- row.names(values$d_category_meta)
  }
}

#' @noRd
read_zotero <- function(filepath) {
  d <- read_csv(filepath) %>%
    clean_names() %>%
    remove_empty() %>%
    mutate(first_author = word(author, sep = ",")) %>%
    arrange(first_author) %>%
    mutate(across(everything(), as.character))

  return(d)
}
