# R/builder_server_tab_01_tag_edit.R

#' @noRd
render_paper_info <- function(label, paper_var) {
  if (!is.null(values$d_mcdr_filtered)) {
    return(renderText(paste(
      label,
      values$d_mcdr_filtered[input$table_rows_selected, ] %>%
        pull(paper_var)
    )))
  } else {
    return(renderText(paste(label, "")))
  }
}

#' @noRd
save_tag_value <- function(key, tag) {
  tag_value <- paste(input[[tag]], collapse = ";")
  if (tag_value == "") {
    tag_value <- NA
  }

  current_val <- values$d_mcdr_tagged[
    values$d_mcdr_tagged$key == key,
    tag,
    drop = TRUE
  ]

  if (!identical(as.character(tag_value), as.character(current_val))) {
    values$d_mcdr_tagged[values$d_mcdr_tagged$key == key, tag] <-
      tag_value
    return(TRUE)
  }
  return(FALSE)
}

#' @noRd
save_last_row <- function(key, d_category_meta, d_notes) {
  if (!is.null(key) && length(key) > 0 && !is.na(key)) {
    tag_changes <- rownames(d_category_meta) %>%
      map_lgl(\(x) save_tag_value(key, x))

    note_changes <- FALSE
    if (!is.null(d_notes)) {
      note_changes <- d_notes %>%
        pull("notes") %>%
        map_lgl(\(x) save_tag_value(key, x))
    }
    if (any(tag_changes) || any(note_changes)) {
      values$d_mcdr_filtered[values$d_mcdr_filtered$key == key, ] <-
        values$d_mcdr_tagged[values$d_mcdr_tagged$key == key, ]

      # trigger table update to show changes in bibliography columns (e.g. Extra)
      values$table_trigger <- values$table_trigger + 1
    }
  }
}

#' @noRd
load_row_tags_fun <- function(x, d_category_meta, table_rows_selected) {
  row_val <- values$d_mcdr_filtered %>%
    slice(table_rows_selected) %>%
    pull(x)

  if (d_category_meta[x, "select_type"] == "check_box_single") {
    if (
      is.na(row_val) |
        row_val == "NA" |
        row_val == "" |
        identical(row_val, character(0))
    ) {
      s <- character(0)
    } else {
      s <- row_val
    }
    updateRadioButtons(inputId = x, selected = s)
  }
  if (d_category_meta[x, "select_type"] == "check_box_multiple") {
    if (
      is.na(row_val) |
        row_val == "NA" |
        row_val == "" |
        identical(row_val, character(0))
    ) {
      s <- character(0)
    } else {
      s <- str_split_1(row_val, ";")
    }
    updateCheckboxGroupInput(inputId = x, selected = s)
  }

  if (d_category_meta[x, "select_type"] == "text_box") {
    updateTextInput(inputId = x, value = row_val)
  }

  if (d_category_meta[x, "select_type"] == "number") {
    values$number_last_val[[x]] <- row_val
    if (is_invalid_numeric(row_val)) {
      updateTextInput(
        inputId = x,
        label = shiny::span(
          d_category_meta[x, "cat_label"],
          style = "color: red;"
        ),
        value = row_val
      )
    } else {
      updateTextInput(
        inputId = x,
        label = d_category_meta[x, "cat_label"],
        value = row_val
      )
    }
  }

  if (d_category_meta[x, "select_type"] == "date") {
    if (
      is.na(row_val) |
        row_val == "NA" |
        row_val == "" |
        identical(row_val, character(0))
    ) {
      d_val <- NA
    } else {
      d_val <- row_val
    }
    updateDateInput(inputId = x, value = d_val)
  }
}

#' @noRd
filter_fun <- function(y) {
  selected_val <- input[[paste("filter", y, sep = "_")]]

  var_with_na_sting <- values$d_mcdr_filtered %>%
    pull(y) %>%
    replace_na("NA")

  values$d_mcdr_filtered <- values$d_mcdr_filtered %>%
    filter(var_with_na_sting %in% selected_val)
}
