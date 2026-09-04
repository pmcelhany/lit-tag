# R/builder_server_utils.R

# Global boolean flag to toggle validation checks.
# Set ENABLE_FILE_VALIDATION <- FALSE to bypass all file validation checks.
# Developers can also bypass checks via option: options(lit_tag_enable_validation = FALSE)
ENABLE_FILE_VALIDATION <- TRUE

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

#' @noRd
validate_csv_headers <- function(filepath) {
  if (!file.exists(filepath)) return("File does not exist.")
  if (file.info(filepath)$size == 0) return("The database file is empty.")
  first_line <- tryCatch(readLines(filepath, n = 1), error = function(e) NULL)
  if (is.null(first_line) || length(first_line) == 0 || trimws(first_line) == "") {
    return("The database file has no content or headers.")
  }
  headers_df <- tryCatch(read.csv(filepath, header = FALSE, nrows = 1, colClasses = "character"), error = function(e) NULL)
  if (is.null(headers_df) || nrow(headers_df) == 0) return("Could not parse the headers of the database file.")
  headers <- as.character(headers_df[1, ])
  invalid_idx <- which(is.na(headers) | trimws(headers) == "")
  if (length(invalid_idx) > 0) {
    return(paste0("Database file contains columns with missing or empty header names (column indices: ", paste(invalid_idx, collapse = ", "), ")."))
  }
  return(TRUE)
}

#' @noRd
validate_database_csv <- function(filepath) {
  header_res <- validate_csv_headers(filepath)
  if (!isTRUE(header_res)) return(header_res)
  headers_df <- read.csv(filepath, header = FALSE, nrows = 1, colClasses = "character")
  headers <- as.character(headers_df[1, ])
  if (!("key" %in% headers)) return("Database file is missing the required 'key' column.")
  df <- tryCatch({
    readr::read_csv(filepath, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  }, error = function(e) NULL)
  if (is.null(df)) return("Failed to read the database CSV file.")
  keys <- df[["key"]]
  missing_keys <- which(is.na(keys) | trimws(keys) == "")
  if (length(missing_keys) > 0) {
    return(paste0("Database file contains missing or empty values in the 'key' column at row(s): ", paste(missing_keys + 1, collapse = ", "), "."))
  }
  duplicated_keys <- keys[duplicated(keys)]
  if (length(duplicated_keys) > 0) {
    unique_dupes <- unique(duplicated_keys)
    return(paste0("Database file contains duplicate values in the 'key' column: ", paste(unique_dupes, collapse = ", "), "."))
  }
  return(TRUE)
}

#' @noRd
validate_categories_xlsx <- function(filepath) {
  if (!file.exists(filepath)) {
    return("Categories file does not exist.")
  }
  
  sheets <- tryCatch({
    readxl::excel_sheets(filepath)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(sheets) || length(sheets) == 0) {
    return("Failed to read sheet names from the categories Excel file.")
  }
  
  for (sheet_name in sheets) {
    sheet_data <- tryCatch({
      readxl::read_excel(filepath, sheet = sheet_name, col_names = FALSE, n_max = 2)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(sheet_data) || nrow(sheet_data) == 0) {
      return(paste0("Failed to read data from sheet '", sheet_name, "' or sheet is empty."))
    }
    
    headers <- as.character(sheet_data[1, ])
    
    if (length(headers) == 0) {
      return(paste0("Sheet '", sheet_name, "' has no columns."))
    }
    
    invalid_header_idx <- which(is.na(headers) | trimws(headers) == "")
    if (length(invalid_header_idx) > 0) {
      return(paste0("Sheet '", sheet_name, "' contains columns with missing or empty header names at column(s): ", paste(invalid_header_idx, collapse = ", "), "."))
    }
    
    if (nrow(sheet_data) < 2) {
      return(paste0("Sheet '", sheet_name, "' is missing the field type row (Row 2)."))
    }
    
    field_types <- as.character(sheet_data[2, ])
    
    allowed_types <- c(
      "check_box_single",
      "check_box_multiple",
      "text_box",
      "number",
      "date",
      "text_area"
    )
    
    for (i in seq_along(field_types)) {
      f_type <- field_types[i]
      if (is.na(f_type) || trimws(f_type) == "") {
        return(paste0("Sheet '", sheet_name, "' contains a missing or empty field type at column ", i, " ('", headers[i], "')."))
      }
      f_type_trimmed <- trimws(f_type)
      if (!(f_type_trimmed %in% allowed_types)) {
        return(paste0("Sheet '", sheet_name, "' contains an invalid field type '", f_type_trimmed, "' at column ", i, " ('", headers[i], "'). Allowed field types are: ", paste(allowed_types, collapse = ", "), "."))
      }
    }
    
    if (sheet_name %in% c("notes", "Notes")) {
      if (length(headers) != 1) {
        return(paste0("The '", sheet_name, "' sheet must contain exactly 1 column, but it has ", length(headers), " columns."))
      }
      if (headers[1] != "Notes") {
        return(paste0("The '", sheet_name, "' sheet column header must be exactly 'Notes', but found '", headers[1], "'."))
      }
      if (trimws(field_types[1]) != "text_area") {
        return(paste0("The '", sheet_name, "' sheet second row value must be 'text_area', but found '", field_types[1], "'."))
      }
    } else {
      text_area_idx <- which(trimws(field_types) == "text_area")
      if (length(text_area_idx) > 0) {
        return(paste0("Sheet '", sheet_name, "' has 'text_area' at column(s): ", paste(text_area_idx, collapse = ", "), ". Only 'notes' or 'Notes' sheet is allowed to use 'text_area'."))
      }
    }
  }
  
  return(TRUE)
}
