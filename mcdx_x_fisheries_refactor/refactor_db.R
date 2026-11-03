library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Paths
dir_path <- "mcdx_x_fisheries_refactor"
snake_cat_path <- file.path(
  dir_path,
  "fisheries_mcdr_lit_tag_categories_2026_09_29_snake.xlsx"
)
sentence_cat_path <- file.path(
  dir_path,
  "fisheries_mcdr_lit_tag_categories_2026_11_02_sentence.xlsx"
)
db_path <- file.path(dir_path, "fisheries_mcdr_2026_11_02_2104_UTC_snake.csv")

mapping_out_path <- file.path(
  dir_path,
  "fisheries_mcdr_tag_mapping_verification.csv"
)
db_out_path <- file.path(
  dir_path,
  "fisheries_mcdr_2026_11_02_2104_UTC_sentence.csv"
)

# 1. Build the Mapping Table
sheets_snake <- excel_sheets(snake_cat_path)
sheets_sentence <- excel_sheets(sentence_cat_path)

mapping_list <- list()

# Helper function to convert Excel column name to snake case database column name
to_snake_case <- function(x) {
  # Special mappings
  if (x == "mCDR focus") {
    return("m_cdr_focus")
  }
  if (x == "mCDR method") {
    return("m_cdr_method")
  }
  if (x == "Type of method used") {
    return("type_of_method_used")
  }
  if (x == "Adjacent topic to mCDR") {
    return("adjacent_topic_to_m_cdr")
  }
  if (x == "Adjacent topic to Fisheries") {
    return("adjacent_topic_to_fisheries")
  }
  if (x == "Paper review status") {
    return("paper_review_status")
  }
  if (x == "Date last reviewed") {
    return("date_last_reviewed")
  }
  if (x == "Chemical/mineral added") {
    return("chemical_mineral_added")
  }
  if (x == "Notes") {
    return("notes")
  }
  if (x == "Life Stage") {
    return("life_stage")
  }

  # Default mapping
  x <- str_to_lower(x)
  x <- str_replace_all(x, " ", "_")
  return(x)
}

for (i in seq_along(sheets_snake)) {
  sheet_snake <- sheets_snake[i]
  sheet_sentence <- sheets_sentence[i] # Assuming they align in order

  df_snake <- read_excel(snake_cat_path, sheet = sheet_snake)
  df_sentence <- read_excel(sentence_cat_path, sheet = sheet_sentence)

  # Skip first row which contains check_box_single, text_box, etc.
  if (nrow(df_snake) > 1 && nrow(df_sentence) > 1) {
    df_snake <- df_snake[-1, ]
    df_sentence <- df_sentence[-1, ]
  }

  # For each column in the sheet, create pairs
  cols <- colnames(df_snake)
  for (col in cols) {
    # Skip standard text entry or date columns
    if (
      col %in%
        c(
          "Species common name",
          "Species scientific name",
          "Date last reviewed"
        )
    ) {
      next
    }

    db_col_name <- to_snake_case(col)

    # Extract non-NA pairs
    snake_vals <- df_snake[[col]]
    sentence_vals <- df_sentence[[col]]

    # We only care about rows where both are not NA, or at least snake is not NA
    valid_idx <- !is.na(snake_vals)

    if (any(valid_idx)) {
      mapping_list[[length(mapping_list) + 1]] <- tibble(
        tag_name = db_col_name,
        snake_case_value = snake_vals[valid_idx],
        sentence_case_value = sentence_vals[valid_idx]
      )
    }
  }
}

mapping_df <- bind_rows(mapping_list) %>%
  distinct() %>%
  filter(!is.na(sentence_case_value)) # Remove any rows where sentence case is NA

# Handle Notes mapping specially if needed
# The user wants to map values within columns, including the notes tab
# Let's ensure no text_area or date values slipped into the mapping
mapping_df <- mapping_df %>%
  filter(
    !(snake_case_value %in%
      c(
        "text_area",
        "text_box",
        "date",
        "enter_date",
        "check_box_single",
        "check_box_multiple"
      ))
  )

# Write verification CSV
write_csv(mapping_df, mapping_out_path)
cat(sprintf("Saved mapping verification to: %s\n", mapping_out_path))

# 2. Refactor the Database
db <- read_csv(db_path, show_col_types = FALSE)

# Create a lookup dictionary: tag_name -> named vector (snake -> sentence)
lookup_list <- split(mapping_df, mapping_df$tag_name) %>%
  map(
    ~ {
      vec <- .x$sentence_case_value
      names(vec) <- .x$snake_case_value
      vec
    }
  )

# Function to replace tags in a single string containing multiple tags separated by ";"
replace_tags <- function(tag_str, col_name) {
  if (is.na(tag_str) || tag_str == "") {
    return(tag_str)
  }

  if (!col_name %in% names(lookup_list)) {
    return(tag_str)
  }

  lookup <- lookup_list[[col_name]]

  # Split by semicolon
  tags <- str_split(tag_str, ";")[[1]]

  # Map each tag; if not found in lookup, keep original
  new_tags <- map_chr(
    tags,
    ~ {
      val <- lookup[.x]
      if (is.na(val)) {
        .x # Fallback to original
      } else {
        val
      }
    }
  )

  # Join back
  paste(new_tags, collapse = ";")
}

# Apply to all columns that have mappings
cols_to_update <- intersect(colnames(db), names(lookup_list))

db_updated <- db
for (col in cols_to_update) {
  db_updated[[col]] <- map_chr(db_updated[[col]], ~ replace_tags(.x, col))
}

# Write updated DB
write_csv(db_updated, db_out_path)
cat(sprintf("Saved refactored database to: %s\n", db_out_path))
