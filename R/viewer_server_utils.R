# R/viewer_server_utils.R

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

category_meta_fun <- function(d) {
  d_meta <- d[1, ] %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("cat_label") %>%
    rename(select_type = V1) %>%
    set_rownames(names(d %>% clean_names()))

  return(d_meta)
}

category_remove_meta_fun <- function(d) {
  d_cat <- d %>%
    clean_names() %>%
    mutate(row_id = 1:nrow(.)) %>%
    filter(row_id > 1) %>%
    select(-row_id)

  return(d_cat)
}

select_box_fun <- function(x, y, cat, meta) {
  box <- NULL

  choice_opts <- NULL
  if (
    meta[y, "select_type"] %in% c("check_box_single", "check_box_multiple")
  ) {
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
  if (meta[y, "select_type"] %in% c("text_box", "number")) {
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

viewer_server_utils <- function() {
  newjs <<- paste0(
    'table.on("column-reorder", function(e, settings, details){
        var table = document.getElementById("',
    ns("summary_table"),
    '");
        var thead = table.getElementsByTagName("thead");
        var ths = thead[0].getElementsByTagName("th");
        var tableFields = [];
        for (let i = 0; i < ths.length; i++) {
            tableFields[i] = ths[i].innerHTML;
        }
        Shiny.onInputChange("',
    ns("colOrder"),
    '", tableFields);
    });'
  )

  values <<- reactiveValues(
    d_mcdr_tagged = NULL,
    categories_with_meta = NULL,
    categories = NULL,
    tag_variables = NULL,
    d_category_meta = NULL,
    d_mcdr_filtered = NULL,
    d_plot = NULL,
    table_trigger = 0,
    has_notes = NULL,
    notes_variables = NULL,
    number_tags = NULL,
    active_cat_tabs = character(0)
  )

  dt_proxy <<- DT::dataTableProxy("table")

  table_vars <<- reactive({
    vars <- c("author", "publication_year", "title")
    if (!is.null(input$show_extra) && input$show_extra) {
      vars <- c("author", "publication_year", "title", "extra")
    }
    return(vars)
  })
}
