# R/viewer_server_tab_02_search_database.R

viewer_server_tab_02_search_database <- function() {
  # trigger table re-render when columns change
  observeEvent(input$show_extra, {
    values$table_trigger <- values$table_trigger + 1
  })

  ### Bibliography table -----------------------------------------
  output$table <- renderDT(
    {
      values$table_trigger
      req(values$d_mcdr_filtered)
      isolate(values$d_mcdr_filtered) %>%
        select(all_of(table_vars()))
    },
    selection = "single",
    fillContainer = TRUE,
    options = list(
      dom = "t",
      pageLength = 10000,
      stateSave = TRUE,
      stateDuration = 0,
      order = list(),
      scrollCollapse = TRUE,
      drawCallback = JS(
        "function(settings) {
                     var table = this.api();
                     var row = table.row('.selected');
                     if (row.node()) {
                       row.node().scrollIntoView({ block: 'nearest' });
                     }
                   }"
      ),
      columnDefs = list(
        list(visible = FALSE, targets = 0),
        list(width = '200px', targets = "_all")
      )
    ),
    rownames = TRUE,
    server = TRUE
  )

  ## Clear criteria button ----------------------------
  observeEvent(input$clear_all_criteria, {
    updateTextInput(inputId = "custom_search", value = "")
    updateTextInput(inputId = "author", value = character(0))
    updateAirDateInput(inputId = "years", value = c(NA, NA))
    updateTextInput(inputId = "title", value = character(0))
    updateTextInput(inputId = "abstract_note", value = character(0))
    updateVirtualSelect(input = "select_missing", selected = character(0))

    clear_tag_input <- function(tag) {
      updateTextInput(inputId = tag, value = character(0))
      updateRadioButtons(inputId = tag, selected = character(0))
      updateCheckboxGroupInput(inputId = tag, selected = character(0))
      updateDateInput(inputId = tag, value = NA)
    }

    values$tag_variables %>%
      map(\(x) clear_tag_input(x))
  })

  ## Select papers button ----------------------
  observeEvent(input$select_papers, {
    ### make criteria table ----------------------

    # Test if the custom filter text throws an error
    filter_condtion_test <-
      safely(filter)

    d_custom_criteria <- NULL
    custom_filter <- NULL
    #custom criteria
    if (input$custom_search != "") {
      custom_filter <- filter_condtion_test(
        values$d_mcdr_tagged,
        eval(rlang::parse_expr(input$custom_search))
      )$result
      if (is.null(custom_filter)) {
        print("bad")
        showModal(modalDialog(
          title = "Custom search problem",
          "Somthing is wrong with your filter statement. Try again!"
        ))
      } else {
        d_custom_criteria <- data.frame(
          field = "custom",
          value = input$custom_search
        )
      }
    }

    # paper criteria
    #function to add paper criteria
    add_criteria <- function(d, f, v) {
      dc <- d %>%
        bind_rows(data.frame(field = f, value = v))
      return(dc)
    }

    d_paper_criteria <- data.frame(
      field = character(0),
      value = character(0)
    ) %>%
      add_criteria("author", input$author) %>%
      add_criteria("title", input$title) %>%
      add_criteria("abstract_note", input$abstract) %>%
      filter(value != "")

    tag_df_fun <- function(tag) {
      data.frame(field = tag, value = paste(input[[tag]], collapse = ";"))
    }

    d_tag_criteria <- values$tag_variables %>%
      map(\(x) tag_df_fun(x)) %>%
      list_rbind() %>%
      filter(value != "")

    d_years_criteria <- NULL
    years <- NULL
    if (!is.null(input$years)) {
      years <- year(input$years)

      d_years_criteria <- data.frame(
        field = "publication_year",
        value = as.character(years[1])
      )
      if (length(years) == 2) {
        d_years_criteria <- data.frame(
          field = "years",
          value = paste(years[1], years[2], sep = " - ")
        )
      }
    }

    d_missing_criteria <- NULL
    if (!is.null(input$select_missing)) {
      d_missing_criteria <- data.frame(field = input$select_missing) %>%
        mutate(value = "missing")
    }

    #d_criteria <- NULL
    d_criteria <- bind_rows(
      d_custom_criteria,
      d_missing_criteria,
      d_years_criteria,
      d_paper_criteria,
      d_tag_criteria
    )

    ### render criteria table -------------------------
    output$criteria_table <- renderDT(
      d_criteria,
      selection = "none",
      options = list(dom = "t", pageLength = 10000),
      rownames = FALSE,
      server = FALSE
    )
    ### render criteria table plot -------------------------
    output$criteria_table_plot <- renderDT(
      d_criteria,
      selection = "none",
      options = list(dom = "t", pageLength = 10000),
      rownames = FALSE,
      server = FALSE
    )

    ### render criteria table plot -------------------------
    output$criteria_table_summary <- renderDT(
      d_criteria,
      selection = "none",
      options = list(dom = "t", pageLength = 10000),
      rownames = FALSE,
      server = FALSE
    )

    ### filter database  -----------------------------

    if (!is.null(custom_filter)) {
      d_filtered <- custom_filter
    } else {
      d_filtered <- values$d_mcdr_tagged
    }

    d_filtered <- d_filtered %>%
      filter(
        if (
          input$exclude_obsolete &
            "date_time_obsolete_db" %in% names(.)
        ) {
          (is.na(date_time_obsolete_db) | date_time_obsolete_db == "NA")
        } else {
          TRUE
        }
      )

    # year range is a special case
    d_criteria_paper_tag <- bind_rows(
      d_years_criteria,
      d_paper_criteria,
      d_tag_criteria
    ) %>%
      filter(field != "years")

    if (nrow(d_criteria_paper_tag) > 0) {
      for (i in 1:nrow(d_criteria_paper_tag)) {
        choices <- NA
        if (!is.na(d_criteria_paper_tag$value[i])) {
          choices <- paste(
            "\\b",
            str_trim(str_split_1(d_criteria_paper_tag$value[i], ";")),
            "\\b",
            sep = ""
          )
        }

        d_filtered$meet_criteria <- NA
        if (nrow(d_filtered) > 0) {
          for (j in 1:nrow(d_filtered)) {
            if (!is.na(d_criteria_paper_tag$field[i])) {
              d_filtered$meet_criteria[j] <-
                any(str_detect(
                  d_filtered[[d_criteria_paper_tag$field[i]]][j],
                  choices
                ))
            }
          }
        }

        d_filtered <-
          d_filtered[d_filtered$meet_criteria, ] %>%
          select(-meet_criteria) %>%
          remove_empty(which = "rows")
      }
    }

    #filter by year range
    if ("years" %in% d_criteria$field) {
      d_filtered <- d_filtered %>%
        mutate(year = as.numeric(publication_year)) %>%
        filter(year >= years[1] & year <= years[2]) %>%
        select(-year)
    }
    #
    if (!is.null(d_missing_criteria)) {
      for (i in 1:length(d_missing_criteria$field)) {
        d_filtered <- d_filtered %>%
          filter(
            is.na(.data[[d_missing_criteria$field[i]]]) |
              .data[[d_missing_criteria$field[i]]] == "NA"
          )
      }
    }

    values$d_mcdr_filtered <- d_filtered

    # surgically update the data without re-rendering the whole widget
    replaceData(
      dt_proxy,
      values$d_mcdr_filtered %>%
        select(all_of(table_vars())),
      resetPaging = FALSE,
      rownames = TRUE
    )

    ### render filter summary ----------------------------
    output$n_papers_selected <- renderText(paste(
      "Number of papers selected:",
      nrow(values$d_mcdr_filtered)
    ))
  })

  ## Show row selected paper info ---------------------
  observeEvent(input$table_rows_selected, {
    table_rows_selected <- input$table_rows_selected

    selected_row_data <- values$d_mcdr_filtered %>%
      slice(table_rows_selected)

    categories <- values$categories %>%
      list_modify("notes" = zap())

    tags_text <- ""
    for (i in 1:length(categories)) {
      tags_text <- paste(
        tags_text,
        "<b>",
        names(categories)[i],
        "</b><br>",
        sep = ""
      )
      for (j in 1:length(names(categories[[i]]))) {
        tag_name <- names(categories[[i]])[j]
        tags_text <- paste(tags_text, "<b>", tag_name, ":</b>", sep = "")
        tags_text <- paste(
          tags_text,
          " ",
          str_replace_all(selected_row_data[[tag_name]], ";", "; "),
          "<br>",
          sep = ""
        )
        if (j == length(names(categories[[i]]))) {
          tags_text <- paste(tags_text, "<br>", sep = "")
        }
      }
    }

    notes_text <- ""

    if (values$has_notes) {
      # notes_variables <- values$categories$notes %>%
      #   pull("notes")

      for (i in 1:length(values$notes_variables)) {
        notes_text <- paste(
          notes_text,
          "<b>",
          values$notes_variables[i],
          ":</b><br>",
          sep = ""
        )
        notes_text <- paste(
          notes_text,
          selected_row_data[[values$notes_variables[i]]],
          "<br><br>",
          sep = ""
        )
      }
    }

    showModal(modalDialog(
      title = selected_row_data$title,
      HTML("<b>Year:</b> ", selected_row_data$publication_year, "<br>"),
      HTML("<b>Author(s):</b> ", selected_row_data$author, "<br><br>"),
      HTML(tags_text),
      HTML(notes_text),
      HTML("<b>Abstract:</b> ", selected_row_data$abstract_note, "<br>"),
      size = "l",
      easyClose = TRUE
    ))
  })

  ## download selected csv ----------------------------
  output$export_selected_csv <- downloadHandler(
    filename = function() {
      paste(input$export_filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$d_mcdr_filtered, file, row.names = FALSE)
    }
  )
}
