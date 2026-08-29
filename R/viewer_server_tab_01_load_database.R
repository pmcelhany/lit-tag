# R/viewer_server_tab_01_load_database.R

viewer_server_tab_01_load_database <- function() {
  ## render database chooser
  output$db_chooser <- renderUI({
    fileInput(
      ns("database_csv"),
      h4("Database File"),
      multiple = FALSE,
      accept = c(".csv")
    )
  })
  output$cat_chooser <- renderUI({
    fileInput(
      ns("categories_excel"),
      h4("Categories File"),
      multiple = FALSE,
      accept = c(".xls", ".xlsx")
    )
  })

  ## Load data button ---------------------------------------
  observeEvent(input$load_data, {
    values$d_mcdr_tagged <- NULL
    values$categories <- NULL
    values$d_category_meta <- NULL
    values$d_mcdr_filtered <- NULL
    selectRows(dt_proxy, selected = NULL)

    #show dialog if database or category file missing
    if (
      is.null(input$database_csv$datapath) |
        is.null(input$categories_excel$datapath)
    ) {
      showModal(modalDialog(title = "Select database and category files."))
    } else {
      withProgress(message = "Loading data", value = 0, {
        incProgress(1 / 4)

        ### File paths -------------------------

        # get filepath from user selection in chooser
        database_file_path <- input$database_csv$datapath
        category_file_path <- input$categories_excel$datapath

        ### Load category data --------------------------------------

        values$categories_with_meta <- category_file_path %>%
          excel_sheets() %>%
          purrr::set_names() %>%
          map(\(x) read_excel(category_file_path, sheet = x))

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

        # vector of number tag variables
        values$number_tags <- NULL
        if ("number" %in% values$d_category_meta$select_type) {
          values$number_tags <- values$d_category_meta %>%
            filter(select_type == "number") %>%
            pull(cat_label) %>%
            make_clean_names()
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
        values$notes_variables <- NULL
        if (values$has_notes) {
          values$notes_variables <- values$categories$notes %>%
            pull("notes")
        }
        incProgress(2 / 4)

        ### Load database ----------------------------------------------
        # funcion to make short publication name
        journal_abrev_fun <- function(s) {
          a <- NA
          if (!is.na(s)) {
            a <- paste(
              str_sub(str_split_1(s, pattern = " "), 1, 4),
              collapse = "_"
            )
          }
          return(a)
        }

        values$d_mcdr_tagged <- read_csv(database_file_path) %>%
          mutate(across(everything(), as.character)) %>%
          remove_empty(which = "rows") %>%
          mutate(
            pub_name_short = .data[["publication_title"]] %>%
              map(\(x) journal_abrev_fun(x)) %>%
              unlist()
          ) %>%
          mutate(first_author = word(author, sep = ";")) %>%
          mutate(
            first_author = paste(
              str_trim(word(first_author, sep = ",")),
              str_sub(str_trim(word(author, 2, 2, sep = ",")), 1, 1),
              sep = "_"
            )
          ) %>%
          arrange(author, publication_year)

        ### Add new tags to database. -----------------------------------
        # If there are tags in the categories file that are not in database,
        # the new tags need to be added

        new_tags <- c(values$tag_variables[
          !(values$tag_variables %in%
            names(values$d_mcdr_tagged))
        ])

        values$d_mcdr_tagged[new_tags] <- NA

        ### Check if number tags include non-number values
        # Loaded DB Validation: check for non-numeric data in "number" tags
        number_tags <- row.names(values$d_category_meta)[
          values$d_category_meta$select_type == "number"
        ]
        invalid_loaded_tags <- c()
        if (length(number_tags) > 0) {
          for (t in number_tags) {
            if (t %in% names(values$d_mcdr_tagged)) {
              vals <- unique(values$d_mcdr_tagged[[t]])
              has_invalid <- any(vapply(
                vals,
                is_invalid_numeric,
                FUN.VALUE = logical(1)
              ))
              if (has_invalid) {
                lbl <- values$d_category_meta[t, "cat_label"]
                invalid_loaded_tags <- c(invalid_loaded_tags, lbl)
              }
            }
          }
        }
        # Show dialog if number tags have non-number values
        if (length(invalid_loaded_tags) > 0) {
          shiny::showModal(shiny::modalDialog(
            title = "Invalid Numeric Data in Loaded Database",
            shiny::tagList(
              shiny::p(
                "The loaded database contains non-numeric data for the following tags which are expecting only numeric values:"
              ),
              shiny::tags$ul(
                lapply(invalid_loaded_tags, function(tag_name) {
                  shiny::tags$li(tag_name)
                })
              ),
              shiny::p(
                "For histogram plots, non-numeric values are ignored (i.e. treated as missing). For all other features in lit-tag-viewer, the all values for these tags are treated as text."
              )
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("OK")
          ))
        }

        ######################

        ### Filter database ---------------
        # the d_mcdr_filtered dataframe is the filtered data shown in table
        values$d_mcdr_filtered <- values$d_mcdr_tagged %>%
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

        output$n_papers_selected <- renderText(paste(
          "Number of papers selected:",
          nrow(values$d_mcdr_filtered)
        ))

        incProgress(3 / 4)

        ### Full  table -----------------------------------------
        output$table_full <- DT::renderDataTable(
          {
            req(values$d_mcdr_tagged)
            values$d_mcdr_tagged %>%
              select(author, publication_year, title)
          },
          selection = "none",
          options = list(
            dom = "t",
            pageLength = 10000,
            stateSave = TRUE,
            stateDuration = 0,
            order = list(),
            columnDefs = list(
              list(visible = FALSE, targets = 0),
              list(width = '20px', targets = "_all")
            ),
            scrollX = TRUE,
            scrollY = TRUE
          ),
          rownames = TRUE,
          server = TRUE
        )

        ### Plot x variables dropdown -----------------

        plot_paper_fields <- c(
          "item_type",
          "publication_year",
          "author",
          "title",
          "pub_name_short",
          "first_author",
          "extra"
        )

        cat_without_notes <- values$categories %>%
          list_modify(notes = zap())

        plot_opt_list <- names(cat_without_notes) %>%
          purrr::set_names() %>%
          map(\(x) names(cat_without_notes[[x]])) %>%
          list_assign(paper_fields = plot_paper_fields)

        opt_list_name_order <- c(
          "paper_fields",
          names(plot_opt_list)[1:length(plot_opt_list) - 1]
        )

        plot_opt_list_sorted <- opt_list_name_order %>%
          purrr::set_names() %>%
          map(\(x) plot_opt_list[[x]])

        updateVirtualSelect(
          inputId = "plot_x_var",
          choices = plot_opt_list_sorted,
          selected = "publication_year"
        )

        ### Plot stack variables dropdown -----------------

        updateVirtualSelect(
          inputId = "plot_stack_var",
          choices = c("none", plot_opt_list_sorted),
          selected = "none"
        )

        ### Select missing value -----------

        updateVirtualSelect(
          inputId = "select_missing",
          choices = plot_opt_list_sorted,
          selected = "none"
        )

        ### Summary table variables dropdown ------------------

        summary_tbl_paper_fields <- c(
          plot_paper_fields,
          "doi",
          "url",
          "extra"
        )

        summary_opt_list <- names(cat_without_notes) %>%
          purrr::set_names() %>%
          map(\(x) names(cat_without_notes[[x]])) %>%
          list_assign(paper_fields = summary_tbl_paper_fields)

        if (!is.null(values$notes_variables)) {
          summary_opt_list <- summary_opt_list %>%
            list_assign(notes = values$notes_variables)
        }

        # summary_opt_list_name_order <- c(
        #   "paper_fields",
        #   names(summary_opt_list)[1:length(summary_opt_list) - 1]
        # )

        summary_opt_list_name_order <- c(
          "paper_fields",
          names(summary_opt_list)[
            "paper_fields" != names(summary_opt_list)
          ]
        )

        summary_opt_list_sorted <- summary_opt_list_name_order %>%
          purrr::set_names() %>%
          map(\(x) summary_opt_list[[x]])

        updateVirtualSelect(
          inputId = "summary_var",
          choices = c(summary_opt_list_sorted),
          selected = c("author", "publication_year", "title")
        )

        ### Add tag input to ui --------------------------------------
        # remove old tag ui
        # if you don't do this and press the load button after a db is already loaded,
        # you will just add another set of tags to  UI, which is not good

        walk(
          values$active_cat_tabs,
          ~ nav_remove(id = "tag_tabs", target = .x)
        )
        #insert tag panels
        names(values$categories)["notes" != names(values$categories)] %>%
          #stringr::str_subset("notes", negate = TRUE) %>%
          map(\(x) {
            nav_insert(
              id = "tag_tabs",
              nav_panel(
                x,
                card(
                  card_header(x),
                  card_body(fluidRow(
                    names(
                      values$categories %>%
                        pluck(x)
                    ) %>%
                      map(\(y) {
                        select_box_fun(
                          x,
                          y,
                          cat = values$categories,
                          meta = values$d_category_meta
                        )
                      })
                  ))
                )
              )
            )
          })
        # Reset vector of active categories and tag UI elements
        values$active_cat_tabs <-
          names(values$categories)[names(values$categories) != "notes"]
        ### Add notes input to ui  ------------------------------------
        if (!is.null(values$notes_variables)) {
          output$notes <- renderUI({
            values$notes_variables %>%
              map(\(x) textInput(ns(x), x))
          })
        } else {
          output$notes <- renderUI({
            return(NULL)
          })
        }

        ### Render n paper in selection summary -------------
        output$n_papers_db <- renderText(paste(
          "Number of papers in db:",
          nrow(values$d_mcdr_tagged)
        ))

        incProgress(4 / 4)
        values$table_trigger <- values$table_trigger + 1
      })
    }
  })

  ## Download database, tag cat, and ris file --------------
  output$download_db <- downloadHandler(
    filename = function() {
      paste("lit-tag-database", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(values$d_mcdr_tagged, file)
    }
  )

  output$download_tag_cat <- downloadHandler(
    filename = function() {
      paste("lit-tag-categories", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(values$categories_with_meta, file)
    }
  )
}
