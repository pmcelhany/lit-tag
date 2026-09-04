#' The lit-tag-builder server
#'
#' @param id module ID.
#'
#' @import tidyverse
#' @import janitor
#' @import shiny
#' @import shinyWidgets
#' @import waiter
#' @import bslib
#' @import DT
#' @import readxl
#' @import magrittr
#' @import xml2
#' @import stringr
#' @import purrr
#' @import lubridate
#' @import readr
#' @import dplyr
#' @import tidyr
#' @noRd

# builder server function ---------------------------------------
builder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Rebind environments of functions defined in other files
    environment(select_box_fun) <- environment()
    environment(load_categories) <- environment()

    environment(render_paper_info) <- environment()
    environment(save_tag_value) <- environment()
    environment(save_last_row) <- environment()
    environment(load_row_tags_fun) <- environment()
    environment(filter_fun) <- environment()

    environment(read_as_char) <- environment()

    environment(unescape_html) <- environment()
    environment(ris_fun) <- environment()

    # Initialize server logic from tab-specific files
    environment(builder_server_tab_02_new_database) <- environment()
    builder_server_tab_02_new_database()

    environment(builder_server_tab_03_sync_zotero) <- environment()
    builder_server_tab_03_sync_zotero()

    environment(builder_server_tab_04_database_maintenance) <- environment()
    builder_server_tab_04_database_maintenance()

    environment(builder_server_tab_05_new_zotero) <- environment()
    builder_server_tab_05_new_zotero()

    environment(builder_server_tab_06_help) <- environment()
    builder_server_tab_06_help()

    # Inject JS for resizable panels in the Tag edit tab
    insertUI(
      selector = "head",
      where = "beforeEnd",
      ui = tagList(
        tags$script(HTML(paste0(
          "
          (function() {
            function initResizer() {
              const containers = document.querySelectorAll('bslib-layout-columns');
              for (const container of containers) {
                if (container.classList.contains('resizable-container')) continue;

                const panels = Array.from(container.querySelectorAll(':scope > .bslib-grid-item'));
                if (panels.length !== 3) continue;

                // Check if it's the right one by looking at headers
                const h3s = Array.from(container.querySelectorAll('h3')).map(h => h.textContent.trim());
                const isTagEdit = h3s.includes('Paper table') &&
                                  h3s.includes('Paper info and notes') &&
                                  h3s.includes('Tags');

                if (!isTagEdit) continue;

                container.classList.add('resizable-container');

                // Set initial flex values
                panels.forEach(p => {
                  p.style.flex = '1 1 0px';
                });

                for (let i = 0; i < panels.length - 1; i++) {
                  const leftPanel = panels[i];
                  const rightPanel = panels[i+1];
                  const gutter = document.createElement('div');
                  gutter.className = 'gutter';
                  leftPanel.after(gutter);

                  gutter.addEventListener('mousedown', function(e) {
                    e.preventDefault();
                    const startX = e.pageX;
                    const leftWidth = leftPanel.getBoundingClientRect().width;
                    const rightWidth = rightPanel.getBoundingClientRect().width;
                    const totalWidth = leftWidth + rightWidth;

                    // Use getComputedStyle to get the actual flex-grow value
                    const leftFlex = parseFloat(window.getComputedStyle(leftPanel).flexGrow) || 1;
                    const rightFlex = parseFloat(window.getComputedStyle(rightPanel).flexGrow) || 1;
                    const totalFlex = leftFlex + rightFlex;

                    gutter.classList.add('dragging');
                    document.body.style.cursor = 'col-resize';

                    function onMouseMove(e) {
                      const deltaX = e.pageX - startX;
                      let newLeftWidth = leftWidth + deltaX;
                      let newRightWidth = rightWidth - deltaX;

                      if (newLeftWidth < 100) {
                        newLeftWidth = 100;
                        newRightWidth = totalWidth - 100;
                      }
                      if (newRightWidth < 100) {
                        newRightWidth = 100;
                        newLeftWidth = totalWidth - 100;
                      }

                      const newLeftFlex = (newLeftWidth / totalWidth) * totalFlex;
                      const newRightFlex = (newRightWidth / totalWidth) * totalFlex;

                      leftPanel.style.flex = newLeftFlex + ' 1 0px';
                      rightPanel.style.flex = newRightFlex + ' 1 0px';
                    }

                    function onMouseUp() {
                      gutter.classList.remove('dragging');
                      document.body.style.cursor = '';
                      document.removeEventListener('mousemove', onMouseMove);
                      document.removeEventListener('mouseup', onMouseUp);
                    }

                    document.addEventListener('mousemove', onMouseMove);
                    document.addEventListener('mouseup', onMouseUp);
                  });
                }
              }
            }

            $(document).on('shown.bs.tab', function() {
              setTimeout(initResizer, 200);
            });

            const observer = new MutationObserver((mutations) => {
              for (const mutation of mutations) {
                if (mutation.addedNodes.length) {
                  initResizer();
                }
              }
            });

            $(document).ready(function() {
              observer.observe(document.body, { childList: true, subtree: true });
              setTimeout(initResizer, 1000); // Increased timeout to be safe
            });
          })();
          "
        )))
      ),
      immediate = TRUE
    )

    ## waiter --------------------------------------
    w <- Waiter$new(html = spin_3(), color = transparent(.5))

    ## Reactive values --------------------------------------------
    values <- reactiveValues(
      d_mcdr_tagged = NULL,
      categories = NULL,
      d_category_meta = NULL,
      d_mcdr_filtered = NULL,
      default_filter_var = character(0),
      last_key = NULL,
      inspire_quotes = NULL,
      inspire_images = NULL,
      d_content_db = NULL,
      d_old_key_db = NULL,
      d_split_db = NULL,
      tag_variables = NULL,
      bib_table_col = c("first_author", "publication_year", "title"),
      bib_sort_column = "first_author",
      bib_sort_dir = "asc",
      active_cat_tabs = character(0),
      active_tag_ui = character(0),
      table_trigger = 0,
      has_notes = FALSE,
      number_observers = list(),
      number_last_val = list()
    )

    ## Proxy for the papers table ------------------------------
    dt_proxy <- DT::dataTableProxy("table")

    # Capture user sort order
    observeEvent(input$table_order_manual, {
      req(input$table_order_manual)
      # input$table_order_manual is usually a list of lists: [[0, "asc"]] or [[column: 0, dir: "asc"]]
      order_info <- input$table_order_manual[[1]]

      col_idx <- NA
      dir <- NA

      if (is.list(order_info)) {
        if (!is.null(order_info$column)) {
          col_idx <- as.numeric(order_info$column)
          dir <- as.character(order_info$dir)
        } else {
          col_idx <- as.numeric(order_info[[1]])
          dir <- as.character(order_info[[2]])
        }
      } else if (is.vector(order_info)) {
        col_idx <- as.numeric(order_info[1])
        dir <- as.character(order_info[2])
      }

      if (!is.na(col_idx) && col_idx < length(values$bib_table_col)) {
        values$bib_sort_column <- values$bib_table_col[col_idx + 1]
        # Ensure direction is valid and not reversed
        values$bib_sort_dir <- if (identical(dir, "desc")) "desc" else "asc"
      }
    })

    ### Bibliography table -----------------------------------------
    output$table <- renderDT(
      {
        # trigger update when data is initially loaded or filtered or when show_extra changes
        values$table_trigger
        req(values$d_mcdr_filtered)

        d_filtered <- isolate(values$d_mcdr_filtered)
        bib_cols <- isolate(values$bib_table_col)
        req(all(bib_cols %in% names(d_filtered)))

        curr_sort_col <- isolate(values$bib_sort_column)
        curr_sort_dir <- isolate(values$bib_sort_dir)
        col_idx <- match(curr_sort_col, bib_cols) - 1
        if (is.na(col_idx)) {
          col_idx <- 0
        }

        # Determine selected row index
        sel_row <- NULL
        last_key <- isolate(values$last_key)
        if (!is.null(last_key) && "key" %in% names(d_filtered)) {
          sel_row <- which(d_filtered$key == last_key)
        }

        datatable(
          d_filtered %>%
            select(all_of(bib_cols)),
          selection = list(mode = "single", selected = sel_row),
          callback = JS(paste0(
            "
          table.on('select', function() {
            if (typeof table.centerRow === 'function') table.centerRow(true);
          });
          table.on('order.dt', function() {
            var order = table.order();
            Shiny.setInputValue('",
            ns("table_order_manual"),
            "', order);
          });
        "
          )),
          options = list(
            dom = "t",
            pageLength = 10000,
            stateSave = TRUE,
            stateDuration = 0,
            order = list(list(col_idx, curr_sort_dir)),
            scrollY = "600px",
            scrollCollapse = TRUE,
            stateLoadParams = JS(
              "function(settings, data) {
                         delete data.order;
                       }"
            ),
            stateSaveParams = JS(
              "function(settings, data) {
                         delete data.order;
                       }"
            ),
            drawCallback = JS(
              "function(settings) {
                     var table = this.api();
                     table.centerRow = function(animate) {
                       var row = table.row('.selected').node();
                       if (!row) return;
                       var $scrollBody = $(row).closest('.dataTables_scrollBody');
                       if ($scrollBody.length) {
                         var container = $scrollBody[0];
                         var containerRect = container.getBoundingClientRect();
                         var rowRect = row.getBoundingClientRect();
                         var relativeTop = rowRect.top - containerRect.top;
                         var currentScroll = container.scrollTop;
                         var target = currentScroll + relativeTop - (container.clientHeight / 2) + (row.offsetHeight / 2);
                         if (animate) {
                           $scrollBody.stop().animate({ scrollTop: Math.max(0, target) }, 200);
                         } else {
                           $scrollBody.scrollTop(Math.max(0, target));
                         }
                       }
                     };
                     setTimeout(function() {
                       table.centerRow(false);
                     }, 200);
                   }"
            )
          ),
          rownames = FALSE
        )
      },
      server = FALSE
    )

    ## Load data button ---------------------------------------
    observeEvent(input$load_data, {
      values$d_mcdr_tagged <- NULL
      values$categories <- NULL
      values$d_category_meta <- NULL
      values$d_mcdr_filtered <- NULL
      values$default_filter_var <- character(0)
      values$last_key <- NULL
      selectRows(dt_proxy, selected = NULL)

      #show dialog if database or category file missing
      if (
        is.null(input$database_csv$datapath) |
          is.null(input$categories_excel$datapath)
      ) {
        showModal(modalDialog(title = "Select database and category files."))
      } else {
        # Perform file format validation checks before loading data
        enable_validation <- TRUE
        if (exists("ENABLE_FILE_VALIDATION")) {
          enable_validation <- ENABLE_FILE_VALIDATION
        }
        enable_validation <- getOption("lit_tag_enable_validation", enable_validation)

        if (isTRUE(enable_validation)) {
          # Validate Database CSV
          db_val_res <- validate_database_csv(input$database_csv$datapath)
          if (!isTRUE(db_val_res)) {
            showModal(modalDialog(
              title = "Database File Validation Error",
              tags$div(
                tags$p("The uploaded database CSV file failed validation check(s):"),
                tags$p(style = "color: red; font-weight: bold;", db_val_res)
              ),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
            return()
          }

          # Validate Categories Excel
          cat_val_res <- validate_categories_xlsx(input$categories_excel$datapath)
          if (!isTRUE(cat_val_res)) {
            showModal(modalDialog(
              title = "Categories File Validation Error",
              tags$div(
                tags$p("The uploaded categories Excel file failed validation check(s):"),
                tags$p(style = "color: red; font-weight: bold;", cat_val_res)
              ),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
            return()
          }
        }

        withProgress(message = "Loading data", value = 0, {
          ### Load category data ---------------------------------------
          incProgress(1 / 4)

          load_categories(input$categories_excel$datapath)

          #vector of notes variables
          notes_variables <- NULL
          if (values$has_notes) {
            notes_variables <- values$categories$notes %>%
              pull("notes")
          }

          tag_variables <- values$tag_variables
          categories_with_meta <- values$categories_with_meta
          d_category_meta <- values$d_category_meta
          categories <- values$categories

          # vector of date tags
          date_fields <- d_category_meta %>%
            filter(select_type == "date") %>%
            row.names()

          incProgress(2 / 4)

          ### Load database ----------------------------------------------

          # the d_mcdr_tagged dataframe always keeps all data
          values$d_mcdr_tagged <- read_csv(input$database_csv$datapath) %>%
            mutate(across(everything(), as.character))

          #add "extra" column if it does not already exist
          if (!("extra" %in% names(values$d_mcdr_tagged))) {
            values$d_mcdr_tagged <- values$d_mcdr_tagged %>%
              mutate(extra = "")
          }

          #add "date_time_obsolete_db" column if it does not already exist
          if (!("date_time_obsolete_db" %in% names(values$d_mcdr_tagged))) {
            values$d_mcdr_tagged <- values$d_mcdr_tagged %>%
              mutate(date_time_obsolete_db = NA_character_)
          }

          # if there is no "notes" column in the original zotero file, it needs added
          # this is a bit of hack to deal with the fact that oned of the category tabs is named "notes"
          # which is also a potential field in zotero.
          # the rest of the code in the app deals with this issue, but it
          # depends on the existnace of a "notes column.
          # there are more graceful ways to do this...
          if (!("notes" %in% names(values$d_mcdr_tagged))) {
            values$d_mcdr_tagged$notes <- "NA"
          }

          ### Add new tags to database. -----------------------------------
          # If there are tags in the categories file that are not in database,
          # the new tags need to be added
          new_tags <- c(tag_variables[
            !(tag_variables %in%
              names(values$d_mcdr_tagged))
          ])

          values$d_mcdr_tagged[new_tags] <- NA

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
          if (length(invalid_loaded_tags) > 0) {
            shiny::showModal(shiny::modalDialog(
              title = "Invalid Numeric Data in Loaded Database",
              shiny::tagList(
                shiny::p(
                  "The loaded database contains non-numeric data for the following tags which require numeric values:"
                ),
                shiny::tags$ul(
                  lapply(invalid_loaded_tags, function(tag_name) {
                    shiny::tags$li(tag_name)
                  })
                ),
                shiny::p(
                  "Change the non-numeric values to numbers.\nThe non-numeric entries can be found by filtering the database."
                )
              ),
              easyClose = TRUE,
              footer = shiny::modalButton("OK")
            ))
          }

          # Clear old number observers to avoid leaking observers
          if (!is.null(values$number_observers)) {
            for (obs in values$number_observers) {
              tryCatch(
                {
                  obs$destroy()
                },
                error = function(e) NULL
              )
            }
          }
          values$number_observers <- list()
          values$number_last_val <- list()

          # Set up new live-edit observers for each "number" tag
          if (length(number_tags) > 0) {
            values$number_observers <- lapply(number_tags, function(y) {
              observeEvent(
                input[[y]],
                {
                  val <- input[[y]]
                  last_val <- values$number_last_val[[y]]

                  # If val is NULL, ignore
                  if (is.null(val)) {
                    return()
                  }

                  # If identical to last known val, ignore
                  if (identical(val, last_val)) {
                    return()
                  }

                  if (is_invalid_numeric(val)) {
                    shiny::showModal(shiny::modalDialog(
                      title = "Invalid Input",
                      paste(
                        "The value entered for tag '",
                        values$d_category_meta[y, "cat_label"],
                        "' must be a numeric entry.",
                        sep = ""
                      ),
                      easyClose = TRUE,
                      footer = shiny::modalButton("OK")
                    ))
                    revert_val <- if (
                      is.null(last_val) || is.na(last_val) || last_val == "NA"
                    ) {
                      ""
                    } else {
                      last_val
                    }
                    shiny::updateTextInput(session, y, value = revert_val)
                  } else {
                    values$number_last_val[[y]] <- val
                    shiny::updateTextInput(
                      session,
                      y,
                      label = values$d_category_meta[y, "cat_label"]
                    )
                  }
                },
                ignoreInit = TRUE
              )
            })
          }

          ### Filter database --------
          # the d_mcdr_filtered dataframe is the filtered data shown in table
          values$table_trigger <- values$table_trigger + 1
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

          # trigger table render on initial load
          values$table_trigger <- values$table_trigger + 1

          incProgress(3 / 4)

          ### Add notes input to ui  ------------------------------------
          if (!is.null(notes_variables)) {
            output$notes <- renderUI({
              notes_variables %>%
                map(\(x) {
                  textAreaInput(ns(x), x, width = 600, height = 200)
                })
            })
          } else {
            output$notes <- renderUI({
              return(NULL)
            })
          }

          ### Add tag input to ui --------------------------------------
          # remove old tag ui
          # if you don't do this and press the load button after a db is already loaded,
          # you will just add another set of tags to  UI, which is not good

          walk(
            values$active_cat_tabs,
            ~ nav_remove(id = "tag_tabs", target = .x)
          )
          #walk(values$active_tag_ui, ~ nav_remove(id = "my_navset", target = .x))

          #insert tag panels
          names(values$categories) %>%
            stringr::str_subset("notes", negate = TRUE) %>%
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

          ### Show selected paper info -------------------------------------
          output$selected_year <- render_paper_info("Year:", "publication_year")
          output$selected_author <- render_paper_info("Authors:", "author")
          output$selected_title <- render_paper_info("Title:", "title")
          output$selected_journal <- render_paper_info(
            "Journal:",
            "publication_title"
          )

          ### Select filter variables dropdown  -------------------------------

          output$n_db <- renderText(paste(
            "Papers in database:",
            nrow(values$d_mcdr_tagged)
          ))
          output$n_filtered_db <- renderText(paste(
            "Papers in filtered database:",
            nrow(values$d_mcdr_filtered)
          ))

          paper_fields <- c("item_type", "publication_year", "first_author")

          cat_without_notes <- values$categories %>%
            list_modify(notes = rlang::zap())

          plot_opt_list <- names(cat_without_notes) %>%
            purrr::set_names() %>%
            map(\(x) names(cat_without_notes[[x]])) %>%
            list_assign(paper_fields = paper_fields)

          opt_list_name_order <- c(
            "paper_fields",
            names(plot_opt_list)[1:length(plot_opt_list) - 1]
          )

          filter_opt_list_sorted <- opt_list_name_order %>%
            purrr::set_names() %>%
            map(\(x) plot_opt_list[[x]])

          default_filter_var <- character(0)
          if (!identical(values$default_filter_var, character(0))) {
            default_filter_var <- str_split_1(values$default_filter_va, ";")
          }

          updateVirtualSelect(
            inputId = "filter_var",
            choices = filter_opt_list_sorted,
            selected = default_filter_var
          )

          incProgress(4 / 4)
        })
      }
    })

    ## Observe show extra --------------------------------
    observeEvent(c(input$show_extra, input$load_data), ignoreInit = TRUE, {
      if (input$show_extra) {
        values$bib_table_col <- c(
          "first_author",
          "publication_year",
          "title",
          "extra"
        )
        output$selected_extra <- render_paper_info("Extra:", "extra")
      } else {
        values$bib_table_col <- c("first_author", "publication_year", "title")
        output$selected_extra <- NULL
      }
      # trigger table update as column structure changed
      values$table_trigger <- values$table_trigger + 1
    })

    ## Observe select filter fields  -------------------------------
    observeEvent(input$filter_var, {
      ### Render UI of filters -----------------------------
      output$filters <- renderUI({
        d_tagged <- isolate(values$d_mcdr_tagged)
        input$filter_var %>%
          map(\(x) {
            id <- paste("filter", x, sep = "_")
            checkboxGroupInput(
              ns(id),
              id,
              unique(
                d_tagged %>%
                  pull(x) %>%
                  replace_na("NA")
              ) %>%
                sort(),
              selected = isolate(input[[id]]),
              inline = TRUE
            )
          })
      })
    })

    ## Observe filter button ----------------------------
    observeEvent(input$filter_db, {
      values$d_mcdr_filtered <- values$d_mcdr_tagged %>%
        filter(
          if (input$exclude_obsolete) {
            (is.na(date_time_obsolete_db) |
              date_time_obsolete_db == "NA")
          } else {
            TRUE
          }
        )

      input$filter_var %>%
        map(\(x) filter_fun(x))

      # trigger re-render on search/filter
      values$table_trigger <- values$table_trigger + 1
    })

    ## Observe show all button  -------------------------
    observeEvent(input$show_all_db, {
      values$d_mcdr_filtered <- values$d_mcdr_tagged

      updateVirtualSelect(inputId = "filter_var", selected = character(0))

      # trigger re-render
      values$table_trigger <- values$table_trigger + 1
    })

    ## Observe unselect filters button ------------------------
    observeEvent(input$unselect_filters, {
      input$filter_var %>%
        map(\(x) {
          updateCheckboxGroupInput(
            inputId = paste("filter", x, sep = "_"),
            selected = character(0)
          )
        })
    })

    ## Show abstract button -------------------------------
    observeEvent(input$show_abstract, {
      showModal(modalDialog(
        title = values$d_mcdr_filtered %>%
          slice(input$table_rows_selected) %>%
          pull("title"),
        values$d_mcdr_filtered %>%
          slice(input$table_rows_selected) %>%
          pull("abstract_note"),
        size = "l"
      ))
    })

    ## Observe changes to row event ----------------------
    observeEvent(
      input$table_rows_selected,
      {
        w$show()
        on.exit(w$hide())

        table_rows_selected <- input$table_rows_selected

        current_key <- NULL
        if (length(table_rows_selected) > 0) {
          current_key <- values$d_mcdr_filtered %>%
            slice(table_rows_selected) %>%
            pull(key)
        }

        last_key <- values$last_key
        d_category_meta <- values$d_category_meta

        d_notes <- NULL
        if (values$has_notes) {
          d_notes <- values$categories$notes
        }

        #just the tag fields (i.e. not notes)
        tags <- rownames(d_category_meta)[rownames(d_category_meta) != "notes"]

        if (is.null(last_key)) {
          if (!is.null(current_key)) {
            # load selected row tags
            tags %>%
              map(\(x) {
                load_row_tags_fun(x, d_category_meta, table_rows_selected)
              })

            #load selected row notes
            if (values$has_notes) {
              d_notes %>%
                pull("notes") %>%
                map(\(x) {
                  updateTextAreaInput(
                    inputId = x,
                    value = values$d_mcdr_filtered %>%
                      slice(table_rows_selected) %>%
                      pull(x)
                  )
                })
            }
          }

          values$last_key <- current_key
        } else if (!identical(current_key, last_key)) {
          # update database with last selected rows data
          # selecting a new row tiggers the saving of the last rows input data

          save_last_row(last_key, d_category_meta, d_notes)

          if (!is.null(current_key)) {
            # load selected row tags
            tags %>%
              map(\(x) {
                load_row_tags_fun(x, d_category_meta, table_rows_selected)
              })

            #load selected row notes
            if (values$has_notes) {
              d_notes %>%
                pull("notes") %>%
                map(\(x) {
                  updateTextAreaInput(
                    inputId = x,
                    value = values$d_mcdr_filtered %>%
                      slice(table_rows_selected) %>%
                      pull(x)
                  )
                })
            }

            #need for some reason to make sure it does not loose
            #highlighting the current row
            selectRows(dt_proxy, table_rows_selected)
          }

          # change the last key to the current row
          # this will be used to save any data changes when a new row is selected
          values$last_key <- current_key
        }
      },
      ignoreNULL = FALSE
    )

    ## Download edits button ------------------
    output$download_edits <- downloadHandler(
      filename = function() {
        base_name <- str_remove(input$database_csv$name, ".csv")
        if (input$remove_timestamps & str_detect(base_name, "_UTC")) {
          n_ts_words <- (5 * str_count(base_name, "_UTC")) + 1
          base_name <- word(base_name, 1, -n_ts_words, sep = "_")
        }
        file_name <- paste(
          base_name,
          "_",
          format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
          ".csv",
          sep = ""
        )
        return(file_name)
      },
      content = function(file) {
        if (!is.null(input$table_rows_selected)) {
          values$last_key <- values$d_mcdr_filtered %>%
            slice(input$table_rows_selected) %>%
            pull(key)

          d_notes <- NULL
          if (values$has_notes) {
            d_notes <- values$categories$notes
          }

          save_last_row(
            values$last_key,
            values$d_category_meta,
            d_notes
          )
        }

        # the remove_leading_special_char function makes sure that
        # the are no leading characters in any of the data that
        # will cause "#NAME?" errors if the file is opened in excel
        values$d_mcdr_tagged %>%
          mutate(across(everything(), as.character)) %>%
          mutate(across(everything(), ~ remove_leading_special_char(.x))) %>%
          write_csv(file)

        #return selection to most recent row of paper table
        if (!is.null(input$table_rows_selected)) {
          selectRows(dt_proxy, input$table_rows_selected)
        }
      }
    )

    ## Database maintenance --------------------------
    ### Database content ------------------------------
    observeEvent(input$content_db, {
      values$d_content_db <- read_csv(input$content_db$datapath)

      output$n_papers <- renderText(HTML(paste(
        "Number of papers in database: ",
        nrow(values$d_content_db),
        sep = ""
      )))

      output$db_tags_table <- renderDT(
        tag_values_in_db(values$d_content_db)$d_tag,
        selection = list(mode = "single"),
        options = list(dom = "t", pageLength = 10000),
        rownames = FALSE,
        server = FALSE,
        colnames = c("Tag name", "Number of unique values")
      )

      output$db_notes_table <- renderDT(
        data.frame(tag_values_in_db(values$d_content_db)$db_notes),
        options = list(dom = "t", pageLength = 10000),
        rownames = FALSE,
        server = FALSE,
        colnames = c("Notes name")
      )
    })

    observeEvent(input$db_tags_table_rows_selected, {
      table_rows_selected <- input$db_tags_table_rows_selected

      tag_info <- tag_values_in_db(values$d_content_db)

      selected_tag <- tag_info$d_tag$tags[table_rows_selected]

      tag_unique <- sort(tag_info$tag_options_unique[[selected_tag]])

      tag_value_string <- paste(
        values$d_content_db[[selected_tag]],
        collapse = ";"
      )

      count_unique <- tag_unique %>%
        map(\(x) str_count(tag_value_string, fixed(x))) %>%
        unlist()

      tag_unique_with_count <- paste(
        tag_unique,
        " (",
        count_unique,
        ")",
        sep = ""
      )

      showModal(modalDialog(
        title = selected_tag,
        HTML(paste(tag_unique_with_count, collapse = "<br>")),
        easyClose = TRUE
      ))
    })

    ### Compare databases --------------------------------
    observeEvent(input$compare_db, {
      d_compare_1 <- read_csv(input$compare_db_1$datapath)
      d_compare_2 <- read_csv(input$compare_db_2$datapath)

      output$papers_in_1_not_2 <- renderDT(
        d_compare_1 %>%
          filter(!(key %in% d_compare_2$key)) %>%
          select(key, first_author, publication_year, title),
        options = list(dom = "t", pageLength = 10000),
        rownames = FALSE,
        server = FALSE,
        colnames = c("Key", "First Author", "Year", "Title")
      )

      output$papers_in_2_not_1 <- renderDT(
        d_compare_2 %>%
          filter(!(key %in% d_compare_1$key)) %>%
          select(key, first_author, publication_year, title),
        options = list(dom = "t", pageLength = 10000),
        rownames = FALSE,
        server = FALSE,
        colnames = c("Key", "First Author", "Year", "Title")
      )

      output$n_papers_compare_1 <-
        renderText(HTML(paste(
          "Number of papers in database #1: ",
          nrow(d_compare_1),
          sep = ""
        )))

      output$n_papers_compare_2 <-
        renderText(HTML(paste(
          "Number of papers in database #2: ",
          nrow(d_compare_2),
          sep = ""
        )))
    })
  })
}
