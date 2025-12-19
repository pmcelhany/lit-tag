#' The lit-tag-viewer server
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
#' @import quarto
#' @importFrom scales label_wrap
#' @import writexl
#' @import stringr
#' @import purrr
#' @import lubridate
#' @import readr
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @noRd


# Shiny server function ---------------------------------------
viewer_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ## Misc functions -----------------------------------
    # function to pull out the category label and selection type metadata
    category_meta_fun <- function(d){

      d_meta <- d[1,] %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column("cat_label") %>%
         rename(select_type = V1) %>%
        set_rownames(names(d %>% clean_names()))

      return(d_meta)
    }

    # function to remove the category meta data
    category_remove_meta_fun <- function(d){
      d_cat <- d %>%
        clean_names() %>%
        mutate(row_id = 1:nrow(.)) %>%
        filter(row_id > 1) %>%
        select(-row_id)

      return(d_cat)
    }


    # The select_box_fun is a function to create an input object for each tag variable
    select_box_fun <- function(x, y, cat, meta){

      box = NULL

      choice_opts <- NULL
      if(meta[y,"select_type"] %in% c("check_box_single", "check_box_multiple")){
        choice_opts <- cat %>%
           pluck(x) %>%
           pull(y) %>%
          sort() %>%
          na.omit()
        if("not_applicable" %in% choice_opts){
          choice_opts <- choice_opts[choice_opts != "not_applicable"]
          choice_opts <- c(choice_opts, "not_applicable")
        }
      }

      if(meta[y,"select_type"] == "check_box_single"){
        box <- radioButtons(inputId = ns(y),
                            label = meta[y,"cat_label"],
                            choices = choice_opts,
                            selected = character(0))
      }

      if(meta[y,"select_type"] == "check_box_multiple"){
        box <- checkboxGroupInput(inputId = ns(y),
                                  label = meta[y,"cat_label"],
                                  choices = choice_opts
        )
      }
      if(meta[y,"select_type"] == "text_box"){
        box <- textInput(inputId = ns(y), label = meta[y,"cat_label"])
      }

      if(meta[y,"select_type"] == "date"){
        box <- dateInput(inputId = ns(y), label = meta[y,"cat_label"],
                         value = NA)
      }

      return(box)
    }

    ### newjs ------
    # function to get column order from summary table
    # code from https://stackoverflow.com/questions/59428159/in-r-shinyproxy-how-do-i-get-the-order-of-columns-from-a-dtdatatable-after-a
    newjs <- 'table.on("column-reorder", function(e, settings, details){
        var table = document.getElementById("summary_table");
        var thead = table.getElementsByTagName("thead");
        var ths = thead[0].getElementsByTagName("th");
        var tableFields = [];
        for (let i = 0; i < ths.length; i++) {
            tableFields[i] = ths[i].innerHTML;
        }
        Shiny.onInputChange("colOrder", tableFields);
    });'


    ## Reactive values --------------------------------------------
    values <- reactiveValues(d_mcdr_tagged = NULL, categories_with_meta = NULL,
                             categories = NULL,
                             tag_variables = NULL,
                             d_category_meta = NULL, d_mcdr_filtered = NULL,
                             d_plot = NULL)

    ## render database chooser
    output$db_chooser <- renderUI({
      fileInput(ns("database_csv"), h4("Database File"),
                multiple = FALSE, accept = c(".csv"))
    })
    output$cat_chooser <- renderUI({
      fileInput(ns("categories_excel"), h4("Categories File"),
                multiple = FALSE, accept = c(".xls", ".xlsx"))
    })
    # output$help_tabs <- renderUI({
    #   navset_pill(
    #     nav_panel(h3("Lit-tag-viewer"),
    #               tags$iframe(style="height:100vh; width:100%; scrolling=yes",
    #                           src="LitTag viewer guide.pdf"))
    #   )
    # })

    ## Load data button ---------------------------------------
    observeEvent(input$load_data, {

      #show dialog if database or category file missing
      if(is.null(input$database_csv$datapath) |
         is.null(input$categories_excel$datapath)){
        showModal(modalDialog(title = "Select database and category files."))
      } else{

      withProgress(message = "Loading data", value = 0, {

        incProgress(1/4)

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

        values$tag_variables <- c( row.names(values$d_category_meta) %>%
                                     stringr::str_subset("notes", negate = TRUE),
                                   values$categories$notes %>%
                                      pull("notes"))

        notes_variables <- values$categories$notes %>%
           pull("notes")

        incProgress(2/4)

        ### Load database ----------------------------------------------
        # funcion to make short publication name
        journal_abrev_fun <- function(s){
          a <- NA
          if(!is.na(s)){
            a <- paste(str_sub(str_split_1(s, pattern = " "), 1, 4),
                       collapse = "_")
          }
          return(a)
        }

        values$d_mcdr_tagged <-  read_csv(database_file_path) %>%
           mutate(across(everything(), as.character)) %>%
          remove_empty(which = "rows") %>%
           mutate(pub_name_short = .data[["publication_title"]] %>%
                    map(\(x) journal_abrev_fun(x)) %>%
                   unlist()) %>%
           mutate(first_author = word(author, sep = ";")) %>%
           mutate(first_author =
                   paste(str_trim(word(first_author, sep = ",")),
                         str_sub(str_trim(word(author, 2,2, sep = ",")), 1, 1),
                         sep = "_")) %>%
          arrange(author, publication_year)

        ### Add new tags to database. -----------------------------------
        # If there are tags in the categories file that are not in database,
        # the new tags need to be added

        new_tags <- c(values$tag_variables[!(values$tag_variables %in%
                                               names(values$d_mcdr_tagged))])

        values$d_mcdr_tagged[new_tags] <- NA

        ### Filter database ---------------
        # the d_mcdr_filtered dataframe is the filtered data shown in table
        values$d_mcdr_filtered <- values$d_mcdr_tagged %>%
           filter(if(input$exclude_obsolete &
                    "date_time_obsolete_db" %in% names(.))
            (is.na(date_time_obsolete_db) | date_time_obsolete_db == "NA") else
              TRUE)

        output$n_papers_selected <- renderText(paste("Number of papers selected:",
                                                     nrow( values$d_mcdr_filtered)))

        incProgress(3/4)

        ### Filter  table -----------------------------------------
        output$table <- renderDT(values$d_mcdr_filtered %>%
                                    select(author, publication_year, title),
                                 selection = "single",
                                 options = list(dom = "t",
                                                pageLength = 10000,
                                                #autoWidth = TRUE,
                                                columnDefs =
                                                  list(list(width =
                                                              '200px',
                                                            targets = "_all"))),
                                 rownames = FALSE, server = FALSE)

        ### Full  table -----------------------------------------
        output$table_full <- DT::renderDataTable(values$d_mcdr_tagged %>%
                                                    select(author, publication_year, title),
                                             selection = "none",
                                             options =
                                               list(dom = "t",
                                                    pageLength = 10000,
                                                    #autoWidth = TRUE
                                                    list(width =
                                                           '20px',
                                                         targets = "_all"),
                                                    scrollX = TRUE,
                                                    scrollY = TRUE),
                                             rownames = FALSE, server = FALSE)

        ### Plot x variables dropdown -----------------

        plot_paper_fields <- c("item_type", "publication_year", "author", "title",
                               "pub_name_short", "first_author")

        cat_without_notes <- values$categories %>%
           list_modify(notes = zap())

        plot_opt_list <- names(cat_without_notes) %>%
           purrr::set_names() %>%
           map(\(x) names(cat_without_notes[[x]])) %>%
           list_assign(paper_fields = plot_paper_fields)

        opt_list_name_order <- c("paper_fields",
                                 names(plot_opt_list)[1:length(plot_opt_list)-1])

        plot_opt_list_sorted <- opt_list_name_order %>%
           purrr::set_names() %>%
           map(\(x) plot_opt_list[[x]])

        updateVirtualSelect(inputId = "plot_x_var",
                            choices = plot_opt_list_sorted,
                            selected = "publication_year")

        ### Plot stack variables dropdown -----------------

        updateVirtualSelect(inputId = "plot_stack_var",
                            choices = c("none", plot_opt_list_sorted),
                            selected = "none")

        ### Select missing value -----------

        updateVirtualSelect(inputId = "select_missing",
                            choices = plot_opt_list_sorted,
                            selected = "none")

        ### Summary table variables dropdown ------------------

        summary_tbl_paper_fields <- c(plot_paper_fields, "doi", "url")

        summary_opt_list <- names(cat_without_notes) %>%
           purrr::set_names() %>%
           map(\(x) names(cat_without_notes[[x]])) %>%
          list_assign(notes = notes_variables) %>%
          list_assign(paper_fields = summary_tbl_paper_fields)


        summary_opt_list_name_order <- c("paper_fields",
                                         names(summary_opt_list)[1:length(summary_opt_list)-1])

        summary_opt_list_sorted <- summary_opt_list_name_order %>%
           purrr::set_names() %>%
           map(\(x) summary_opt_list[[x]])

        updateVirtualSelect(inputId = "summary_var",
                            choices = c(summary_opt_list_sorted),
                            selected = c("author", "publication_year", "title"))


        ### Add tag input to ui --------------------------------------
        #insert tag panels
        names(values$categories) %>%
          stringr::str_subset("notes", negate = TRUE) %>%
           map(\(x)
              nav_insert(id = "tag_tabs",
                         nav_panel(x,
                                   card(card_header(x),
                                        card_body(fluidRow(names(values$categories %>%
                                                                    pluck(x)) %>%
                                                              map(\(y)
                                                                 select_box_fun(x, y,
                                                                                cat = values$categories,
                                                                                meta = values$d_category_meta)
                                                             )))))))

        ### Add notes input to ui  ------------------------------------
        output$notes  <- renderUI({
          notes_variables %>%
             map(\(x)  textInput(ns(x), x))
        })

        ### Render n paper in selection summary -------------
        output$n_papers_db = renderText(paste("Number of papers in db:",
                                              nrow(values$d_mcdr_tagged)))


        incProgress(4/4)
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
      })


    output$download_tag_cat <- downloadHandler(
      filename = function() {
        paste("lit-tag-categories", ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(values$categories_with_meta, file)
      })

    output$download_ris <- downloadHandler(
      filename = function() {
        paste("fisheries_mcdr_Zotero_2025_10_29", "ris", sep = ".")
      },
      content = function(file) {
        file.copy("fisheries_mcdr_Zotero_2025_10_29.ris", file)
      })


    ## Clear criteria button ----------------------------
    observeEvent(input$clear_all_criteria, {
      updateTextInput(inputId =  "custom_search", value = "")
      updateTextInput(inputId =  "author", value = character(0))
      updateAirDateInput(inputId =  "years", value = c(NA,NA))
      updateTextInput(inputId =  "title", value = character(0))
      updateTextInput(inputId =  "abstract_note", value = character(0))
      updateVirtualSelect(input = "select_missing", selected = character(0))

      clear_tag_input <- function(tag){
        updateTextInput(inputId = tag, value = character(0))
        updateRadioButtons(inputId = tag, selected = character(0))
        updateCheckboxGroupInput(inputId = tag, selected = character(0))
        updateDateInput(inputId = tag,  value = NA)
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
      if(input$custom_search != ""){
        custom_filter <- filter_condtion_test(values$d_mcdr_tagged,
                                              eval(rlang::parse_expr(input$custom_search)))$result
        if(is.null(custom_filter)){
          print("bad")
          showModal(modalDialog(
            title = "Custom search problem",
            "Somthing is wrong with your filter statement. Try again!"
          ))
        } else{
          d_custom_criteria <- data.frame(field = "custom",
                                          value = input$custom_search)
        }
      }

      # paper criteria
      #function to add paper criteria
      add_criteria <- function(d, f, v){
        dc <- d %>%
          bind_rows(data.frame(field = f, value = v))
        return(dc)
      }

      d_paper_criteria <- data.frame(field = character(0),
                                     value = character(0)) %>%
        add_criteria("author", input$author) %>%
        add_criteria("title", input$title) %>%
        add_criteria("abstract_note", input$abstract) %>%
         filter(value != "")


      tag_df_fun <- function(tag){
        data.frame(field = tag, value = paste(input[[tag]], collapse = ";"))
      }

      d_tag_criteria <-  values$tag_variables %>%
         map(\(x) tag_df_fun(x)) %>%
         list_rbind() %>%
         filter(value != "")

      d_years_criteria <- NULL
      years <- NULL
      if(!is.null(input$years)){
        years <- year(input$years)

        d_years_criteria <- data.frame(field = "publication_year",
                                       value =  as.character(years[1]))
        if(length(years) == 2){
          d_years_criteria <- data.frame(field = "years",
                                         value =  paste(years[1],
                                                        years[2],
                                                        sep = " - "))
        }
      }

      d_missing_criteria = NULL
      if(!is.null(input$select_missing)){
        d_missing_criteria <- data.frame(field = input$select_missing) %>%
           mutate(value = "missing")
      }

      #d_criteria <- NULL
      d_criteria <- bind_rows(d_custom_criteria, d_missing_criteria,
                              d_years_criteria, d_paper_criteria, d_tag_criteria)

      ### render criteria table -------------------------
      output$criteria_table <- renderDT(d_criteria, selection = "none",
                                        options = list(dom = "t",
                                                       pageLength = 10000),
                                        rownames = FALSE, server = FALSE)
      ### render criteria table plot -------------------------
      output$criteria_table_plot <- renderDT(d_criteria, selection = "none",
                                             options = list(dom = "t",
                                                            pageLength = 10000),
                                             rownames = FALSE, server = FALSE)

      ### render criteria table plot -------------------------
      output$criteria_table_summary <- renderDT(d_criteria, selection = "none",
                                                options = list(dom = "t",
                                                               pageLength = 10000),
                                                rownames = FALSE, server = FALSE)

      ### filter database  -----------------------------


      if(!is.null(custom_filter)){
        d_filtered <- custom_filter
      } else{
        d_filtered <- values$d_mcdr_tagged
      }

      d_filtered <- d_filtered %>%
         filter(if(input$exclude_obsolete &
                  "date_time_obsolete_db" %in% names(.))
          (is.na(date_time_obsolete_db) | date_time_obsolete_db == "NA") else
            TRUE)

      # year range is a special case
      d_criteria_paper_tag <- bind_rows(d_years_criteria, d_paper_criteria,
                                        d_tag_criteria) %>%
         filter(field != "years")

      if(nrow(d_criteria_paper_tag) > 0){
        for(i in 1:nrow(d_criteria_paper_tag)){
          choices <- NA
          if(!is.na(d_criteria_paper_tag$value[i])){
            choices <- paste("\\b",
                             str_trim(str_split_1(d_criteria_paper_tag$value[i], ";")),
                             "\\b", sep = "")
          }

          d_filtered$meet_criteria <- NA
          for(j in 1:nrow(d_filtered)){
            if(!is.na(d_criteria_paper_tag$field[i])){
              d_filtered$meet_criteria[j] <-
                any(str_detect(d_filtered[[d_criteria_paper_tag$field[i]]][j],
                               choices))
            }
          }

          d_filtered <-
            d_filtered[d_filtered$meet_criteria,]  %>%
             select(-meet_criteria) %>%
            remove_empty(which = "rows")
        }
      }

      #filter by year range
      if("years" %in% d_criteria$field){
        d_filtered <- d_filtered %>%
           mutate(year = as.numeric(publication_year)) %>%
           filter(year >= years[1] & year <= years[2]) %>%
           select(-year)

      }
      #
      if(!is.null(d_missing_criteria)){
        for(i in 1:length(d_missing_criteria$field)){
          d_filtered <- d_filtered %>%
             filter(is.na(.data[[d_missing_criteria$field[i]]]) |
                     .data[[d_missing_criteria$field[i]]] == "NA")
        }

      }

      values$d_mcdr_filtered <- d_filtered

      ### render filter summary ----------------------------
      output$n_papers_selected <- renderText(paste("Number of papers selected:",
                                                   nrow( values$d_mcdr_filtered)))

    })

    ## Show row selected paper info ---------------------
    observeEvent(input$table_rows_selected, {

      table_rows_selected <- input$table_rows_selected

      selected_row_data = values$d_mcdr_filtered %>%
         slice(table_rows_selected)

      categories <- values$categories %>%
         list_modify("notes" = zap())

      tags_text <- ""
      for(i in 1:length(categories)){
        tags_text <- paste(tags_text, "<b>", names(categories)[i], "</b><br>",
                           sep = "")
        for(j in 1:length(names(categories[[i]]))){
          tag_name <- names(categories[[i]])[j]
          tags_text <- paste(tags_text, "<b>", tag_name, ":</b>", sep = "")
          tags_text <- paste(tags_text, " ",
                             str_replace_all(selected_row_data[[tag_name]],
                                             ";", "; "), "<br>", sep = "")
          if(j == length(names(categories[[i]]))){
            tags_text <- paste(tags_text, "<br>", sep = "")
          }
        }
      }

      notes_variables <- values$categories$notes %>%
         pull("notes")

      notes_text <- ""
      for(i in 1:length(notes_variables)){
        notes_text = paste(notes_text, "<b>", notes_variables[i], ":</b><br>",
                           sep = "")
        notes_text = paste(notes_text, selected_row_data[[notes_variables[i]]],
                           "<br><br>", sep = "")

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
      })

    ## Plot -------------------------------------------
    output$plot <- renderPlot({

      ### Use full data set or the filtered data set? -----------
      d <- NULL
      if(input$plot_data == "Full dataset"){
        d <- values$d_mcdr_tagged
      } else{
        d <- values$d_mcdr_filtered
      }

      ### Exclude N/A or missing from x-axis? -------
      d <- d %>%
         filter(!(!is.na(.data[[input$plot_x_var]]) &
                   "Not applicable x-axis" %in% input$plot_remove &
                   .data[[input$plot_x_var]] == "not_applicable")) %>%
         filter(!("Missing x-axis" %in% input$plot_remove &
                   is.na(.data[[input$plot_x_var]])))

      ### Max character x var -----------
      max_x_char <- 20

      ### Author format function -----------------
      author_format <- function(author){
        return(paste(str_trim(word(author, sep = ",")),
                     str_sub(str_trim(word(author, 2,2, sep = ",")), 1, 1), sep = "_"))
      }

      bar_data <- "percent"
      if(input$show_bar_data == "Number of papers"){
        bar_data <- "n"
      }

      ### If there is no stacking variable ----------
      if(input$plot_stack_var == "none"){

        #if the x-axis mutli-select is combined
        d_plot <- d %>%
           select(input$plot_x_var)
        if(!("Combine x-axis" %in% input$combine_multi)){
          d_plot <- d_plot %>%
            separate_longer_delim(input$plot_x_var, delim = ";")
        }

        # the x-axis only plot
        d_plot_2 <- d_plot %>%
           mutate(!!input$plot_x_var :=
                   if_else(rep(input$plot_x_var == "author", nrow(.)),
                           author_format(!!as.name(input$plot_x_var)),
                           !!as.name(input$plot_x_var))) %>%
          tabyl(input$plot_x_var) %>%
           mutate(percent = paste(round(percent * 100), "%", sep = "")) %>%
           mutate(!!input$plot_x_var :=
                   str_replace_all(!!as.name(input$plot_x_var), ";", ", "))

        n_total_paper <- sum(d_plot_2$n)

        # plot data to download for non-stacked plot
        values$d_plot <- d_plot_2

        p <- d_plot_2 %>%
          ggplot(aes(.data[[input$plot_x_var]], n, label = .data[[bar_data]])) +
          geom_col(fill = "blue") +
          ylab("Number of Papers") +
          ggtitle(paste("Total instances =", n_total_paper)) +
          scale_x_discrete(labels = label_wrap(max_x_char)) +
          theme_bw(base_size = 24) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
        #### If show percent -------------------
        if(input$show_bar_data != "None"){
          p +  geom_text(vjust = -1, color = "black", size = 8) +
            coord_cartesian(clip = "off")
        } else{
          p
        }
      } else{

        ### If there is a stacking variable -----------------
        #### Is stack var multi-select? -----------------
        has_multi_select_stacked <-
          any(str_detect(d[[input$plot_stack_var]], ";"),
              na.rm = TRUE)

        if("Combine stacked" %in% input$combine_multi){
          has_multi_select_stacked <- FALSE
        }
        #### Exclude N/A or missing from stacked? -------
        d <- d %>%
           filter(!(!is.na(.data[[input$plot_stack_var]]) &
                     "Not applicable stacked" %in% input$plot_remove &
                     .data[[input$plot_stack_var]] == "not_applicable")) %>%
           filter(!("Missing stacked" %in% input$plot_remove &
                     is.na(.data[[input$plot_stack_var]])))

        #if the x-axis mutli-select is combined
        d_plot <- d %>%
           select(input$plot_x_var)
        if(!("Combine x-axis" %in% input$combine_multi)){
          d_plot <- d_plot %>%
            separate_longer_delim(input$plot_x_var, delim = ";")
        }

        #### d_z for multi-select stack ---------------

        d_x_expand <- d_plot %>%
           mutate(!!input$plot_x_var :=
                   if_else(rep(input$plot_x_var == "author", nrow(.)),
                           author_format(!!as.name(input$plot_x_var)),
                           !!as.name(input$plot_x_var))) %>%
          tabyl(input$plot_x_var) %>%
           mutate(percent = paste(round(percent * 100), "%", sep = ""))

        d_x_expand_n <- d_x_expand %>%
           select(any_of(input$plot_x_var), n)

        n_total_paper = sum(d_x_expand_n$n)

        d_both_expand_f_1 <- d %>%
           select(all_of(c(input$plot_x_var, input$plot_stack_var)))


        #if the x-axis mutli-select is combined
        if(!("Combine x-axis" %in% input$combine_multi)){
          d_both_expand_f_1 <- d_both_expand_f_1 %>%
            separate_longer_delim(input$plot_x_var, delim = ";")
        }

        d_both_expand_f_2 <- d_both_expand_f_1 %>%
           mutate(!!input$plot_x_var :=
                   if_else(rep(input$plot_x_var == "author", nrow(.)),
                           author_format(!!as.name(input$plot_x_var)),
                           !!as.name(input$plot_x_var)))

        if(!("Combine stacked" %in% input$combine_multi)){
          d_both_expand_f_2 <- d_both_expand_f_2 %>%
            separate_longer_delim(input$plot_stack_var, delim = ";")
        }

        d_both_expand_f_3 <- d_both_expand_f_2  %>%
           mutate(!!input$plot_stack_var :=
                   if_else(rep(input$plot_stack_var == "author", nrow(.)),
                           author_format(!!as.name(input$plot_stack_var)),
                           !!as.name(input$plot_stack_var))) %>%
          tabyl(.data[[input$plot_x_var]], .data[[input$plot_stack_var]]) %>%
          adorn_percentages()

        d_z <- d_x_expand_n %>%
          left_join(d_both_expand_f_3, join_by(!!as.name(input$plot_x_var))) %>%
           mutate(across(-any_of(c(input$plot_x_var, "n")), ~ .x * n)) %>%
           select(-n) %>%
          pivot_longer(where(is.numeric), names_to = "stack_var", values_to = "n")

        #### plot stacked ---------------------------

        p <- d_z %>%
           mutate(!!input$plot_x_var :=
                   str_replace_all(!!as.name(input$plot_x_var), ";", ", ")) %>%
           mutate(stack_var = str_replace_all(stack_var, ";", ", ")) %>%
          ggplot(aes(x = .data[[input$plot_x_var]], y = n,
                     fill = stack_var)) +
          geom_col() +
          ylab("Number of Papers or occurences") +
          ggtitle(paste("Total papers =", n_total_paper)) +
          scale_x_discrete(labels = label_wrap(max_x_char)) +
          scale_fill_discrete(labels = label_wrap(max_x_char)) +
          theme_bw(base_size = 24) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
          guides(fill = guide_legend(title = input$plot_stack_var))

        #### warning if stacked multiselect ------------------
        if(has_multi_select_stacked){
          p <- p +
            ggtitle(paste("Total papers =", n_total_paper),
                    subtitle = paste("Note: The stacked variable has multiple",
                                     "selections for each x-axis variable.\nThe fill colors",
                                     "show the proportion of each stack value, not the",
                                     "absolute count."))
          theme(plot.subtitle = element_text(size = 12))
        }
        #### if show percent ------------------
        if(input$show_bar_data != "None"){
          p <- p +
            geom_text(data = d_x_expand %>%
                         mutate(stack_var = NA) %>%
                         mutate(!!input$plot_x_var :=
                                 str_replace_all(!!as.name(input$plot_x_var), ";", ", ")),
                      aes(label = .data[[bar_data]]), vjust = -1, color = "black",
                      size = 8) +
            coord_cartesian(clip = "off")

        }
        # plot data to download for stacked plot
        values$d_plot <- d_z
        p
      }

    }, height = 800)

    ### download plot data -----------------------

    output$download_plot_data <- downloadHandler(
      filename = function() {
        paste("lit_tag_plot_data_", format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
              ".csv", sep = "")
      },
      content = function(file) {
        write_csv(values$d_plot, file)
      })

    ## Summary table ---------------------------------------------

    ### Show summary table change -------------------
    #observeEvent(input$show_summary_table,{
    #observeEvent(c(input$summary_data),{
    observeEvent(c(input$summary_var, input$summary_data, input$select_papers),{
      #### Use full data set or the filtered data set? -----------
      d <- NULL
      if(input$summary_data == "Full dataset"){
        d <- values$d_mcdr_tagged
      } else{
        d <- values$d_mcdr_filtered
      }

      if(!is.null(d)){
        d <- d %>%
           select(input$summary_var)
      }

      output$summary_table <- renderDT(d, selection = "none",
                                       extensions = 'ColReorder',
                                       callback = JS(newjs),
                                       options = list(dom = "t",
                                                      pageLength = 10000,
                                                      #autoWidth = TRUE,
                                                      columnDefs =
                                                        list(list(width =
                                                                    '200px',
                                                                  targets = "_all")),
                                                      scrollX = TRUE,
                                                      scrollY = TRUE,
                                                      colReorder = TRUE),
                                       rownames = FALSE, server = FALSE,
      )


    }, ignoreInit = TRUE)

    ### Download summary csv ----------------------------
    output$download_summary <- downloadHandler(
      filename = function() {
        paste(input$summary_download_name, ".csv", sep = "")
      },
      content = function(file) {

        d <- NULL
        if(input$summary_data == "Full dataset"){
          d <- values$d_mcdr_tagged
        } else{
          d <- values$d_mcdr_filtered
        }

        # if the column order has not changes, select var in default order
        # if the order has changed, select user specifier order
        if(is.null(input$colOrder)){
          d <- d %>%
             select(input$summary_var)
        } else{
          d <- d %>%
             select(input$colOrder)
        }

        write.csv(d, file, row.names = FALSE)
      })

    ## Render quarto fun -----------
    render_quarto_fun <- function(report_type){
      d_report <- values$d_mcdr_tagged
      if(input$report_data == "Filtered dataset") {
        d_report <- values$d_mcdr_filtered
      }

      if(input$report_sort_order == "author"){
        d_report <- d_report %>%
          arrange(author, publication_year, title)
      }
      if(input$report_sort_order == "publication_year"){
        d_report <- d_report %>%
          arrange(publication_year, author, title)
      }
      if(input$report_sort_order == "title"){
        d_report <- d_report %>%
          arrange(title, author, title)
      }

      d_report <- d_report %>%
        replace(is.na(.), "NA")

      categories <- values$categories %>%
         list_modify("notes" = rlang::zap())

      # make data frame of categories and tag names for quarto report
      max_tag_per_cat <- names(categories) %>%
         map(\(x) length(categories[[x]])) %>%
        unlist() %>%
        max()

      d_cat_tag <- NULL
      for(i in 1:length(categories)){
        d_cat_temp <-
          data.frame(c(names(categories[[i]]),
                       rep("NA",
                           max_tag_per_cat - length(names(categories[[i]])))))
        d_cat_tag <- bind_cols(d_cat_tag, d_cat_temp)
      }

      names(d_cat_tag) <- names(categories)

      quarto::quarto_render(
        input = "report/lit_tag_report_template.qmd",
        output_format = report_type,
        #output_file = ,
        execute_params = list(
          name = input$report_author,
          report_title = input$report_title,
          d = d_report,
          categories = d_cat_tag,
          note_var = values$tag_variables[str_detect(values$tag_variables,
                                                     "note")],
          include_url = "paper_url" %in% input$report_include,
          include_abstract = "abstract" %in% input$report_include,
          include_tags = "tags" %in% input$report_include,
          include_tags_missing = "missing_tags" %in% input$report_include,
          include_tags_not_applicable =
            "not_applicable_tags" %in% input$report_include,
          include_notes = "notes" %in% input$report_include,
          include_pagebreaks = "pagebreaks" %in% input$report_include
        )
      )
    }

    ## Show report button ------------------------
    observeEvent(input$show_report,{
      withProgress(message = "Rendering report", value = 0, {
        incProgress(1/4)
        render_quarto_fun("html")
        output$report <- renderUI(includeHTML({
          "report/lit_tag_report_template.html"
        }))
        incProgress(4/4)
      })
    })

    ## Downlaod report button --------------------
    output$download_report <- downloadHandler(

      filename =  function(){
        paste(input$report_filename, input$report_type, sep = ".")
      },

      content = function(file) {
        withProgress(message = "Rendering report", value = 0, {

          incProgress(1/4)

          render_quarto_fun(input$report_type)

          # copy the quarto generated file to `file` argument.
          generated_file_name <- paste("report/lit_tag_report_template",
                                       input$report_type,
                                       sep = ".")
          file.copy(generated_file_name, file)

          incProgress(4/4)
        })
      }
    )

    ## Download mcdr example button ------------------
    output$mcdr_example <- downloadHandler(
      filename = function() {
        "mcdr_example.zip"
      },
      content = function(file) {
        file.copy("docs/mcdr_example.zip", file)
      },
      contentType = "application/zip")

  })
}
