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

    # remove_leading_special_char function will remove characters that
    # cause problems with excel if they are the first character in the cell

    remove_leading_special_char <- function(x){
      x_no_leading_special_char <- data.frame(x = x) %>%
         mutate(x = if_else(str_sub(x, 1, 1) %in% c("-", "+", "="),
                           str_sub(x, 2, -1), x)) %>%
         pull(x)

      return(x_no_leading_special_char)
    }

  ## waiter --------------------------------------
  w <- Waiter$new(html = spin_3(),
                  color = transparent(.5))

  ## Reactive values --------------------------------------------
  values <- reactiveValues(d_mcdr_tagged = NULL, categories = NULL,
                           d_category_meta = NULL, d_mcdr_filtered = NULL,
                           default_filter_var = character(0),
                           last_key = NULL, inspire_quotes = NULL,
                           inspire_images = NULL, d_content_db = NULL,
                           d_old_key_db = NULL, d_split_db = NULL,
                           tag_variables = NULL, bib_table_col = NULL)

  ## Render paper info function ----------------------------
  render_paper_info <- function(label, paper_var){
    return(renderText(paste(label, values$d_mcdr_filtered %>%
                               slice(input$table_rows_selected) %>%
                               pull(paper_var))))
  }

  ## load categories function -----------------
  load_categories <- function(filepath){

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

    #vector of tag variables
    values$tag_variables <- c( row.names(values$d_category_meta) %>%
                          stringr::str_subset("notes", negate = TRUE),
                        values$categories$notes %>%
                          pull("notes"))

  }

  ## Load data button ---------------------------------------
  observeEvent(input$load_data, {

    #show dialog if database or category file missing
    if(is.null(input$database_csv$datapath) |
       is.null(input$categories_excel$datapath)){
      showModal(modalDialog(title = "Select database and category files."))
    } else{
    withProgress(message = "Loading data", value = 0, {
      ### Load category data ---------------------------------------
      incProgress(1/4)

      load_categories(input$categories_excel$datapath)

      #vector of notes variables
      notes_variables <- values$categories$notes %>%
         pull("notes")

      tag_variables <- values$tag_variables
      categories_with_meta <- values$categories_with_meta
      d_category_meta <- values$d_category_meta
      categories <- values$categories

      # vector of date tags
      date_fields <- d_category_meta %>%
         filter(select_type == "date") %>%
        row.names()

      incProgress(2/4)

      ### Load database ----------------------------------------------

      # the d_mcdr_tagged dataframe always keeps all data
      values$d_mcdr_tagged <-  read_csv(input$database_csv$datapath) %>%
         mutate(across(everything(), as.character))

      #add "extra" column if it does not already exist
      if(!("extra" %in% names(values$d_mcdr_tagged))){
        values$d_mcdr_tagged <-  values$d_mcdr_tagged %>%
          mutate(extra = "")
      }

      # if there is no "notes" column in the original zotero file, it needs added
      # this is a bit of hack to deal with the fact that oned of the category tabs is named "notes"
      # which is also a potential field in zotero.
      # the rest of the code in the app deals with this issue, but it
      # depends on the existnace of a "notes column.
      # there are more graceful ways to do this...
      if(!("notes"%in% names(values$d_mcdr_tagged))){
        values$d_mcdr_tagged$notes <- "NA"
      }

      ### Add new tags to database. -----------------------------------
      # If there are tags in the categories file that are not in database,
      # the new tags need to be added

      new_tags <- c(tag_variables[!(tag_variables %in%
                                      names(values$d_mcdr_tagged))])

      values$d_mcdr_tagged[new_tags] <- NA

      ### Filter database --------
      # the d_mcdr_filtered dataframe is the filtered data shown in table
      values$d_mcdr_filtered <- values$d_mcdr_tagged %>%
         filter(if(input$exclude_obsolete &
                  "date_time_obsolete_db" %in% names(.))
          (is.na(date_time_obsolete_db) | date_time_obsolete_db == "NA") else
            TRUE)

      incProgress(3/4)

      ### Add notes input to ui  ------------------------------------
      output$notes  <- renderUI({
        notes_variables %>%
            map(\(x)
              textAreaInput(ns(x), x, width = 600, height = 200))
      })

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
                                                               select_box_fun(x, y, cat = values$categories,
                                                                              meta = values$d_category_meta)
                                                           )))))))
      ### Bibliography table -----------------------------------------
      if(all(values$bib_table_col %in%
             names(values$d_mcdr_filtered))){
        output$table <- renderDT(values$d_mcdr_filtered %>%
                                    select(values$bib_table_col),
                                 selection = list(mode ="single"),
                                 options = list(dom = "t",
                                                pageLength = 10000),
                                 rownames = FALSE, server = FALSE)
      }

      ### Show selected paper info -------------------------------------
      output$selected_year <- render_paper_info("Year:", "publication_year")
      output$selected_author <- render_paper_info("Authors:", "author")
      output$selected_title <- render_paper_info("Title:", "title")
      output$selected_journal <- render_paper_info("Journal:",
                                                   "publication_title")
      # output$selected_extra <- render_paper_info("Extra:",
      #                                              "extra")


      ### Select filter variables dropdown  -------------------------------

      output$n_db <- renderText(paste("Papers in database:",
                                      nrow(values$d_mcdr_tagged)))
      output$n_filtered_db <- renderText(paste("Papers in filtered database:",
                                               nrow(values$d_mcdr_filtered)))

      paper_fields <- c("item_type", "publication_year", "first_author")

      cat_without_notes <- values$categories %>%
         list_modify(notes = rlang::zap())

      plot_opt_list <- names(cat_without_notes) %>%
         purrr::set_names() %>%
          map(\(x) names(cat_without_notes[[x]])) %>%
         list_assign(paper_fields = paper_fields)

      opt_list_name_order <- c("paper_fields",
                               names(plot_opt_list)[1:length(plot_opt_list)-1])

      filter_opt_list_sorted <- opt_list_name_order %>%
         purrr::set_names() %>%
          map(\(x) plot_opt_list[[x]])


      default_filter_var <- character(0)
      if(!identical(values$default_filter_var, character(0))){
        default_filter_var <- str_split_1(values$default_filter_va, ";")
      }

      updateVirtualSelect(inputId = "filter_var",
                          choices = filter_opt_list_sorted,
                          selected = default_filter_var
      )

      incProgress(4/4)
    })
    }
  })

  ## Proxy for the papers table ------------------------------
  dt_proxy <- DT::dataTableProxy("table")

  ## Observe show extra --------------------------------
  observeEvent(input$show_extra,{
    if(input$show_extra){
      values$bib_table_col <- c("first_author", "publication_year", "title", "extra")
      output$selected_extra <- render_paper_info("Extra:", "extra")
    } else{
      values$bib_table_col <- c("first_author", "publication_year", "title")
      output$selected_extra <- NULL
    }
  })

  ## Observe select filter fields  -------------------------------
  observeEvent(input$filter_var, {

    ### Render UI of filters -----------------------------
    output$filters  <- renderUI({
      input$filter_var %>%
          map(\(x) checkboxGroupInput(ns(paste("filter", x, sep = "_")),
                                    paste("filter", x, sep = "_"),
                                    unique(values$d_mcdr_tagged %>%
                                              pull(x) %>%
                                             replace_na("NA")) %>%
                                      sort(),
                                    inline = TRUE))
    })

  })
  ## Observe filter button ----------------------------

  observeEvent(input$filter_db,{
    values$d_mcdr_filtered <-  values$d_mcdr_tagged %>%
       filter(if(input$exclude_obsolete) (is.na(date_time_obsolete_db) |
                                           date_time_obsolete_db == "NA") else TRUE)


    filter_fun <- function(y){

      selected_val <- input[[paste("filter", y, sep = "_")]]

      var_with_na_sting <- values$d_mcdr_filtered %>%
         pull(y) %>%
        replace_na("NA")

      values$d_mcdr_filtered <-  values$d_mcdr_filtered %>%
         filter(var_with_na_sting %in% selected_val)
    }

    input$filter_var %>%
        map(\(x) filter_fun(x))

  })

  ## Observe show all button  -------------------------

  observeEvent(input$show_all_db, {
    values$d_mcdr_filtered <-  values$d_mcdr_tagged
  })

  ## Observe unselect filters button ------------------------
  observeEvent(input$unselect_filters, {
    input$filter_var %>%
        map(\(x) updateCheckboxGroupInput(inputId =
                                          paste("filter", x, sep = "_"),
                                        selected = character(0)))
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


  ## Observe changes in row selected ----------------------

  ### Save tag value function -----------------------
  save_tag_value <- function(key, tag){

    tag_value <-  paste(input[[tag]], collapse = ";")
    if(tag_value == ""){
      tag_value <- NA
    }

    values$d_mcdr_tagged[values$d_mcdr_tagged$key == key, tag] <-
      tag_value
  }

  ### Save last row function -----------------------
  save_last_row <- function(key, d_category_meta, d_notes){
    if(!is.null(key)){

      rownames(d_category_meta) %>%
          map(\(x) save_tag_value(key, x))

      d_notes %>%
         pull("notes") %>%
          map(\(x) save_tag_value(key, x))


      values$d_mcdr_filtered[values$d_mcdr_filtered$key == key,] <-
        values$d_mcdr_tagged[values$d_mcdr_tagged$key == key, ]

    }
  }

  ### Observe changes to row function ------------------
  load_row_tags_fun <- function(x, d_category_meta, table_rows_selected){

    #save_last_row(values$last_key)

    row_val <- values$d_mcdr_filtered %>%
       slice(table_rows_selected) %>%
       pull(x)

    if(d_category_meta[x, "select_type"] == "check_box_single"){
      if(is.na(row_val) | row_val == "NA" | row_val == "" |
         identical(row_val, character(0))){
        s <- character(0)
      } else{
        s <- row_val
      }
      updateRadioButtons(inputId = x, selected = s)
    }
    if(d_category_meta[x, "select_type"] == "check_box_multiple"){
      if(is.na(row_val) | row_val == "NA" | row_val == "" |
         identical(row_val, character(0))){
        s <- character(0)
      } else{
        s <- str_split_1(row_val, ";")
      }
      updateCheckboxGroupInput(inputId = x, selected = s)
    }

    if(d_category_meta[x, "select_type"] == "text_box"){
      updateTextInput(inputId = x, value = row_val)
    }

    if(d_category_meta[x, "select_type"] == "date"){
      updateDateInput(inputId = x, value = row_val)
    }

  }

  ### Observe changes in row event ----------------------
  observeEvent(input$table_rows_selected, {

    w$show()

    table_rows_selected <- input$table_rows_selected

    current_key <- values$d_mcdr_filtered %>%
       slice(table_rows_selected) %>%
       pull(key)

    last_key <- values$last_key
    d_category_meta <- values$d_category_meta
    d_notes <- values$categories$notes

    #just the tag fields (i.e. not notes)
    tags <-  rownames(d_category_meta)[rownames(d_category_meta) != "notes"]

    if(is.null(last_key)){
      last_key <- current_key
      # load selected row tags
      tags %>%
          map(\(x) load_row_tags_fun(x, d_category_meta, table_rows_selected))

      #load selected row notes
      d_notes %>%
         pull("notes") %>%
          map(\(x) updateTextAreaInput(inputId = x,
                                     value = values$d_mcdr_filtered %>%
                                        slice(table_rows_selected) %>%
                                        pull(x)))

      values$last_key <- current_key
    }
    if(current_key != last_key){

      # update database with last selected rows data
      # selecting a new row tiggers the saving of the last rows input data
      save_last_row(last_key, d_category_meta, d_notes)

      # load selected row tags
      tags %>%
          map(\(x) load_row_tags_fun(x, d_category_meta, table_rows_selected))

      #load selected row notes
      d_notes %>%
         pull("notes") %>%
          map(\(x) updateTextAreaInput(inputId = x,
                                     value = values$d_mcdr_filtered %>%
                                        slice(table_rows_selected) %>%
                                        pull(x)))

      # change the last key to the current row
      # this will be used to save any data changes when a new row is selected
      values$last_key <- current_key

      #need for some reason to make sure it does not loose
      #highlighting the current row
      selectRows(dt_proxy, table_rows_selected)
    }

    w$hide()

  })

  ## Download edits button ------------------
  output$download_edits <- downloadHandler(
    filename = function() {
      paste(str_remove(input$database_csv$name, ".csv"), "_",
            format(now("UTC"), "%Y_%m_%d_%H%M_UTC"), ".csv", sep = "")
    },
    content = function(file) {

      values$last_key <- values$d_mcdr_filtered %>%
         slice(input$table_rows_selected) %>%
         pull(key)

      save_last_row(values$last_key, values$d_category_meta,
                    values$categories$notes)

      # the remove_leading_special_char function makes sure that
      # the are no leading characters in any of the data that
      # will cause "#NAME?" errors if the file is opened in excel
      values$d_mcdr_tagged %>%
         mutate(across(everything(), as.character)) %>%
         mutate(across(everything(), ~ remove_leading_special_char(.x))) %>%
        write_csv(file)

    })

    ## Read Zotero function -------------
    read_zotero <- function(filepath){
      d <-  read_csv(filepath) %>%
        clean_names() %>%
        remove_empty() %>%
        mutate(first_author = word(author, sep = ",")) %>%
        arrange(first_author) %>%
        mutate(across(everything(), as.character))

      return(d)
    }

  ## New database button ---------------------------
  output$new_database <- downloadHandler(
    filename = function() {
      paste(input$new_db_name, ".csv", sep = "")
    },
    content = function(file) {
      d_zotero <- read_zotero(input$new_zotero_csv$datapath)
      load_categories(input$cat_new_db$datapath)
      d_new_db <- d_zotero
      d_new_db[values$tag_variables] <- NA

      output$nrow_new_db <- renderText(paste("Number of papers in new db:",
                                           nrow(d_new_db)))
      output$n_new_tags <-
        renderText(paste("Number of tags (including notes) in new db:",
                         length(values$tag_variables)))

      write_csv(d_new_db, file)
    }
  )

  ## Sync zotero button --------------------------

  output$update_from_zotero <- downloadHandler(
    filename = function() {
      paste(str_remove(input$sync_database_csv$name, ".csv"), "_",
            format(now("UTC"), "%Y_%m_%d_%H%M_UTC"), ".csv", sep = "")
    },
    content = function(file) {

      withProgress(message = "Updating from Zotero", value = 0, {

        ### Read zotero --------------------------------------

        d_zotero <-  read_zotero(input$sync_zotero_csv$datapath)

        incProgress(1/4)

        ### Tag variables -------------------------------------------------

        # Tag variables
        # tag_variables <- c( row.names(values$d_category_meta) %>%
        #                       stringr::str_subset("notes", negate = TRUE),
        #                     values$categories$notes %>%
        #                        pull("notes"))

        load_categories(input$sync_categories_excel$datapath)
        tag_variables <- values$tag_variables


        incProgress(2/4)

        ### Set init database ------------------------------------------------
        #d_database <- values$d_mcdr_tagged
        d_database <- read_csv(input$sync_database_csv$datapath)


        ### Set Keys -----------------------------------------------
        # Intial, new and obsolete keys
        keys_db_init <- unique(d_database$key)
        keys_z <- unique(d_zotero$key)
        new_keys <- keys_z[!(keys_z %in% keys_db_init)]
        old_keys_in_zotero <- keys_db_init[keys_db_init %in% keys_z]
        # old_keys_not_in_zotero are also called "obsolete keys"
        old_keys_not_in_zotero <- keys_db_init[!(keys_db_init) %in% keys_z]

        output$n_init_db <- renderText(paste("Inital papers in db:",
                                             length(keys_db_init)))
        output$n_zotero <- renderText(paste("Papers in Zotero file:",
                                            length(keys_z)))
        output$n_new_keys <- renderText(paste("New paper keys in Zotero:",
                                              length(new_keys)))
        output$n_old_key <- renderText(paste("Old paper keys in db but not Zotero:",
                                             length(old_keys_not_in_zotero)))
        output$n_new_db <- renderText(paste("Papers in new db:",
                                            length(keys_db_init) +
                                              length(new_keys)))

        incProgress(3/4)

        ### Get current datetime -----------------------------------------
        current_datetime <- format(now("UTC"), "%Y_%m_%d_%H%M_UTC")

        ### Create updated database ----------------------------------------
        # if data file does not contain "key" column make new database
        # else append to new papers to existing db
        if(!("key" %in% names(d_database))){
          d_updated_db <- d_zotero %>%
             mutate(date_time_added_db = current_datetime,
                   date_time_obsolete_db = NA)
          d_updated_db[tag_variables] <- "NA"
        } else{
          d_new_zotero <- d_zotero %>%
             filter(key %in% new_keys) %>%
             mutate(date_time_added_db = current_datetime,
                   date_time_obsolete_db = NA)
          d_new_db <- d_new_zotero %>%
             mutate(across(everything(), as.character))
          d_new_db[tag_variables] <- "NA"


          # papers in oringial db and the new zotero
          # update the db with any edits to the zotero variable
          d_updated_z_db <- d_database %>%
             filter(!(key %in% old_keys_not_in_zotero)) %>%
            left_join(d_zotero, join_by("key")) %>%
             select(-contains(".x")) %>%
            purrr::set_names(str_remove(names(.), "\\.y")) %>%
             mutate(across(everything(), as.character))

          # papers in original db but no in the new zotero
          d_obsolete_db <- d_database %>%
             filter(key %in% old_keys_not_in_zotero) %>%
             mutate(date_time_obsolete_db = current_datetime) %>%
             mutate(across(everything(), as.character))

          # combine all types of papers in one new db
          d_updated_db <- bind_rows(d_obsolete_db, d_updated_z_db, d_new_db) %>%
            arrange(author, publication_year)

        }

        ### Write updated database to csv
        write_csv(d_updated_db, file)
        incProgress(4/4)

      })
    })

  ## Database maintenance --------------------------
  ### Database content ------------------------------

  tag_values_in_db <- function(d){
    zotero_fields <-  read_csv("data/zotero_fields.csv")$zotero_fields
    db_names <- names(d)
    tags_notes <- db_names[!(db_names %in% zotero_fields)]
    db_tags <- tags_notes[!str_detect(tags_notes, "notes")]
    db_notes <- tags_notes[str_detect(tags_notes, "notes")]

    tag_options <- db_tags %>%
       purrr::set_names() %>%
        map(\(x) as.character(unique(d[[x]])))

    tag_options_unique <- names(tag_options) %>%
       purrr::set_names() %>%
        map(\(x) unique(str_trim(unlist(unlist(str_split(tag_options[[x]], ";"))))))

    tag_option_length <- tag_options_unique %>%
        map(\(x) length(x)) %>%
      unlist()

    d_tag <- data.frame(n_option = tag_option_length) %>%
      tibble::rownames_to_column("tags") %>%
      arrange(tags)

    return(list(d_tag = d_tag, tag_options_unique = tag_options_unique,
                db_notes = db_notes))
  }

  observeEvent(input$content_db,{
    values$d_content_db <-   read_csv(input$content_db$datapath)

    output$n_papers <- renderText(HTML(paste("Number of papers in database: ",
                                             nrow(values$d_content_db), sep = "")))

    output$db_tags_table <- renderDT(tag_values_in_db(values$d_content_db)$d_tag,
                                     selection = list(mode ="single"),
                                     options = list(dom = "t",
                                                    pageLength = 10000),
                                     rownames = FALSE, server = FALSE,
                                     colnames = c("Tag name", "Number of unique values"))

    output$db_notes_table <- renderDT(data.frame(tag_values_in_db(values$d_content_db)$db_notes),
                                      options = list(dom = "t",
                                                     pageLength = 10000),
                                      rownames = FALSE, server = FALSE,
                                      colnames = c("Notes name"))

  })

  observeEvent(input$db_tags_table_rows_selected, {
    table_rows_selected <- input$db_tags_table_rows_selected
    #browser()
    tag_info <- tag_values_in_db(values$d_content_db)

    selected_tag <- tag_info$d_tag$tags[table_rows_selected]

    tag_unique <- sort(tag_info$tag_options_unique[[selected_tag]])

    tag_value_string <- paste(values$d_content_db[[selected_tag]],
                              collapse = ";")

    count_unique <- tag_unique %>%
        map(\(x) str_count(tag_value_string, fixed(x))) %>%
      unlist()

    tag_unique_with_count <- paste(tag_unique, " (", count_unique, ")",
                                   sep = "")

    showModal(modalDialog(
      title = selected_tag,
      HTML(paste(tag_unique_with_count, collapse = "<br>")),
      easyClose = TRUE))

  })
  ### Compare databases --------------------------------

  observeEvent(input$compare_db, {
    d_compare_1 <-   read_csv(input$compare_db_1$datapath)
    d_compare_2 <-   read_csv(input$compare_db_2$datapath)

    output$papers_in_1_not_2 <- renderDT(
      d_compare_1 %>%
         filter(!(key %in% d_compare_2$key)) %>%
         select(key, first_author, publication_year, title),
      options = list(dom = "t",
                     pageLength = 10000),
      rownames = FALSE, server = FALSE,
      colnames = c("Key", "First Author", "Year", "Title")
    )

    output$papers_in_2_not_1 <- renderDT(
      d_compare_2 %>%
         filter(!(key %in% d_compare_1$key)) %>%
         select(key, first_author, publication_year, title),
      options = list(dom = "t",
                     pageLength = 10000),
      rownames = FALSE, server = FALSE,
      colnames = c("Key", "First Author", "Year", "Title")
    )

    output$n_papers_compare_1 <-
      renderText(HTML(paste("Number of papers in database #1: ",
                            nrow(d_compare_1), sep = "")))

    output$n_papers_compare_2 <-
      renderText(HTML(paste("Number of papers in database #2: ",
                            nrow(d_compare_2), sep = "")))
  })

  ### Replace/delete data -----------------------------------
  #### Replace tag option function ----------------
  replace_tag_option <- function(d, tag, option, value){

    dr <- d %>%
       mutate(!!sym(tag) := str_replace(.[[tag]], fixed(option), value))

    return(dr)
  }
  #### replace tag name ---------------------

  output$replace_tag_name_download <- downloadHandler(
    filename = function() {
      paste(str_remove(input$edit_db$name, ".csv"), "_",
            format(now("UTC"), "%Y_%m_%d_%H%M_UTC"), ".csv", sep = "")
    },
    content = function(file) {

      d_edit_db <-  read_csv(input$edit_db$datapath)
      old_name <- input$old_tag_name
      new_name <- input$new_tag_name
      d_edit_complete <- d_edit_db %>%
         rename(!!sym(new_name) := old_name)

      write_csv(d_edit_complete, file)

    }
  )

  #### replace tag option name download -------------------------

  output$replace_option_name_download <- downloadHandler(
    filename = function() {
      paste(str_remove(input$edit_db$name, ".csv"), "_",
            format(now("UTC"), "%Y_%m_%d_%H%M_UTC"), ".csv", sep = "")
    },
    content = function(file) {

      d_edit_db <-  read_csv(input$edit_db$datapath)

      t_name <- input$tag_name
      old_opt_name <- input$old_option_name
      new_opt_name <- input$new_option_name

      d_edit_complete <- replace_tag_option(d_edit_db, t_name,
                                            old_opt_name, new_opt_name)

      write_csv(d_edit_complete, file)

    }
  )

  #### delete tags download  ----------------------------

  output$delete_tags_download <- downloadHandler(
    filename = function() {
      paste(str_remove(input$edit_db$name, ".csv"), "_",
            format(now("UTC"), "%Y_%m_%d_%H%M_UTC"), ".csv", sep = "")
    },
    content = function(file) {

      d_edit_db <-  read_csv(input$edit_db$datapath)
      delete_tags <- str_trim(str_split_1(input$delete_tags, ","))
      d_edit_complete <- d_edit_db %>%
         select(!delete_tags)

      write_csv(d_edit_complete, file)

    }
  )

  #### delete tag options download -------------------------

  output$delete_tag_option_download <- downloadHandler(
    filename = function() {
      paste(str_remove(input$edit_db$name, ".csv"), "_",
            format(now("UTC"), "%Y_%m_%d_%H%M_UTC"), ".csv", sep = "")
    },
    content = function(file) {

      d_edit_db <-  read_csv(input$edit_db$datapath)
      delete_tag_options <- str_trim(str_split_1(input$delete_tag_options, ","))

      tags <- word(delete_tag_options, sep = "/")
      options <- word(delete_tag_options, -1, sep = "/")

      d_edit_complete <- d_edit_db

      for(i in 1:length(tags)){
        d_edit_complete <- replace_tag_option(d_edit_complete, tags[i],
                                              options[i], "")
      }


      write_csv(d_edit_complete, file)

    }
  )

  #### delete not in zotero download -------------------------------
  output$delete_not_in_zotero_download <- downloadHandler(
    filename = function() {
      paste(str_remove(input$edit_db$name, ".csv"), "_",
            format(now("UTC"), "%Y_%m_%d_%H%M_UTC"), ".csv", sep = "")
    },
    content = function(file) {

      d_edit_db <-  read_csv(input$edit_db$datapath)
      zotero_keys <-  read_csv(input$zotero_for_delete$datapath) %>%
        clean_names() %>%
         pull(key)

      d_edit_complete <- d_edit_db %>%
         filter(key %in% zotero_keys)

      write_csv(d_edit_complete, file)

    }
  )

  #### delete papers based on tag options download -------------------------------
  output$delete_papers_with_tag_option_download <- downloadHandler(
    filename = function() {
      paste(str_remove(input$edit_db$name, ".csv"), "_",
            format(now("UTC"), "%Y_%m_%d_%H%M_UTC"), ".csv", sep = "")
    },
    content = function(file) {

      #browser()
      d_edit_db <-  read_csv(input$edit_db$datapath)
      delete_papers_tag_options <-
        str_trim(str_split_1(input$delete_papers_with_tag_options, ","))

      tags <- word(delete_papers_tag_options, sep = "/")
      options <- word(delete_papers_tag_options, -1, sep = "/")

      d_edit_complete <- d_edit_db

      for(i in 1:length(tags)){
        d_edit_complete <- d_edit_complete %>%
           filter(!!sym(tags[i]) != options[i])
      }

      write_csv(d_edit_complete, file)

    }
  )

  ## New Zotero ------------------------
  ### Generate RIS ---------------------
  output$generate_ris <- downloadHandler(
    filename = function() {
      paste(str_remove(input$database_nz_csv$name, ".csv"), ".ris", sep = "")
    },
    content = function(file) {
      withProgress(message = "Generating RIS file", value = 0, {
        # read the original lit-tag db file that has old zotero key values
        d_nz <-  read_csv(input$database_nz_csv$datapath)

        incProgress(1/4)
        output$n_old_key_db <- renderText(HTML(paste("Number of papers in original (old keys) database: ",
                                                     nrow(d_nz), sep = "")))


        # sometimes, the title in the original db file might have html escape code (e.g. "&lt;")
        # if these escape characters are included as-is in the RIS file, we will fail to get a proper match
        # when creating the final new keys lit tag database
        # to fix this problem, escape codes are coverted to text (e.g. "&lt;" = "<")
        unescape_html <- function(str){
          xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
        }

        html_escape_pattern <- "&([a-zA-Z0-9]+|#[0-9]+|#x[0-9a-fA-F]+);"

        for(i in 1:nrow(d_nz)){
          title <- d_nz$title[i]
          escapes_in_title <- unlist(regmatches(title, gregexpr(html_escape_pattern, title)))
          replace_escapes_in_title <- sapply(escapes_in_title, unescape_html)
          if(length(escapes_in_title) > 0){
            for(j in 1:length(escapes_in_title)){
              title <- str_replace(title, escapes_in_title[j], replace_escapes_in_title[j])
            }
          }
          d_nz$title[i] <- title
        }

        # save the old keys db with html escape codes removed as reactive value for use in new key db generation
        values$d_old_key_db <- d_nz

        #function to generate RIS file of citations in old key db

        ris_fun <- function(paper){
          #paper <- d_nz[1,]
          # Zotero strips htlm code from ris imports
          # To prserve html rags, replace "<" and ">" with text codes
          paper <- paper %>%
             mutate(title = str_replace_all(title, "<", "&lt"),
                   title = str_replace_all(title, ">", "&gt"),
                   title = str_replace_all(title, "\"", "&quot")) %>%
             mutate(abstract_note = str_replace_all(abstract_note, "<", "&lt"),
                   abstract_note = str_replace_all(abstract_note, ">", "&gt"),
                   abstract_note = str_replace_all(abstract_note, "\"", "&quot")) %>%
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

          ris_tag_fun <- function(tag){
            value <-   pull(paper, ris_tag_map[tag])
            value_tag <- NULL
            if(!is.na(value)){
              value_tag <- paste(tag, "-", value, sep = "  ")
            }
            return(value_tag)
          }

          tag_value <- names(ris_tag_map) %>%
              map(\(x) ris_tag_fun(x)) %>%
            unlist()

          general_tags <- data.frame(tag_value)

          author_tags <- data.frame(tag_value =
                                      paste("AU", "-", str_split_1(paper$author, ";"),
                                            sep = "  "))

          record <- data.frame(tag_value = paste("TY", "-",
                                                 case_match(paper$item_type,
                                                            "journalArticle" ~ "JOUR",
                                                            "report" ~ "RPRT",
                                                            "book" ~ "BOOK",
                                                            "thesis" ~ "THES",
                                                            "preprint" ~ "UNPB",
                                                            "bookSection" ~ "CHAP",
                                                            "conferencePaper" ~ "CPAPER",
                                                            .default = "-99"),
                                                 sep = "  ")) %>%
            bind_rows(author_tags) %>%
            bind_rows(general_tags) %>%
            bind_rows(data.frame(tag_value = "ER  -"))

          return(record)
        }

        incProgress(2/4)

        d_ris <- 1:nrow(d_nz) %>%
            map(\(x) ris_fun(d_nz[x,])) %>%
           list_rbind()

        incProgress(3/4)

        write_csv(d_ris, file, quote = "none")

        output$ris_generated <- renderText(
          HTML("RIS file generated from original database downloaded"))
        incProgress(4/4)
      })
    }
  )

  ### Generate new keys database ---------------------
  output$generate_new_keys_db <- downloadHandler(
    filename = function() {
      paste(str_remove(input$database_knz_csv$name, ".csv"),  "_",
            format(now("UTC"), "%Y_%m_%d_%H%M_UTC"), ".csv", sep = "")
    },
    content = function(file) {

      #export file made by zotero from RIS import
      d_nkz <-  read_csv(input$database_knz_csv$datapath) %>%
        clean_names() %>%
         select(key, publication_year, author, title)

      output$n_new_key_zotero <- renderText(
        HTML(paste("Number of papers in zotero file with new keys:",
                   nrow(d_nkz))))

      #copy the old key db file replace they keys from new zotero export by matching year, author and title
      d_new_key_db <- values$d_old_key_db %>%
         select(-key) %>%
        left_join(d_nkz, join_by(publication_year, author, title)) %>%
        relocate(key)

      output$n_new_key_db <- renderText(
        HTML(paste("Number of papers in new keys database:",
                   nrow(d_new_key_db))))

      output$n_new_key_db_missing <- renderText(
        HTML(paste("Number of papers with missing keys in new keys database:",
                   sum(is.na(d_new_key_db$key)))))

      write_csv(d_new_key_db, file)
    }
  )

  ## Download unicorn example button ------------------
  output$unicorn_example <- downloadHandler(
    filename = function() {
      "unicorn_example.zip"
    },
    content = function(file) {
      file.copy("data/unicorn_example.zip", file)
    },
    contentType = "application/zip")

  ## Download mcdr example button ------------------
  output$mcdr_example <- downloadHandler(
    filename = function() {
      "mcdr_example.zip"
    },
    content = function(file) {
      file.copy("data/mcdr_example.zip", file)
    },
    contentType = "application/zip")
})
}
