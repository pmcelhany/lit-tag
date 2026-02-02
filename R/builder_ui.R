#' The lit-tag-builder ui
#'
#' @param id module ID.
#'
#' @import shiny
#' @import shinyWidgets
#' @import waiter
#' @import bslib
#' @import DT
#' @noRd



# builder ui ----------------------
builder_ui <- function(id){
  ns <- NS(id)
  page_navbar(
    ## App title --------------------------------
    title = div(img(src = "www/lit_tag_logo_1.png", height = "100px"),
                h1("Builder"), HTML("&nbsp; &nbsp;"),
                style = "color: brown; display: flex; align-items: center;"),

    ## Tag edit panel ---------------------
    nav_panel(h2("Tag edit"),
              tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
              ),
              useWaiter(),
              layout_columns(
                col_widths = c(4,4,4),
                ### Left panel: database path and table of papers ---------------------
                card(card_header(h3("Paper table")),
                     style = "border",
                     fileInput(ns("database_csv"), h4("Database File"),
                               multiple = FALSE, accept = c(".csv")),
                     fileInput(ns("categories_excel"), h4("Categories File"),
                               multiple = FALSE, accept = c(".xls", ".xlsx")),
                     input_task_button(ns("load_data"), "Load database"),
                     textOutput(ns("n_db")),
                     textOutput(ns("n_filtered_db")),
                     #input_task_button("unselect_filters", "Unselect all filters"),
                     checkboxInput(ns("exclude_obsolete"), "Exclude obsolete papers",
                                   value = TRUE),
                     checkboxInput(ns("show_extra"), "Show Zotero \"extra\" field",
                                   value = FALSE),
                     virtualSelectInput(
                       inputId = ns("filter_var"),
                       label = "Filter variables",
                       choices = "",
                       hideClearButton = FALSE,
                       multiple = TRUE,
                       disableOptionGroupCheckbox = TRUE
                     ),
                     uiOutput(ns("filters")),
                     input_task_button(ns("filter_db"), "Filter database"),
                     input_task_button(ns("show_all_db"), "Show all papers"),
                     DT::DTOutput(ns('table'))),

                ### Center panel: paper info and notes -----------------------------------
                card(card_header(h3("Paper info and notes")),
                     checkboxInput(ns("remove_timestamps"),
                                   "Remove timestamps from original file name",
                                   value = TRUE, width = "100%"),
                     downloadButton(ns("download_edits"),
                                    "Save edits"),
                     textOutput(ns("selected_author")),
                     textOutput(ns("selected_year")),
                     textOutput(ns("selected_title")),
                     textOutput(ns("selected_journal")),
                     textOutput(ns("selected_extra")),
                     input_task_button(ns("show_abstract"), "Abstract"),
                     uiOutput(ns("notes"))),
                ### Right panel: tags -------------------------------------------
                card(card_header(h3("Tags")),
                     navset_pill(id = ns("tag_tabs")))
              )
    ),

    ## New database panel -------------------------
    nav_panel(h2("New database"),
              sidebarLayout(
                sidebarPanel(width = 3,
                             h3("Create a new lit-tag database"),
                             HTML("<br>"),
                             HTML("<br>"),
                             fileInput(ns("new_zotero_csv"), h4("Zotero CSV File"),
                                       multiple = FALSE, accept = c(".csv")),
                             fileInput(ns("cat_new_db"), h4("Categories for new db"),
                                       multiple = FALSE, accept = c(".xlsx")),
                             textInput(ns("new_db_name"), "New database name",
                                       value = "Untitled"),
                             downloadButton(ns("new_database"),
                                            "Download new database")
                ),
                mainPanel(width = 9,
                          h3(textOutput(ns("nrow_new_db"))),
                          h3(textOutput(ns("n_new_tags")))
                )

              )
    ),

    ## Sync zotero panel ---------------------
    nav_panel(h2("Sync Zotero"),
              sidebarLayout(
                sidebarPanel(width = 3,
                             fileInput(ns("sync_database_csv"), h4("Database to sync"),
                                       multiple = FALSE, accept = c(".csv")),
                             fileInput(ns("sync_categories_excel"), h4("Categories file for sync"),
                                       multiple = FALSE, accept = c(".xls", ".xlsx")),
                             fileInput(ns("sync_zotero_csv"), h4("Zotero csv file for sync"),
                                       multiple = FALSE, accept = c(".csv")),
                             downloadButton(ns("update_from_zotero"),
                                            "Update database from Zotero")
                ),
                mainPanel(width = 9,
                          h4(textOutput(ns("n_init_db"))),
                          h4(textOutput(ns("n_zotero"))),
                          h4(textOutput(ns("n_new_keys"))),
                          h4(textOutput(ns("n_old_key"))),
                          h4(textOutput(ns("n_new_db")))
                )

              )
    ),
    ## Database maintenance panel --------------------------------
    nav_panel(h2("Database Maintenance"),
              tabsetPanel(id = ns("db_maintenance_panel"),
                          ### Database contents ------------------------------------------
                          tabPanel("Database contents",
                                   sidebarLayout(
                                     sidebarPanel(width = 3,
                                                  h3("This tab shows all contents of db, not just options from categories file"),
                                                  HTML("<br>"),
                                                  fileInput(ns("content_db"),
                                                            h4("Database for evaluation"),
                                                            multiple = FALSE,
                                                            accept = c(".csv")),
                                                  HTML("<br>"),
                                                  h4("Click on table row to see unique tag values"),
                                                  HTML("<br>"),
                                                  htmlOutput(ns("n_papers")),
                                                  HTML("<br>"),
                                                  HTML("<b>Noted fields</b>"),
                                                  DT::DTOutput(ns("db_notes_table"))),
                                     mainPanel(width = 9,
                                               DT::DTOutput(ns("db_tags_table"))))),
                          ### Compare databases -------------------------
                          tabPanel("Compare databases",
                                   sidebarLayout(
                                     sidebarPanel(width = 3,
                                                  fileInput(ns("compare_db_1"),
                                                            h4("Database to compare #1"),
                                                            multiple = FALSE,
                                                            accept = c(".csv")),
                                                  fileInput(ns("compare_db_2"),
                                                            h4("Database to compare #2"),
                                                            multiple = FALSE,
                                                            accept = c(".csv")),
                                                  input_task_button(ns("compare_db"),
                                                                    "Compare databases"),
                                                  htmlOutput(ns("n_papers_compare_1")),
                                                  htmlOutput(ns("n_papers_compare_2"))),
                                     mainPanel(width = 9,
                                               h3("Papers in db #1, but not in db #2"),
                                               DTOutput(ns("papers_in_1_not_2")),
                                               h3("Papers in db #2, but not in db #1"),
                                               DTOutput(ns("papers_in_2_not_1"))))),
                          ### Replace/delete data -----------------
                          tabPanel("Replace/delete data",
                                   fileInput(ns("edit_db"),
                                             h4("Database to replace/delete"),
                                             multiple = FALSE,
                                             accept = c(".csv")),
                                   h3("These options download a new database with
                                                        the selected change"),
                                   navset_pill_list(
                                     nav_panel("Replace tag name",
                                               textInput(ns("old_tag_name"),
                                                         "Old tag name: "),
                                               textInput(ns("new_tag_name"),
                                                         "New tag name: "),
                                               downloadButton(ns("replace_tag_name_download"),
                                                              "Download database with replaced tag name")),
                                     nav_panel("Replace tag option name",
                                               textInput(ns("tag_name"),
                                                         "Tag name: "),
                                               textInput(ns("old_option_name"),
                                                         "Old option name: "),
                                               textInput(ns("new_option_name"),
                                                         "New option name: "),
                                               downloadButton(ns("replace_option_name_download"),
                                                              "Download database with replaced tag option name")),
                                     nav_panel("Delete tags",
                                               textInput(ns("delete_tags"),
                                                         "Tags to delete (comma seperated): "),
                                               downloadButton(ns("delete_tags_download"),
                                                              "Download database with deleted tags")),
                                     nav_panel("Delete tag options",
                                               textInput(ns("delete_tag_options"),
                                                         "Tag options to delete (specify as \"tag/option\", comma seperated): "),
                                               downloadButton(ns("delete_tag_option_download"),
                                                              "Download datase with deleted tag options")),
                                     nav_panel("Delete papers not in zotero",
                                               fileInput(ns("zotero_for_delete"),
                                                         h4("Zotero for delete comparison"),
                                                         multiple = FALSE,
                                                         accept = c(".csv")),
                                               downloadButton(ns("delete_not_in_zotero_download"),
                                                              "Download database with papers not in Zotero deleted")),
                                     nav_panel("Delete papers based on tag options",
                                               textInput(ns("delete_papers_with_tag_options"),
                                                         "Tag options for paper deletion (specify as \"tag/option\", comma seperated): "),
                                               downloadButton(ns("delete_papers_with_tag_option_download"),
                                                              "Download database with paper deletion based on tag options"))
                                   )
                          ),
                          ### Combine databases -----------------
                          tabPanel("Combine databases",
                                   sidebarLayout(
                                     sidebarPanel(width = 3,
                                                  fileInput(ns("combine_dbs"),
                                                            h4("Databases to combine"),
                                                            multiple = TRUE,
                                                            accept = c(".csv")),
                                                  fileInput(ns("combine_cat"),
                                                            h4("Categories for combined database"),
                                                            multiple = FALSE,
                                                            accept = c(".xlsx")),
                                                  textInput(ns("combined_filename"),
                                                            h4("Name for combined database"),
                                                            value = "Untitled"),
                                                  downloadButton(ns("download_combined"),
                                                                 "Download combined database")
                                                  ),
                                     mainPanel(width = 9,
                                               h3(htmlOutput(ns("n_combined")))
                                               )
                                     )
                                   )
              )),

    ## New zotero panel ---------------------
    nav_panel(h2("New Zotero"),
              sidebarLayout(
                sidebarPanel(width = 3,
                             fileInput(ns("database_nz_csv"),
                                       h4("Old Keys Database File"),
                                       multiple = FALSE, accept = c(".csv")),
                             radioButtons(ns("all_or_unique_ris"), "RIS file",
                                          choices = c("All references",
                                                      "Unique titles"),
                                          selected = "All references"),
                             downloadButton(ns("generate_ris"), "Generate RIS"),
                             HTML("<br>"),
                             HTML("<br>"),
                             fileInput(ns("database_knz_csv"),
                                       h4("New Keys Zotero File"),
                                       multiple = FALSE, accept = c(".csv")),
                             downloadButton(ns("generate_new_keys_db"),
                                            "Generate new keys database")
                ),
                mainPanel(width = 9,
                          textOutput(ns("n_old_key_db")),
                          textOutput(ns("ris_generated")),
                          textOutput(ns("n_new_key_zotero")),
                          textOutput(ns("n_new_key_db")),
                          textOutput(ns("n_new_key_db_missing"))
                )
              )
    ),

    ## Help panel ------------------------
    nav_panel(h2("Help"),
              navset_pill(
                nav_panel("lit-tag-builder",
                          tags$iframe(style="height:100vh; width:100%; scrolling=yes",
                                      src="www/lit_tag_builder_guide.pdf")),
                nav_panel("example files",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         downloadButton(ns("unicorn_example"),
                                                        "Unicorn db example files"),
                                         HTML("<br>"),
                                         HTML("<br>"),
                                         downloadButton(ns("mcdr_example"),
                                                        "mCDR db example files")
                            ),
                            mainPanel(width = 9)
                          )
                )
              )
    )
  )
}
