#' The lit-tag-viewer ui
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
viewer_ui <- function(id){
  ns <- NS(id)

  page_navbar(

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    ## App title --------------------------------
    title = div(img(src = "www/lit_tag_logo_1.png", height = "100px"),
                h1("Viewer"), HTML("&nbsp; &nbsp;"),
                style = "color: brown; display: flex; align-items: center;"),

    ## Load database panel -------------------------
    nav_panel(h2("Load database"),
              sidebarLayout(
                sidebarPanel(width = 3,

                             # If in app data, show db info after loading
                             uiOutput(ns("in_app_db_info")),

                             # If user data, show file choosers
                             uiOutput(ns("db_chooser")),
                             uiOutput(ns("cat_chooser")),

                             ######
                             # Always show load and download buttons
                             input_task_button(ns("load_data"), "Load database"),
                             div(style="margin-bottom:20px"),
                             downloadButton(ns("download_db"),
                                            "Download database"),
                             div(style="margin-bottom:20px"),
                             downloadButton(ns("download_tag_cat"),
                                            "Download tag categories"),
                             div(style="margin-bottom:20px"),
                             uiOutput(ns("download_citation"))
                ),
                mainPanel(width = 9,
                          DTOutput(ns("table_full")))
              )
    ),
    ## Search db panel ---------------------
    nav_panel(h2("Search database"),
              #useWaiter(),
              layout_columns(
                col_widths = c(4,4,4),
                ### Left panel: table of papers ---------------------
                card(card_header(h3("Paper table (filtered)")),
                     checkboxInput(ns("show_extra"),
                                   "Show \"extra\" field",
                                   value = FALSE),
                     DTOutput(ns('table'))),

                ### Center panel: search criteria ---------------------------
                card(card_header(h3("Search criteria")),
                     input_task_button(ns("clear_all_criteria"),
                                       "Clear all criteria"),
                     checkboxInput(ns("exclude_obsolete"),
                                   "Exclude obsolete papers",
                                   value = TRUE),

                     virtualSelectInput(
                       inputId = ns("select_missing"),
                       label = "Select missing values",
                       choices = "",
                       hideClearButton = TRUE,
                       disableSelectAll = TRUE,
                       multiple = TRUE,
                       disableOptionGroupCheckbox = TRUE
                     ),
                     card(card_header(h4("Paper criteria")),
                          textInput(ns("author"), "Author"),
                          airYearpickerInput(ns("years"), "Years", range = TRUE,
                                             minDate = "1960-01-01",
                                             maxDate = "2026-01-01"),
                          textInput(ns("title"), "Title"),
                          textInput(ns("abstract"), "Abstract")),
                     card(card_header(h4("Notes criteria")),
                          uiOutput(ns("notes"))),
                     card(card_header(h4("Tag criteria")),
                          navset_pill(id = ns("tag_tabs"))),
                     textInput(ns("custom_search"),
                               "Advanced custom search (R condition syntax)",
                               width = "100%")),

                ### Right panel: selected papers summary --------------------
                card(card_header(h3("Paper selection summary")),
                     input_task_button(ns("select_papers"), "Select papers"),
                     card(card_header(h4("Selection criteria")),
                          DTOutput(ns("criteria_table"))),
                     card(card_header(h4("Selection summary")),
                          textOutput(ns("n_papers_db")),
                          textOutput(ns("n_papers_selected")))
                )
              )
    ),

    ## Summary graphs panel ---------------------
    nav_panel(h2("Summary plots"),
              sidebarLayout(
                sidebarPanel(width = 3,
                             h3("Plot Variables"),
                             radioButtons(ns("plot_data"), "Plot data",
                                          choices = c("Full dataset",
                                                      "Filtered dataset"),
                                          selected = "Full dataset"),
                             checkboxGroupInput(ns("plot_remove"), "Exclude",
                                                choices =
                                                  c("Not applicable x-axis",
                                                    "Missing x-axis",
                                                    "Not applicable stacked",
                                                    "Missing stacked")),
                             checkboxGroupInput(ns("combine_multi"),
                                                "Combine multi-select",
                                                choices =
                                                  c("Combine x-axis",
                                                    "Combine stacked")),
                             virtualSelectInput(
                               inputId = ns("plot_x_var"),
                               label = "Plot x-axis variable",
                               choices = "",
                               hideClearButton = TRUE,
                               multiple = FALSE,
                               disableOptionGroupCheckbox = TRUE
                             ),
                             virtualSelectInput(
                               inputId = ns("plot_stack_var"),
                               label = "Plot stack variable",
                               choices = "",
                               hideClearButton = TRUE,
                               multiple = FALSE,
                               disableOptionGroupCheckbox = TRUE
                             ),
                             radioButtons(ns("show_bar_data"),
                                          "Show bar data value",
                                          choices = c("None", "Percent",
                                                      "Number of papers"),
                                          selected = "None"),
                             downloadButton(ns("download_plot_data"),
                                            "Download plot data"),
                             HTML("<br>"),
                             HTML("<br>"),
                             card(card_header("Selection criteria"),
                                  DTOutput(ns("criteria_table_plot")))
                ),
                mainPanel(width = 9,
                            h3("Plot"),
                            plotOutput(ns("plot"))
                )
              )),
    ## Summary table panel ---------------------------
    nav_panel(h2("Summary table"),
              sidebarLayout(
                sidebarPanel(width = 3,
                             radioButtons(ns("summary_data"), "Summary data",
                                          choices = c("Full dataset",
                                                      "Filtered dataset"),
                                          selected = "Full dataset"),
                             virtualSelectInput(
                               inputId = ns("summary_var"),
                               label = "Summary variables",
                               choices = "",
                               hideClearButton = FALSE,
                               multiple = TRUE,
                               disableOptionGroupCheckbox = TRUE
                             ),
                             textInput(ns("summary_download_name"),
                                       "Summary download filename",
                                       value = "Untitled"),
                             downloadButton(ns("download_summary"),
                                            "Download summary table (.csv)"),
                             div(style="margin-bottom:20px"),
                             card(card_header("Selection criteria"),
                                  DTOutput(ns("criteria_table_summary")))
                ),
                mainPanel(width = 9,
                          DTOutput(ns("summary_table"))
                )
              )
    ),
    ## Reports panel --------------------------
    nav_panel(h2("Reports"),
              sidebarLayout(
                sidebarPanel(width = 3,
                             textInput(ns("report_title"),"Report title:",
                                       value = "untitled"),
                             textInput(ns("report_author"),"Report author:",
                                       value = ""),

                             radioButtons(ns("report_data"), "Report data",
                                          choices = c("Full dataset",
                                                      "Filtered dataset"),
                                          selected = "Filtered dataset"),
                             selectInput(ns("report_sort_order"), "Report sort order",
                                         choices = c("author", "publication_year",
                                                     "title"),
                                         selected = "author"),
                             checkboxGroupInput(ns("report_include"), "Include in report",
                                                choices = c("paper_url", "abstract",
                                                            "tags", "missing_tags",
                                                            "not_applicable_tags",
                                                            "notes", "pagebreaks"),
                                                selected = c("paper_url", "abstract",
                                                             "tags", "missing_tags",
                                                             "not_applicable_tags",
                                                             "notes")),
                             actionButton(ns("show_report"), "Show report"),
                             textInput(ns("report_filename"),"Report filename:",
                                       value = "untitled"),
                             radioButtons(ns("report_type"), "Report download file type:",
                                          choices = c("html", "pdf", "docx"),
                                          selected = "html"),
                             downloadButton(ns("download_report"), "Download report")
                ),
                mainPanel(width = 9,
                          htmlOutput(ns("report")))
              )
    ),
    ## Help panel ------------------------
    nav_panel(h2("Help"),
              navset_pill(
                nav_panel("lit-tag-viewer",
                          tags$iframe(style="height:100vh; width:100%; scrolling=yes",
                                      src="www/LitTag viewer guide.pdf")),
                nav_panel("example files",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         # downloadButton(ns("unicorn_example"),
                                         #                "Unicorn db example files"),
                                         # HTML("<br>"),
                                         # HTML("<br>"),
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
