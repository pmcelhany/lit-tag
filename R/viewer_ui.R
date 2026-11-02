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
viewer_ui <- function(id) {
  ns <- NS(id)

  golem_opts <- golem::get_golem_options()
  app_title <- if (!is.null(golem_opts$app_title)) {
    golem_opts$app_title
  } else {
    "Viewer"
  }
  app_icon <- if (!is.null(golem_opts$app_icon)) {
    golem_opts$app_icon
  } else {
    "www/lit_tag_logo_1.png"
  }

  page_navbar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
    ),

    ## App title --------------------------------
    title = div(
      img(src = app_icon, style = "max-height: 8vh; min-height: 50px; object-fit: contain;"),
      h1(app_title),
      HTML("&nbsp; &nbsp;"),
      style = "color: brown; display: flex; align-items: center;"
    ),

    ## Load database panel -------------------------
    nav_panel(
      h2("Load database"),
      layout_sidebar(
        sidebar = sidebar(# If in app data, show db info after loading
          uiOutput(ns("in_app_db_info")),

          # If user data, show file choosers
          uiOutput(ns("db_chooser")),
          uiOutput(ns("cat_chooser")),

          ######
          # Always show load and download buttons
          uiOutput(ns("load_button")),
          div(class = "mb-4"),
          downloadButton(ns("download_db"), "Download database"),
          div(class = "mb-4"),
          downloadButton(ns("download_tag_cat"), "Download tag categories"),
          div(class = "mb-4"),
          downloadButton(
            ns("download_ris_load"),
            "Download citation file (.ris)"
          )
        ),
        DTOutput(ns("table_full"))
      )
    ),
    ## Search db panel ---------------------
    nav_panel(
      h2("Search database"),
      #useWaiter(),
      layout_columns(
        col_widths = breakpoints(md = c(12, 12, 12), lg = c(6, 6, 12), xl = c(4, 4, 4)),
        ### Left panel: table of papers ---------------------
        card(
          card_header(h3("Paper table (filtered)")),
          card_body(
            checkboxInput(
              ns("show_extra"),
              "Show \"extra\" field",
              value = FALSE
            ),
            fill = FALSE
          ),
          card_body(
            DTOutput(ns('table'))
          )
        ),

        ### Center panel: search criteria ---------------------------
        card(
          card_header(h3("Search criteria")),
          input_task_button(ns("clear_all_criteria"), "Clear all criteria"),
          checkboxInput(
            ns("exclude_obsolete"),
            "Exclude obsolete papers",
            value = TRUE
          ),

          virtualSelectInput(
            inputId = ns("select_missing"),
            label = "Select missing values",
            choices = "",
            hideClearButton = TRUE,
            disableSelectAll = TRUE,
            multiple = TRUE,
            disableOptionGroupCheckbox = TRUE
          ),
          card(
            card_header(h4("Paper criteria")),
            textInput(ns("author"), "Author"),
            airYearpickerInput(
              ns("years"),
              "Years",
              range = TRUE,
              minDate = "1960-01-01",
              maxDate = "2026-01-01"
            ),
            textInput(ns("title"), "Title"),
            textInput(ns("abstract"), "Abstract")
          ),
          card(card_header(h4("Notes criteria")), uiOutput(ns("notes"))),
          card(
            card_header(h4("Tag criteria")),
            navset_pill(id = ns("tag_tabs"))
          ),
          textInput(
            ns("custom_search"),
            "Advanced custom search (R condition syntax)",
            width = "100%"
          )
        ),

        ### Right panel: selected papers summary --------------------
        card(
          card_header(h3("Paper selection summary")),
          input_task_button(ns("select_papers"), "Select papers"),
          textOutput(ns("n_papers_db")),
          textOutput(ns("n_papers_selected")),
          card(
            card_header(h4("Selection criteria")),
            DTOutput(ns("criteria_table"))
          ),
          # card(card_header(h4("Selection summary")),
          #      textOutput(ns("n_papers_db")),
          #      textOutput(ns("n_papers_selected")))
        )
      )
    ),

    ## Summary graphs panel ---------------------
    nav_panel(
      h2("Summary plots"),
      layout_sidebar(
        sidebar = sidebar(h3("Plot Variables"),
          radioButtons(
            ns("plot_data"),
            "Plot data",
            choices = c("Full dataset", "Filtered dataset"),
            selected = "Full dataset"
          ),
          checkboxGroupInput(
            ns("plot_remove"),
            "Exclude",
            choices = c(
              "Not applicable x-axis",
              "Missing x-axis",
              "Not applicable stacked",
              "Missing stacked"
            )
          ),
          checkboxGroupInput(
            ns("combine_multi"),
            "Combine multi-select",
            choices = c("Combine x-axis", "Combine stacked")
          ),
          checkboxGroupInput(
            ns("plot_numbers_as_text"),
            "Number tags",
            choices = c("Plot number tags as text")
          ),
          numericInput(
            ns("hist_n_bins"),
            "Histogram number of bins",
            value = 30,
            min = 5,
            max = 50
          ),
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
          radioButtons(
            ns("show_bar_data"),
            "Show bar data value",
            choices = c("None", "Percent", "Number of papers"),
            selected = "None"
          ),
          downloadButton(ns("download_plot_data"), "Download plot data"),
          div(class = "mb-3"),
          div(class = "mb-3"),
          card(
            card_header("Selection criteria"),
            DTOutput(ns("criteria_table_plot"))
          )
        ),
        h3("Plot"), plotOutput(ns("plot"))
      )
    ),
    ## Summary table panel ---------------------------
    nav_panel(
      h2("Summary table"),
      layout_sidebar(
        sidebar = sidebar(radioButtons(
            ns("summary_data"),
            "Summary data",
            choices = c("Full dataset", "Filtered dataset"),
            selected = "Full dataset"
          ),
          virtualSelectInput(
            inputId = ns("summary_var"),
            label = "Summary variables",
            choices = "",
            hideClearButton = FALSE,
            multiple = TRUE,
            disableOptionGroupCheckbox = TRUE
          ),
          textInput(
            ns("summary_download_name"),
            "Summary download filename",
            value = "Untitled"
          ),
          downloadButton(
            ns("download_summary"),
            "Download summary table (.csv)"
          ),
          div(class = "mb-4"),
          downloadButton(
            ns("download_ris_summary"),
            "Download citation file (.ris)"
          ),
          div(class = "mb-4"),
          card(
            card_header("Selection criteria"),
            DTOutput(ns("criteria_table_summary"))
          )
        ),
        DTOutput(ns("summary_table"))
      )
    ),
    ## Reports panel --------------------------
    nav_panel(
      h2("Reports"),
      layout_sidebar(
        sidebar = sidebar(textInput(ns("report_title"), "Report title:", value = "untitled"),
          textInput(ns("report_author"), "Report author:", value = ""),

          radioButtons(
            ns("report_data"),
            "Report data",
            choices = c("Full dataset", "Filtered dataset"),
            selected = "Filtered dataset"
          ),
          selectInput(
            ns("report_sort_order"),
            "Report sort order",
            choices = c("author", "publication_year", "title"),
            selected = "author"
          ),
          checkboxGroupInput(
            ns("report_include"),
            "Include in report",
            choices = c(
              "paper_url",
              "abstract",
              "tags",
              "missing_tags",
              "not_applicable_tags",
              "notes",
              "pagebreaks"
            ),
            selected = c(
              "paper_url",
              "abstract",
              "tags",
              "missing_tags",
              "not_applicable_tags",
              "notes"
            )
          ),
          actionButton(ns("show_report"), "Show report"),
          textInput(
            ns("report_filename"),
            "Report filename:",
            value = "untitled"
          ),
          radioButtons(
            ns("report_type"),
            "Report download file type:",
            choices = c("html", "pdf", "docx"),
            selected = "html"
          ),
          downloadButton(ns("download_report"), "Download report")
        ),
        htmlOutput(ns("report"), style = "width: 100%;")
      )
    ),
    ## Help panel ------------------------
    nav_panel(
      h2("Help"),
      navset_pill(
        nav_panel(
          "lit-tag-viewer",
          tags$iframe(
            style = "height:100vh; width:100%; scrolling=yes",
            src = "www/lit_tag_viewer_guide.pdf"
          )
        ),
        nav_panel(
          "example files",
          layout_sidebar(
        sidebar = sidebar(# downloadButton(ns("unicorn_example"),
              #                "Unicorn db example files"),
              # div(class = "mb-3"),
              # div(class = "mb-3"),
              downloadButton(ns("mcdr_example"), "mCDR db example files")
            )
      )
        )
      )
    )
  )
}
