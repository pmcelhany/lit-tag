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

    # Rebind environments of functions defined in other files
    environment(select_box_fun) <- environment()
    environment(is_invalid_numeric) <- environment()
    environment(category_meta_fun) <- environment()
    environment(category_remove_meta_fun) <- environment()
    environment(viewer_server_utils) <- environment()
    environment(viewer_server_tab_01_load_database) <- environment()
    environment(viewer_server_tab_02_search_database) <- environment()
    environment(viewer_server_tab_03_summary_plots) <- environment()
    environment(viewer_server_tab_04_summary_table) <- environment()
    environment(viewer_server_tab_05_reports) <- environment()
    environment(viewer_server_tab_06_help) <- environment()

    # Initialize utils and tabs
    viewer_server_utils()
    viewer_server_tab_01_load_database()
    viewer_server_tab_02_search_database()
    viewer_server_tab_03_summary_plots()
    viewer_server_tab_04_summary_table()
    viewer_server_tab_05_reports()
    viewer_server_tab_06_help()
  })
}
