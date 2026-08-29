# Viewer Server Reorganization Plan

## Planned File Structure
- `R/viewer_server_utils.R`: Common helper functions and shared reactive expressions.
- `R/viewer_server_tab_01_load_database.R`: Server logic for Tab 1 (Load database).
- `R/viewer_server_tab_02_search_database.R`: Server logic for Tab 2 (Search database).
- `R/viewer_server_tab_03_summary_plots.R`: Server logic for Tab 3 (Summary plots).
- `R/viewer_server_tab_04_summary_table.R`: Server logic for Tab 4 (Summary table).
- `R/viewer_server_tab_05_reports.R`: Server logic for Tab 5 (Reports).
- `R/viewer_server_tab_06_help.R`: Server logic for Tab 6 (Help).

## Function Mapping
### `R/viewer_server_utils.R`
- `is_invalid_numeric()`
- `category_meta_fun()`
- `category_remove_meta_fun()`
- `select_box_fun()`
- `newjs`
- `values` (reactiveValues)
- `dt_proxy`
- `table_vars()`

### `R/viewer_server_tab_01_load_database.R`
- `output$db_chooser`
- `output$cat_chooser`
- `observeEvent(input$load_data)`
- `journal_abrev_fun()` (nested)
- `output$table_full` (nested rendering)
- `output$notes` (nested rendering)
- `output$n_papers_db` (nested rendering)
- `output$n_papers_selected` (nested rendering)
- `output$download_db`
- `output$download_tag_cat`

### `R/viewer_server_tab_02_search_database.R`
- `observeEvent(input$show_extra)`
- `output$table`
- `observeEvent(input$clear_all_criteria)`
- `clear_tag_input()` (nested)
- `observeEvent(input$select_papers)`
- `add_criteria()` (nested)
- `tag_df_fun()` (nested)
- `output$criteria_table` (nested rendering)
- `output$criteria_table_plot` (nested rendering)
- `output$criteria_table_summary` (nested rendering)
- `output$n_papers_selected` (nested rendering)
- `observeEvent(input$table_rows_selected)`
- `output$export_selected_csv`

### `R/viewer_server_tab_03_summary_plots.R`
- `output$plot`
- `author_format()` (nested)
- `output$download_plot_data`

### `R/viewer_server_tab_04_summary_table.R`
- `output$summary_table`
- `output$download_summary`

### `R/viewer_server_tab_05_reports.R`
- `render_quarto_fun()`
- `observeEvent(input$show_report)`
- `output$download_report`

### `R/viewer_server_tab_06_help.R`
- `output$download_ris`
- `output$mcdr_example`
