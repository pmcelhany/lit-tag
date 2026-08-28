# Builder Server Reorganization Plan

## Planned File Structure
- `R/builder_server_utils.R`: Common helper functions and shared reactive expressions.
- `R/builder_server_tab_01_tag_edit.R`: Server logic for Tab 1 (Tag edit).
- `R/builder_server_tab_02_new_database.R`: Server logic for Tab 2 (New database).
- `R/builder_server_tab_03_sync_zotero.R`: Server logic for Tab 3 (Sync Zotero).
- `R/builder_server_tab_04_database_maintenance.R`: Server logic for Tab 4 (Database Maintenance).
- `R/builder_server_tab_05_new_zotero.R`: Server logic for Tab 5 (New Zotero).
- `R/builder_server_tab_06_help.R`: Server logic for Tab 6 (Help).

## Function Mapping
### `R/builder_server_utils.R`
- `is_invalid_numeric()`
- `add_cols_if_missing()`
- `category_meta_fun()`
- `category_remove_meta_fun()`
- `select_box_fun()`
- `remove_leading_special_char()`
- `load_categories()`
- `read_zotero()`

### `R/builder_server_tab_01_tag_edit.R`
- `render_paper_info()`
- `save_tag_value()`
- `save_last_row()`
- `load_row_tags_fun()`
- `filter_fun()`

### `R/builder_server_tab_02_new_database.R`
- `output$new_database`

### `R/builder_server_tab_03_sync_zotero.R`
- `output$update_from_zotero`

### `R/builder_server_tab_04_database_maintenance.R`
- `tag_values_in_db()`
- `replace_tag_option()`
- `read_as_char()`
- `output$replace_tag_name_download`
- `output$replace_option_name_download`
- `output$delete_tags_download`
- `output$delete_tag_option_download`
- `output$delete_not_in_zotero_download`
- `output$delete_papers_with_tag_option_download`
- `output$download_combined`

### `R/builder_server_tab_05_new_zotero.R`
- `unescape_html()`
- `ris_fun()`
- `ris_tag_fun()`
- `output$generate_ris`
- `output$generate_new_keys_db`

### `R/builder_server_tab_06_help.R`
- `output$unicorn_example`
- `output$mcdr_example`
