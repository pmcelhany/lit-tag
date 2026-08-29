# R/builder_server_tab_06_help.R

#' @noRd
builder_server_tab_06_help <- function() {
  output$unicorn_example <- downloadHandler(
    filename = function() {
      "unicorn_example.zip"
    },
    content = function(file) {
      file.copy("data/unicorn_example.zip", file)
    },
    contentType = "application/zip"
  )

  output$mcdr_example <- downloadHandler(
    filename = function() {
      "mcdr_example.zip"
    },
    content = function(file) {
      file.copy("data/mcdr_example.zip", file)
    },
    contentType = "application/zip"
  )
}
