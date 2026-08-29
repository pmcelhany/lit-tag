# R/viewer_server_tab_06_help.R

viewer_server_tab_06_help <- function() {
  output$download_ris <- downloadHandler(
    filename = function() {
      paste("fisheries_mcdr_Zotero_2025_10_29", "ris", sep = ".")
    },
    content = function(file) {
      file.copy("fisheries_mcdr_Zotero_2025_10_29.ris", file)
    }
  )

  ## Download mcdr example button ------------------
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
