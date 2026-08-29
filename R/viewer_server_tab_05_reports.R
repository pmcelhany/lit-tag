# R/viewer_server_tab_05_reports.R

viewer_server_tab_05_reports <- function() {
  ## Render quarto fun -----------
  render_quarto_fun <- function(report_type) {
    d_report <- values$d_mcdr_tagged
    if (input$report_data == "Filtered dataset") {
      d_report <- values$d_mcdr_filtered
    }

    if (input$report_sort_order == "author") {
      d_report <- d_report %>%
        arrange(author, publication_year, title)
    }
    if (input$report_sort_order == "publication_year") {
      d_report <- d_report %>%
        arrange(publication_year, author, title)
    }
    if (input$report_sort_order == "title") {
      d_report <- d_report %>%
        arrange(title, author, title)
    }

    d_report <- d_report %>%
      replace(is.na(.), "NA")

    if (report_type == "pdf") {
      d_report <- d_report %>%
        mutate(across(everything(), \(x) escape_latex(x)))
    }

    categories <- values$categories %>%
      list_modify("notes" = rlang::zap())

    # make data frame of categories and tag names for quarto report
    max_tag_per_cat <- names(categories) %>%
      map(\(x) length(categories[[x]])) %>%
      unlist() %>%
      max()

    d_cat_tag <- NULL
    for (i in 1:length(categories)) {
      d_cat_temp <-
        data.frame(c(
          names(categories[[i]]),
          rep("NA", max_tag_per_cat - length(names(categories[[i]])))
        ))
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
        # note_var = values$tag_variables[str_detect(
        #   values$tag_variables,
        #   "note"
        # )]
        note_var = values$notes_variables,
        include_url = "paper_url" %in% input$report_include,
        include_abstract = "abstract" %in% input$report_include,
        include_tags = "tags" %in% input$report_include,
        include_tags_missing = "missing_tags" %in% input$report_include,
        include_tags_not_applicable = "not_applicable_tags" %in%
          input$report_include,
        include_notes = ("notes" %in%
          input$report_include &
          !is.null(values$notes_variables)),
        include_pagebreaks = "pagebreaks" %in% input$report_include
      )
    )
  }

  ## Show report button ------------------------
  observeEvent(input$show_report, {
    withProgress(message = "Rendering report", value = 0, {
      incProgress(1 / 4)
      render_quarto_fun("html")
      output$report <- renderUI({
        tags$iframe(
          src = paste0(
            "reports/lit_tag_report_template.html?t=",
            as.numeric(Sys.time())
          ),
          style = "width: 100%; height: 100vh; border: none;"
        )
      })
      incProgress(4 / 4)
    })
  })

  ## Downlaod report button --------------------
  output$download_report <- downloadHandler(
    filename = function() {
      paste(input$report_filename, input$report_type, sep = ".")
    },

    content = function(file) {
      withProgress(message = "Rendering report", value = 0, {
        incProgress(1 / 4)

        render_quarto_fun(input$report_type)

        # copy the quarto generated file to `file` argument.
        generated_file_name <- paste(
          "report/lit_tag_report_template",
          input$report_type,
          sep = "."
        )
        file.copy(generated_file_name, file)

        incProgress(4 / 4)
      })
    }
  )
}
