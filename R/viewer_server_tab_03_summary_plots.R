# R/viewer_server_tab_03_summary_plots.R

viewer_server_tab_03_summary_plots <- function() {
  ## Plot -------------------------------------------
  output$plot <- renderPlot(
    {
      ### Use full data set or the filtered data set? -----------
      d <- NULL
      if (input$plot_data == "Full dataset") {
        d <- values$d_mcdr_tagged
      } else {
        d <- values$d_mcdr_filtered
      }

      # ###  Number tags
      # if (
      #   is.null(input$plot_numbers_as_text) &
      #     !is.null(values$number_tags)
      # ) {
      #   d <- d %>%
      #     mutate(across(any_of(values$number_tags), as.numeric))
      # }

      ### Exclude N/A or missing from x-axis? -------
      d <- d %>%
        filter(
          !(!is.na(.data[[input$plot_x_var]]) &
            "Not applicable x-axis" %in% input$plot_remove &
            .data[[input$plot_x_var]] == "not_applicable")
        ) %>%
        filter(
          !("Missing x-axis" %in%
            input$plot_remove &
            is.na(.data[[input$plot_x_var]]))
        )

      ### Max character x var -----------
      max_x_char <- 20

      ### Author format function -----------------
      author_format <- function(author) {
        return(paste(
          str_trim(word(author, sep = ",")),
          str_sub(str_trim(word(author, 2, 2, sep = ",")), 1, 1),
          sep = "_"
        ))
      }

      bar_data <- "percent"
      if (input$show_bar_data == "Number of papers") {
        bar_data <- "n"
      }
      ### histogram for numberic tag
      #browser()
      if (
        is.null(input$plot_numbers_as_text) &
          !is.null(values$number_tags)
      ) {
        if (input$plot_x_var %in% values$number_tags) {
          h_plot_data <- data.frame(
            x_val = as.numeric(unlist(strsplit(
              d[[input$plot_x_var]],
              ";",
              fixed = TRUE
            )))
          )
          p <- h_plot_data %>%
            ggplot(aes(x_val)) +
            geom_histogram(fill = "blue", bins = input$hist_n_bins) +
            xlab(input$plot_x_var) +
            theme_bw(base_size = 24)
          print(p)
          return(p)
        }
      }

      ### If there is no stacking variable ----------
      if (input$plot_stack_var == "none") {
        #if the x-axis mutli-select is combined

        d_plot <- d %>%
          select(input$plot_x_var)
        if (!("Combine x-axis" %in% input$combine_multi)) {
          d_plot <- d_plot %>%
            separate_longer_delim(input$plot_x_var, delim = ";")
        }

        # the x-axis only plot
        d_plot_2 <- d_plot %>%
          mutate(
            !!input$plot_x_var := if_else(
              rep(input$plot_x_var == "author", nrow(.)),
              author_format(!!as.name(input$plot_x_var)),
              !!as.name(input$plot_x_var)
            )
          ) %>%
          tabyl(input$plot_x_var) %>%
          mutate(percent = paste(round(percent * 100), "%", sep = "")) %>%
          mutate(
            !!input$plot_x_var := str_replace_all(
              !!as.name(input$plot_x_var),
              ";",
              ", "
            )
          )

        n_total_paper <- sum(d_plot_2$n)

        # plot data to download for non-stacked plot
        values$d_plot <- d_plot_2

        p <- d_plot_2 %>%
          ggplot(aes(
            .data[[input$plot_x_var]],
            n,
            label = .data[[bar_data]]
          )) +
          geom_col(fill = "blue") +
          ylab("Number of Papers") +
          ggtitle(paste("Total instances =", n_total_paper)) +
          scale_x_discrete(labels = label_wrap(max_x_char)) +
          theme_bw(base_size = 24) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
        #### If show percent -------------------
        if (input$show_bar_data != "None") {
          p +
            geom_text(vjust = -1, color = "black", size = 8) +
            coord_cartesian(clip = "off") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
        } else {
          p
        }
      } else {
        ### If there is a stacking variable -----------------
        #### Is stack var multi-select? -----------------
        has_multi_select_stacked <-
          any(str_detect(d[[input$plot_stack_var]], ";"), na.rm = TRUE)

        if ("Combine stacked" %in% input$combine_multi) {
          has_multi_select_stacked <- FALSE
        }
        #### Exclude N/A or missing from stacked? -------
        d <- d %>%
          filter(
            !(!is.na(.data[[input$plot_stack_var]]) &
              "Not applicable stacked" %in% input$plot_remove &
              .data[[input$plot_stack_var]] == "not_applicable")
          ) %>%
          filter(
            !("Missing stacked" %in%
              input$plot_remove &
              is.na(.data[[input$plot_stack_var]]))
          )

        #if the x-axis mutli-select is combined
        d_plot <- d %>%
          select(input$plot_x_var)
        if (!("Combine x-axis" %in% input$combine_multi)) {
          d_plot <- d_plot %>%
            separate_longer_delim(input$plot_x_var, delim = ";")
        }

        #### d_z for multi-select stack ---------------

        d_x_expand <- d_plot %>%
          mutate(
            !!input$plot_x_var := if_else(
              rep(input$plot_x_var == "author", nrow(.)),
              author_format(!!as.name(input$plot_x_var)),
              !!as.name(input$plot_x_var)
            )
          ) %>%
          tabyl(input$plot_x_var) %>%
          mutate(percent = paste(round(percent * 100), "%", sep = ""))

        d_x_expand_n <- d_x_expand %>%
          select(any_of(input$plot_x_var), n)

        n_total_paper <- sum(d_x_expand_n$n)

        d_both_expand_f_1 <- d %>%
          select(all_of(c(input$plot_x_var, input$plot_stack_var)))

        #if the x-axis mutli-select is combined
        if (!("Combine x-axis" %in% input$combine_multi)) {
          d_both_expand_f_1 <- d_both_expand_f_1 %>%
            separate_longer_delim(input$plot_x_var, delim = ";")
        }

        d_both_expand_f_2 <- d_both_expand_f_1 %>%
          mutate(
            !!input$plot_x_var := if_else(
              rep(input$plot_x_var == "author", nrow(.)),
              author_format(!!as.name(input$plot_x_var)),
              !!as.name(input$plot_x_var)
            )
          )

        if (!("Combine stacked" %in% input$combine_multi)) {
          d_both_expand_f_2 <- d_both_expand_f_2 %>%
            separate_longer_delim(input$plot_stack_var, delim = ";")
        }

        d_both_expand_f_3 <- d_both_expand_f_2 %>%
          mutate(
            !!input$plot_stack_var := if_else(
              rep(input$plot_stack_var == "author", nrow(.)),
              author_format(!!as.name(input$plot_stack_var)),
              !!as.name(input$plot_stack_var)
            )
          ) %>%
          tabyl(.data[[input$plot_x_var]], .data[[input$plot_stack_var]]) %>%
          adorn_percentages()

        d_z <- d_x_expand_n %>%
          left_join(
            d_both_expand_f_3,
            join_by(!!as.name(input$plot_x_var))
          ) %>%
          mutate(across(-any_of(c(input$plot_x_var, "n")), ~ .x * n)) %>%
          select(-n) %>%
          pivot_longer(
            where(is.numeric),
            names_to = "stack_var",
            values_to = "n"
          )

        #### plot stacked ---------------------------

        p <- d_z %>%
          mutate(
            !!input$plot_x_var := str_replace_all(
              !!as.name(input$plot_x_var),
              ";",
              ", "
            )
          ) %>%
          mutate(stack_var = str_replace_all(stack_var, ";", ", ")) %>%
          ggplot(aes(
            x = .data[[input$plot_x_var]],
            y = n,
            fill = stack_var
          )) +
          geom_col() +
          ylab("Number of Papers or occurences") +
          ggtitle(paste("Total papers =", n_total_paper)) +
          scale_x_discrete(labels = label_wrap(max_x_char)) +
          scale_fill_discrete(labels = label_wrap(max_x_char)) +
          theme_bw(base_size = 24) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
          guides(fill = guide_legend(title = input$plot_stack_var))

        #### warning if stacked multiselect ------------------
        if (has_multi_select_stacked) {
          p <- p +
            ggtitle(
              paste("Total papers =", n_total_paper),
              subtitle = paste(
                "Note: The stacked variable has multiple",
                "selections for each x-axis variable.\nThe fill colors",
                "show the proportion of each stack value, not the",
                "absolute count."
              )
            )
          theme(plot.subtitle = element_text(size = 12))
        }
        #### if show percent ------------------
        if (input$show_bar_data != "None") {
          p <- p +
            geom_text(
              data = d_x_expand %>%
                mutate(stack_var = NA) %>%
                mutate(
                  !!input$plot_x_var := str_replace_all(
                    !!as.name(input$plot_x_var),
                    ";",
                    ", "
                  )
                ),
              aes(label = .data[[bar_data]]),
              vjust = -1,
              color = "black",
              size = 8
            ) +
            coord_cartesian(clip = "off") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
        }
        # plot data to download for stacked plot
        values$d_plot <- d_z
        p
      }
    },
    height = 800
  )

  ### download plot data -----------------------

  output$download_plot_data <- downloadHandler(
    filename = function() {
      paste(
        "lit_tag_plot_data_",
        format(now("UTC"), "%Y_%m_%d_%H%M_UTC"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      write_csv(values$d_plot, file)
    }
  )
}
