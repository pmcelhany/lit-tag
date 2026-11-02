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

    # Inject JS for resizable panels in the Search database tab
    insertUI(
      selector = "head",
      where = "beforeEnd",
      ui = tagList(
        tags$script(HTML(paste0(
          "
          (function() {
            function initResizerViewer() {
              const containers = document.querySelectorAll('bslib-layout-columns');
              for (const container of containers) {
                if (container.classList.contains('resizable-container')) continue;

                const panels = Array.from(container.querySelectorAll(':scope > .bslib-grid-item'));
                if (panels.length !== 3) continue;

                // Check if it's the right one by looking at headers
                const h3s = Array.from(container.querySelectorAll('h3')).map(h => h.textContent.trim());
                const isSearchDb = h3s.includes('Paper table (filtered)') &&
                                   h3s.includes('Search criteria') &&
                                   h3s.includes('Paper selection summary');

                if (!isSearchDb) continue;

                container.classList.add('resizable-container');

                // Set initial flex values
                panels.forEach(p => {
                  p.style.flex = '1 1 0px';
                });

                for (let i = 0; i < panels.length - 1; i++) {
                  const leftPanel = panels[i];
                  const rightPanel = panels[i+1];
                  const gutter = document.createElement('div');
                  gutter.className = 'gutter';
                  leftPanel.after(gutter);

                  gutter.addEventListener('mousedown', function(e) {
                    e.preventDefault();
                    const startX = e.pageX;
                    const leftWidth = leftPanel.getBoundingClientRect().width;
                    const rightWidth = rightPanel.getBoundingClientRect().width;
                    const totalWidth = leftWidth + rightWidth;

                    // Use getComputedStyle to get the actual flex-grow value
                    const leftFlex = parseFloat(window.getComputedStyle(leftPanel).flexGrow) || 1;
                    const rightFlex = parseFloat(window.getComputedStyle(rightPanel).flexGrow) || 1;
                    const totalFlex = leftFlex + rightFlex;

                    gutter.classList.add('dragging');
                    document.body.style.cursor = 'col-resize';

                    function onMouseMove(e) {
                      const deltaX = e.pageX - startX;
                      let newLeftWidth = leftWidth + deltaX;
                      let newRightWidth = rightWidth - deltaX;

                      if (newLeftWidth < 100) {
                        newLeftWidth = 100;
                        newRightWidth = totalWidth - 100;
                      }
                      if (newRightWidth < 100) {
                        newRightWidth = 100;
                        newLeftWidth = totalWidth - 100;
                      }

                      const newLeftFlex = (newLeftWidth / totalWidth) * totalFlex;
                      const newRightFlex = (newRightWidth / totalWidth) * totalFlex;

                      leftPanel.style.flex = newLeftFlex + ' 1 0px';
                      rightPanel.style.flex = newRightFlex + ' 1 0px';
                    }

                    function onMouseUp() {
                      gutter.classList.remove('dragging');
                      document.body.style.cursor = '';
                      document.removeEventListener('mousemove', onMouseMove);
                      document.removeEventListener('mouseup', onMouseUp);
                    }

                    document.addEventListener('mousemove', onMouseMove);
                    document.addEventListener('mouseup', onMouseUp);
                  });
                }
              }
            }

            $(document).on('shown.bs.tab', function() {
              setTimeout(initResizerViewer, 200);
            });

            const observer = new MutationObserver((mutations) => {
              for (const mutation of mutations) {
                if (mutation.addedNodes.length) {
                  initResizerViewer();
                }
              }
            });

            $(document).ready(function() {
              observer.observe(document.body, { childList: true, subtree: true });
            });
          })();
          "
        )))
      )
    )
  })
}
