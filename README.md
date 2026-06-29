# `{littag}`

## About Lit-tag

To facilitate the review, evaluation and analysis of scientific literature, the lit-tag R Shiny application provides a convenient interface for users to generate a citation database with custom, user-defined tags and notes. Lit-tag is not subject-specific and is useful for any field of research. Starting with a table of citations exported from a Zotero library and a user-generated Excel file describing a set of tags and notes fields, lit-tag provides tools for assigning tags and notes to papers ("lit-tag-builder" module) and for exporting, graphing, and generating reports from the resulting database ("lit-tag-viewer" module). The app fills a need not met by the limited tagging tools available in bibliographic software and does not require database programming skills.

## Lit-tag paper pre-print

A paper on lit-tag is currently available as pre-print ([McElhany, Grabb and Wood 2026](https://arxiv.org/abs/2603.19238)).

## Package installation

You can install `{littag}` as a package like so:

``` r
remotes::install_github("pmcelhany/lit-tag")
```

## Run app from package

You can launch the application by running:

``` r
library(littag)
littag::run_app()
```

## Run app from source code

1.  Open littag.proj in RStudio.

2.  Open app.R script.

3.  Click "Run App" button in top bar of script panel.

## Run on NOAA Fisheries server

To run lit-tag on the NOAA Fisheries Posit server, [click here](https://connect.fisheries.noaa.gov/lit-tag/).

## Publish on Posit sever

1.  Open littag.proj in RStudio.

2.  Open app.R script.

3.  Click "Publish" button in top bar of script panel.

## Quarto path

Generating reports in lit-tag viewer requires the installation of [Quarto](https://quarto.org/) on the local machine or on the Posit server, depending on where the app is run. The path to Quarto is set using the Sys.setenv() function in the lit-tag App.R script. The App.R script includes several common path configurations, but the Quarto installation is platform dependent and the path will depend on your particular deployment.
