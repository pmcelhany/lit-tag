---
title: "lit-tag: an app for adding custom tags and notes to a citation database"
tags:
- R
- Zotero
- annotation
- citation database
- shiny
date: "04 December 2025"
output: pdf_document
authors:
- name: Paul McElhany
  orcid: "0000-0002-9826-755X"
  affiliation: '1'
- name: Kalina Grabb
  orchid: "0000-0001-8771-7191"
  affiliation: '2'
- name: Madison Wood
  orchid: "0009-0006-2511-3441"
  affiliation: '3'
bibliography: paper.bib
affiliations:
- index: 1
  name: NOAA Northwest Fisheries Science Center
- index: 2
  name: Dalhousie University
- index: 3
  name: NOAA Ocean Acidification Progam
---

# Summary

To facilitate the review, evaluation and analysis of scientific literature, the lit-tag Shiny application provides a convenient interface for users to generate a citation database with custom, user-defined tags and notes. Lit-tag is not subject-specific and is useful for any field of research. Starting with a table of citations exported from a Zotero library and a user-generated Excel file describing a set of tags and notes fields, lit-tag provides tools for assigning tags and notes to papers (“lit-tag-builder” module) and for exporting, graphing, and generating reports from the resulting database (“lit-tag-viewer” module). The application has been used in several scientific reviews related to marine carbon dioxide removal @grabb.; @mcelhany; @gurney-smith.

# Statement of need

Scientific literature review and meta-analysis projects often involve summarizing the contents of many, often hundreds, of papers @snyder2019. During the review, data are collected on many different attributes of the study (e.g., experiment type, treatment conditions, location, results, etc.). Although literature review projects may start out collecting this information in a spreadsheet, the approach quickly becomes unwieldy as the number of papers and attributes increases. At the same time researchers are compiling data on the contents of papers, they need to conveniently collect and use the full citation information for each paper. The lit-tag app links the contents of a library generated with Zotero @digitalscholar2025a, an open-source reference management software which has tools for easily downloading citation information and adding references to documents, with a database of user-defined paper attributes and notes.

# Design

The lit-tag app has two modules: 1) lit-tag-builder for generating, editing and updating the database and 2) lit-tag-viewer for generating tables, graphs and reports from the database \autoref{fig:overview}.

![Relationship between Zotero, lit-tag-builder module, lit-tag-viewer module and imported/exported files.\label{fig:overview}](images/lit_tag_overview.png)

The main editing tab in the builder module contains panels for selecting papers, viewing paper details and notes, and assigning tags to papers \autoref{fig:builder}. Other tabs in the builder module have tools for syncing with the Zotero database when adding new papers, database maintenance for global edits of the database (e.g., renaming a tag option, deleting categories), creating and linking to a new Zotero database and viewing the module user guide.

The viewer module contains options for searching and filtering the database (including custom searches using R syntax), plotting summary tables using any two tag variables for full and filtered datasets (Fig. 3), and generating custom tables (csv files) and reports (html, pdf or word). 

![Example screen shot of the “Tag edit” tab of lit-tag-builder module user interface.\label{fig:builder}](images/builder.png)

![Example screen shot of the “Summary plots” tab of the lit-tag-viewer module.\label{fig:viewer}](images/viewer.png)

# Acknowledgements

Madison Wood’s work on this project was supported by a Sea Grant Knauss Fellowship. Kalina Grabb’s work on this project was supported by funding from NOAA Ocean Acidification Program and NOAA Northeast Fisheries Science Center through IBSS. We would also like to thank the International Council for the Exploration of the Sea (ICES) mCDR x Fisheries Workshop group for inspiration and app testing.

# References
