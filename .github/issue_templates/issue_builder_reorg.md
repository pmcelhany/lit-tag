

## 1. Scope & Targeted Files

> **Strict Context Constraint:** Do NOT read or modify files outside of
> this explicit list.

- **Primary Target Files:** `R/builder_server.R` and `[R/builder_ui]`

## 2. Objective & Expected Behavior

**Goal:** Write a plan output in a markdown file for project changes
described below. DO NOT EDIT ANY CODE OR GENERATED ANY FILES EXCEPT THE
MARKDOWN PLAN.

**Plan for the following:** Move the contents of the current large
`builder_server.R` script into several shorter scripts based on
requirements listed below. Do not change any features or behavior of the
shiny app in any way. Do not alter the functions or code structure
except to the minimum extent needed to divide the contents of the
`builder_server.R` script into multiple smaller scripts.

- **Current Behavior:** There is a single builder_server.R script that
  is ~1,900 lines. The app currently works correctly.
- **Expected Behavior:** Several shorter scripts that result in the
  exact same app behavior.

## 3. Plan elements

- List of new R scripts
- The brief description of the contents of each of the new scripts a
  list of the functions currently in `builder_server.R` that will be in
  each script.

## 4. Step-by-Step Task Checklist

- [ ] Develop plan to break `builder_server.R` into multiple files
- [ ] Write a plan in markdown with the required elements
