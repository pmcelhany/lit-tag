

# Repository Guidelines for AI Coding Agents

## Architecture Overview

- **Framework:** Complex R Shiny app built using
  `[e.g., Golem framework / standard UI-Server-Global / Modular Shiny]`.
- **Directory Layout:**
  - `R/`: UI modules, server functions, and helper scripts.
  - `www/`: Static assets, CSS stylesheets, and JavaScript files.
  - `global.R`: Global environment variables and package loadings.
- **Core Packages:**
  `[e.g., shiny, bslib, dplyr, plotly, targets, shinyjs]`.

## Universal Coding Rules

### 1. Shiny Reactivity & Namespacing

- **Strict Module Namespacing:** Always wrap input and output IDs in
  `ns()` within module UI functions. Access inputs inside server modules
  using `input$id` directly without `ns()`.
- **Prevent Re-executing Code:** Avoid putting heavy computation inside
  reactive expressions without caching or `bindCache()`/`bindEvent()`
  where applicable.
- **Isolate Side Effects:** Use `observeEvent()` for side effects and
  `eventReactive()` for reactive calculations triggered by inputs.

### 2. Code Quality & Modularity

- **Scope Changes:** Only edit files explicitly mentioned in the issue.
  Do not refactor untouched modules or rewrite existing working code
  unnecessarily.
- **Script Structure:** Maintain pure functions in helper scripts (e.g.,
  `R/utils_*.R`) and keep reactive logic strictly inside module server
  functions.
- **Style:** Follow `tidyverse` coding conventions. Use clear,
  self-documenting variable names.

### 3. File Handling & Anti-Patterns

- **Do NOT** change function signatures in existing shared utility files
  unless explicitly asked.
- **Do NOT** introduce top-level `source()` calls inside module files;
  all scripts in `R/` are auto-loaded.
- **Do NOT** modify `www/` static assets or CSS unless specified.

## Verification Checklist

Before submitting a pull request or completing a task, verify: - Code
parses without syntax errors. - All reactive IDs match their respective
`ns()` definitions. - New dependencies are explicitly listed in
`DESCRIPTION` or `global.R`.
