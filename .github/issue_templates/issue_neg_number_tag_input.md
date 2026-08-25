

## AGENT CONSTRAINTS (TOKEN OPTIMIZATION)

- **Target Scope:** Do NOT rewrite or re-output the entire server.R
  file.
- **Output Format:** Provide ONLY the specific modified functions or
  reactive blocks (e.g., `observeEvent(input$...)`) with surrounding
  context line numbers.
- **Skip Reasoning:** Do not output long natural-language explanations;
  provide only the code modifications and a 2-sentence summary.

## 1. Scope & Targeted Files

> **Strict Context Constraint:** Do NOT read or modify files outside of
> this explicit list.

- **Primary Target File(s):** `R/builder_server.R`
- **Secondary Reference File(s) (Read-Only):** None

## 2. Objective & Expected Behavior

**Goal:** Do not pop up the modal dialog that has
`title = "Invalid Input"` if the user has started to type a negative
number

- **Current Behavior:** The invalid date dialog pops up if the user
  starts to input a negative number.
- **Expected Behavior:**
  1.  Negative numbers are valid input for a number tag.
  2.  The invalid date dialog should not pop up if the user types a
      valid number.

## 3. Implementation Requirements

- **Inputs/Outputs:** Identify and work within existing inputs/outputs
  in `builder_ui.R` / `builder_server.R`. Wrap all new UI element IDs in
  `ns()`.
- **Modals:** Use native `shiny::showModal(shiny::modalDialog(...))`
  only. Do NOT add external notification packages.
- **Reactivity Rules:** Use `observeEvent()` or `eventReactive()` paired
  with `bindEvent()` for trigger validations. Do not run checks inside
  raw reactive expressions.
- **Package Calls:** Do not add any new libraries

## 4. Step-by-Step Task Checklist

- [ ] Allow user to enter negative numbers
- [ ] Run the automated verification commands below.

## 5. Required Verification Protocol

Execute the following verification scripts in order and fix any errors
before outputting the final PR:

1.  **Loadability:** `Rscript -e "devtools::load_all()"`
2.  **Namespace Check:** `Rscript -e "devtools::document()"`
3.  **Syntax Validation:**
    `Rscript -e "invisible(lapply(list.files('R', pattern = '\\.R$', full.names = TRUE), parse))"`
