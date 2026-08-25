

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

**Goal:** Return a number tag label from red back to the default label
color if the user changes a non-numeric value to a number.

- **Current Behavior:** If a number tag imports a non-number value, the
  tag label in the in the “Tags” card (“Tag edits” tab) will be red and
  will remain red even if the user enters a valid number
- **Expected Behavior:**
  1.  If the tag label in the in the “Tags” card (“Tag edits” tab) is
      red because the imported value is invalid, the tag label should
      change to the default label color if the user enters a valid
      number for the tag.

## 3. Implementation Requirements

- **Inputs/Outputs:** Identify and work within existing inputs/outputs
  in `builder_ui.R` / `builder_server.R`. Wrap all new UI element IDs in
  `ns()`.
- **Reactivity Rules:** Use `observeEvent()` or `eventReactive()` paired
  with `bindEvent()` for trigger validations. Do not run checks inside
  raw reactive expressions.
- **Package Calls:** Do not add any new libraries

## 4. Step-by-Step Task Checklist

- [ ] Change tag label color to default color if a user corrects an
  non-number by entering a number for a number type tag.
- [ ] Run the automated verification commands below.

## 5. Required Verification Protocol

Execute the following verification scripts in order and fix any errors
before outputting the final PR:

1.  **Loadability:** `Rscript -e "devtools::load_all()"`
2.  **Namespace Check:** `Rscript -e "devtools::document()"`
3.  **Syntax Validation:**
    `Rscript -e "invisible(lapply(list.files('R', pattern = '\\.R$', full.names = TRUE), parse))"`
