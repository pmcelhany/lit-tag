

## 1. Scope & Targeted Files

> **Strict Context Constraint:** Do NOT read or modify files outside of
> this explicit list.

- **Primary Target File(s):** `R/builder_ui.R` , `R/builder_server.R`
- **Secondary Reference File(s) (Read-Only):**

## 2. Objective & Expected Behavior

**Goal:** Add option “number” tags in the categories file. Provide data
validation that user input number for number tags.

- **Current Behavior:** Tags can be “check_box_single”,
  “check_box_multiple”, “text_box”, or “date”
- **Expected Behavior:** New tag option for “number”. The user input for
  a number tag should only a number.
  - If the user enters a non-number value in the “Tags” card of the “Tag
    edits” tab , a dialog window should pop up prompting the user to
    enter a number.
  - If a non-number value is already in a loaded database for a number
    tag, an information dialog should pop up listing the name for the
    number tags with invalid (i.e. non-numeric) data and a message that
    the tags contain non-numeric data, but should contain only numbers.
  - If a non-number value is already in a loaded database for a number
    tag, the text for the value for the tag in the “Tags” card of the
    “Tag edits” tab should be colored red to indicate that is a
    non-numeric value.
  - Missing or “NA” values should be ignored with regard to whether or
    not they are numeric (i.e. a missing or “NA” value should not
    trigger a message about non-numeric values.

## 3. Implementation Requirements

- **Inputs/Outputs:**
- **Namespacing:** Ensure `ns()` is used in UI; raw IDs used in server.
- **Reactivity Rules:** Use `observeEvent()` or `eventReactive()` with
  `bindEvent()` where applicable to prevent unwanted re-executation.
- **Package Calls:** Use explicit `pkg::function()` syntax. Do NOT add
  `library()` calls or import new packages.

## 4. Step-by-Step Task Checklist

- [ ] Implement new number tag option in `R/builder_ui.R` ,
  `R/builder_server.R.`
- [ ] Ensure the changes handle NULL or empty states safely.
- [ ] Run the automated verification commands below.

## 5. Required Verification Protocol

Execute the following verification scripts in order and fix any errors
before outputting the final PR:

1.  **Loadability:** `Rscript -e "devtools::load_all()"`
2.  **Namespace Check:** `Rscript -e "devtools::document()"`
3.  **Syntax Validation:**
    `Rscript -e "invisible(lapply(list.files('R', pattern = '\\.R$', full.names = TRUE), parse))"`
