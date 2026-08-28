

## 1. Scope & Targeted Files

> **Strict Context Constraint:** Do NOT read or modify files outside of
> this explicit list.

- **Primary Source File:** `R/builder_server.R`
- **Structure Reference File (Read-Only):** `R/builder_ui.R`
- **Allowed Output File:** `reorg_plan.md`

## 2. Objective & Expected Behavior

**Goal:** Draft a modularization plan in `reorg_plan.md`.

**STRICT CONSTRAINT:** DO NOT EDIT OR CREATE ANY R CODE FILES. DO NOT
ADD `source()` CALLS (Golem auto-loads files in `R/`). ONLY CREATE
`reorg_plan.md`.

**Architectural Rules & Naming Convention:** 1. Inspect `R/builder_ui.R`
to identify the **6 UI tabs**. 2. Map `R/builder_server.R` logic
directly to match these 6 tabs. 3. **Strict Naming Scheme:** -
Tab-specific files MUST use two-digit indexed lower_snake_case:
`R/builder_server_tab_01_<tab_name>.R`,
`R/builder_server_tab_02_<tab_name>.R`, …,
`R/builder_server_tab_06_<tab_name>.R`. - Shared helpers, non-tab
reactive expressions, and utility functions MUST be assigned to
`R/builder_server_utils.R`. 4. Preserve all original function names,
parameters, and Shiny reactivity behavior without changing features.

## 3. Required Output Format

Write `reorg_plan.md` using EXACTLY this Markdown structure (keep
descriptions concise):

``` markdown
# Builder Server Reorganization Plan

## Planned File Structure
- `R/builder_server_utils.R`: Common helper functions and shared reactive expressions.
- `R/builder_server_tab_01_<tab_name>.R`: Server logic for Tab 1 (<Tab Name>).
- `R/builder_server_tab_02_<tab_name>.R`: Server logic for Tab 2 (<Tab Name>).
- `R/builder_server_tab_03_<tab_name>.R`: Server logic for Tab 3 (<Tab Name>).
- `R/builder_server_tab_04_<tab_name>.R`: Server logic for Tab 4 (<Tab Name>).
- `R/builder_server_tab_05_<tab_name>.R`: Server logic for Tab 5 (<Tab Name>).
- `R/builder_server_tab_06_<tab_name>.R`: Server logic for Tab 6 (<Tab Name>).

## Function Mapping
### `R/builder_server_utils.R`
- `function_name_1()`
- `function_name_2()`

### `R/builder_server_tab_01_<tab_name>.R`
- `function_name_3()`
```

## 4. Step-by-Step Task Checklist

- [ ] Parse `R/builder_ui.R` to identify the 6 tab names in sequential
  order.
- [ ] Parse `R/builder_server.R` and map functions to their designated
  tab file or `R/builder_server_utils.R`.
- [ ] Output the completed plan into `reorg_plan.md` strictly following
  the target format.
