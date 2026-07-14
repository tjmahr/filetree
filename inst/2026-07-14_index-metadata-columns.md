---
status: done
---

# Prefix `ft_index()` Metadata Columns

Change the `ft_index()` output contract so columns that are not extracted
fields consistently begin with `.`. Replace the single file-template result
value with an always-present `.file_template` column. Make detailed component
template matches available through an optional `.templates` list-column.

## Key Files

- `inst/overview.md` -- durable package context and indexing output contract.
- `R/filetree.R:ft_index()` -- builds, validates, and orders index columns.
- `R/filetree.R:.ft_path_table()` -- creates path-derived index metadata.
- `R/filetree.R:ft_glimpse_problems()` -- consumes index metadata columns.
- `tests/testthat/test-filetree.R` -- regression coverage for index output.

## Work Items

- [x] Define the `.templates` list-column semantics.
- [x] Add a failing regression for file-layer classification by path depth.
- [x] Add a regression showing a template registered to the wrong layer cannot
  override path-depth classification.
- [x] Remove invalid parent-layer file-template resolution.
- [x] Add failing tests for the revised output contract.
- [x] Prefix `at_layer` and all `layer__*` output columns with `.`.
- [x] Replace `template` with a `.templates` list-column of matched templates.
- [x] Update internal consumers and user-facing documentation other than
  `README.Rmd`.
- [x] Format the changed R code and run package tests.
- [x] Add `.file_template` to every index.
- [x] Make `.templates` opt-in with `include_templates = FALSE` by default.
- [x] Verify the compact default restores large-index performance.

## Design Decisions

- Every `ft_index()` column that is package metadata rather than an extracted
  field begins with `.`.
- `.file_template` contains the matched ordinary file-template name, or `NA`.
- `.templates` is an optional list-column of long-form tibbles with columns
  `layer`, `type`, `name`, and `value`, enabled by `include_templates = TRUE`.
- A path component occupies exactly one layer and is either a directory or a
  file. File ownership is determined by path depth, not by a template match on
  an already-consumed parent layer.
- Do not modify `README.Rmd`.

## Decision Log

### 2026-07-14 - Request

**Request:** Rename `at_layer` to `.at_layer`, rename `layer__<name>` columns
to `.layer__<name>`, and replace `template` with a `.templates` list-column that
identifies the templates used by each indexed path.

**Rationale:** Extracted fields should be visually distinct from structural and
diagnostic metadata in the index tibble.

### 2026-07-14 - Tracking Location

**Decision:** Store the working note in `inst/`, following the repository's
existing dated task-note convention.

**Rationale:** This repository uses `inst/` rather than `_dev/todos/` for task
tracking documents.

### 2026-07-14 - Template Match Representation

**Superseded decision:** Use one `.templates` list-column whose rows contain
entries named for the matched component and layer, such as `.dir__subject` and
`.file__subject`.

**Reason superseded:** The proposal encoded component type and layer into field
names. The later design uses normalized rows instead, and the layer invariant
clarifies that one path cannot use both component types at the same layer.

### 2026-07-14 - Long-Form Template Matches

**Decision:** Each `.templates` cell is a tibble with columns `layer`, `type`,
`name`, and `value`. Each row represents one matched directory or file
template. `value` is the raw directory name or filename matched by the
template.

**Rationale:** The normalized representation handles multiple template matches
per indexed path without encoding layer and component type into field names.

**Details:** Match rows are ordered by path position, with directory matches
before the filename match. A matching template is recorded even when one of its
captures conflicts with an earlier extracted value. Paths without successful
matches and inert ignored audit rows contain a typed empty tibble. Ignore
template matches remain represented by `.ignore_template` and `.ignore_type`
rather than being duplicated in `.templates`.

### 2026-07-14 - Invalid Parent-Layer File Classification

**Observation:** The sidecar regression registered `{subject}-manifest.txt` as
a `subject` file template even though the `subject` component of paths such as
`ab-01/ab-01-manifest.txt` is already occupied by the `ab-01` directory. The
file is the next component and therefore belongs to the `time` layer.

**Reproduction:** Initial path classification assigns these files to `time`.
`.ft_resolve_file_layers()` then considers both `time` and the already-consumed
`subject` layer. A matching subject file template overwrites `.at_layer` with
`subject`; even an unmatched filename is assigned to `subject` through the
resolver's fallback. Registering the manifest template at `time` leaves all
three files correctly classified as `time`.

**Root cause:** `.ft_candidate_file_layers()` includes `n_dir` as a parent
candidate in addition to the depth-implied `n_dir + 1` layer, violating the
one-component-per-layer invariant.

### 2026-07-14 - Depth Is Authoritative

**Decision:** Remove parent-layer file resolution. A path component's position
determines its layer, whether the component is a directory or a file. File and
ignored-file templates are considered only at the depth-implied layer.

**Rationale:** This directly enforces one component per layer and removes the
invalid state rather than diagnosing it after reassignment.

**Partial schemas:** With `strict = FALSE`, a file at a layer without file
templates remains accepted. With `strict = TRUE`, it receives the existing
missing-file-template problem for its path-depth layer.

**Wrong-layer registration:** Registering both directory and file templates at
a layer is valid because they describe alternative component types on sibling
paths. A file template registered at the wrong depth is therefore not a schema
construction error, but it cannot match or reclassify a file at another layer.

**Testing:** Cover both a correctly registered `time` sidecar template and the
former bad schema with the manifest template registered at `subject`. In the
bad schema, the files must remain classified at `time` and must not match or
fall back to the `subject` template.

### 2026-07-14 - Implementation

**Change:** Removed parent-layer candidate resolution for ordinary and ignored
file templates. `.ft_path_table()` now remains authoritative for `.at_layer`.

**Change:** Renamed raw index metadata to `.at_layer` and `.layer__<name>` and
updated `when` matching, diagnostics, and problem batching to use the dotted
names.

**Change:** Replaced scalar `template` output with `.templates`. Successful
directory and file matches are accumulated as character data and materialized
once per index row as a tibble with `layer`, `type`, `name`, and `value`.

**Testing:** Added red-green regressions for corrected sidecar registration,
wrong-layer ordinary templates, dotted metadata, template match records,
capture conflicts, ignored audit rows, and wrong-layer ignore templates. The
ignore regression was mutation-checked by temporarily restoring the parent
depth error and observing the expected failure.

### 2026-07-14 - Template Match Performance

**Observation:** Appending a tibble with `dplyr::bind_rows()` for every match
made a 20,000-row synthetic index take longer than 30 seconds.

**Decision:** Accumulate four character vectors per path and construct each
nested tibble once after matching.

**Verification:** The optimized 20,000-row benchmark completed in 1.41 seconds.
A 200,000-row benchmark completed in 15.94 seconds and produced a 470.7 MB
index. The nested per-row tibbles have a meaningful time and memory cost
relative to the former scalar template column.

### 2026-07-14 - Test Verification

**Verification:** After documentation and formatting, `devtools::test()`
passed with `FAIL 0 | WARN 0 | SKIP 0 | PASS 236`.

**Verification:** `devtools::check(document = FALSE)` completed with
`0 errors | 0 warnings | 0 notes`.

### 2026-07-14 - Durable Documentation Location

**Decision:** Keep feature-specific decisions, investigation details,
performance measurements, and verification in this dated work note. Keep the
current package contract and architecture in `inst/overview.md`. Do not retain
separate design or implementation-plan artifacts outside `inst/`.

**Rationale:** The repository already uses `inst/` for agent-facing work notes
and durable package context. Consolidating there avoids parallel documentation
trees and keeps the package check clean without a `docs` build exclusion.

### 2026-07-14 - Optional Template Match Details

**Problem:** Always constructing `.templates` requires one nested tibble per
indexed path. In the synthetic 200,000-path benchmark, this increased runtime
from the historical 3--4 seconds to 15.94 seconds and produced a 470.7 MB
index. Most callers only need to know which file template matched.

**Decision:** `ft_index()` gains an `include_templates` argument that defaults
to `FALSE`. The `.templates` list-column is absent by default. When
`include_templates = TRUE`, it retains the current long-form tibble format with
character columns `layer`, `type`, `name`, and `value`.

**Decision:** Every index includes a `.file_template` character column. It
contains the name of the file template that matched the final path component.
Unmatched files, files at layers without a file template, structurally invalid
paths, and inert ignored audit rows contain `NA_character_`.

**Implementation constraint:** When `include_templates = FALSE`, do not collect
directory-template match records or construct per-row template tibbles merely
to discard them. Record the successful file-template name directly in
`.file_template`. This is the intended performance improvement, rather than
only hiding `.templates` from the returned tibble.

**Testing:** Cover the default absence of `.templates`, the always-present
`.file_template`, opt-in long-form template records, the selected name when
multiple file templates are registered, `NA` values for unmatched and ignored
rows, and scalar logical validation for `include_templates`.

**Documentation:** Update the `ft_index()` reference and `inst/overview.md` to
describe the default compact index and opt-in audit detail. Do not modify
`README.Rmd`.

### Implementation Plan: Compact Template Metadata

**Goal:** Always return the matched file-template name while constructing the
long-form template audit only when explicitly requested.

**Architecture:** Initialize `.file_template` as a character vector alongside
the index table and assign the winning template name in the existing file-match
loop. Guard directory and file audit-record accumulation with
`include_templates`, and only materialize `.templates` when that argument is
`TRUE`.

**Files:** Modify `R/filetree.R`, `tests/testthat/test-filetree.R`,
`inst/overview.md`, this work note, and generated `man/ft_index.Rd`.
`README.Rmd` remains untouched.

- [x] Add tests proving that the default result includes `.file_template` but
  excludes `.templates`, including selected, unmatched, and ignored rows.
- [x] Run the focused tests and confirm they fail because the new API is absent.
- [x] Add scalar-logical validation for `include_templates`, populate
  `.file_template`, and skip audit collection/materialization by default.
- [x] Run the focused tests and confirm the compact default behavior passes.
- [x] Update existing audit tests to request `include_templates = TRUE` and
  confirm the long-form representation is unchanged.
- [x] Add an argument-validation snapshot and confirm it passes.
- [x] Update roxygen documentation and `inst/overview.md`, then regenerate
  `man/ft_index.Rd`.
- [x] Format the package, run the full test suite, benchmark default and opt-in
  indexing, and run `devtools::check(document = FALSE)`.
- [x] Record verification and benchmark results here, review the diff, and
  commit the implementation without including unrelated user changes.

### 2026-07-14 - Compact Template Metadata Implementation

**Change:** Added `.file_template` as an always-present character column. The
file-template matching loop writes the winning template name directly; rows
without an ordinary file-template match retain `NA_character_`. After
validation, matched rows with any problems are also reset to `NA_character_`;
opt-in `.templates` continues to preserve their successful match audit.

**Change:** Added `include_templates = FALSE` to `ft_index()`. Directory and
file audit records are neither accumulated nor materialized unless the caller
opts in. With `include_templates = TRUE`, `.templates` retains its existing
long-form schema and match ordering.

**Red-green verification:** The compact-output regression first failed because
`.file_template` was absent and `.templates` was always returned. The argument
snapshot first failed with the base `if (NA)` error instead of the intended
argument-specific message. Both passed after their respective implementation
changes.

**Benchmark:** On a synthetic 200,000-path index in the same R session, compact
output took 6.85 seconds and occupied 21.5 MB. Opt-in template audit output took
12.72 seconds and occupied 390.8 MB. Omitting the audit reduced elapsed time by
about 46% and result size by about 94.5% for this workload.

**Review:** Independent review identified that a filename match could remain in
`.file_template` when directory or capture validation made the row invalid.
Added red-green coverage for both cases and clear `.file_template` after `.ok`
is computed. Re-review found no remaining critical or important code issues.

**Verification:** `devtools::test()` passed with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 244`. `devtools::check(document = FALSE)`
completed with `Status: OK` and `0 errors | 0 warnings | 0 notes`.
