---
status: done
---

# Ignore Templates and Root API

Track design and implementation work for adding first-class ignored file and
directory templates, plus an official API for changing a `filetree` root.

## Context

`filetree` currently models a schema as a root, ordered layers, a global regex
pool, directory templates, and file templates. `ft_index()` lists files under
`ft$root`, computes relative paths, validates directory components, resolves
the owning file layer, validates file templates, and returns only indexed files
with `.ok` and `.problems` diagnostics.

The package already supports conditional templates via `when` and
template-local regex overrides via `with`. File templates can attach to any
layer, including parent layers for sidecar files.

## Goals

- Add `ft_ignore_file_template()` and `ft_ignore_dir_template()` as public API.
- Make ignored matches excluded from `ft_index()` results.
- Keep the new functions parallel to the existing `ft_add_*_template()`
  functions where that remains semantically clean.
- Add an official API for changing the root of an existing `filetree` object.

## Key Files

- `R/filetree.R` - core object structure, template registration, indexing, and
  display.
- `tests/testthat/test-filetree.R` - current package regression coverage.
- `inst/overview.md` - durable architecture and semantics overview.
- `README.Rmd` - user-facing examples when the API is finalized.
- `NAMESPACE` and `man/*.Rd` - generated from roxygen after API changes.

## Work Items

- [x] Clarify ignored-template semantics and edge cases.
- [x] Clarify the root-changing API name and behavior.
- [x] Decide object storage for ignored file and directory templates.
- [x] Decide whether ignored templates support `when` and `with`.
- [x] Decide where ignore filtering happens inside `ft_index()`.
- [x] Design schema/print display for ignored templates.
- [x] Add tests for ignored files, ignored directories, conditionals, and root
  replacement.
- [x] Implement the approved API.
- [x] Update roxygen docs, `NAMESPACE`, `README.Rmd`, `README.md`, and
  `inst/overview.md`.
- [x] Run formatter and package tests.

## Implementation Plan

1. Add failing tests in `tests/testthat/test-filetree.R` for:
   - `ft_set_root()` replacing direct `$root` mutation.
   - `ft_ignore_file_template()` dropping matching files from `ft_index()`.
   - `ft_ignore_dir_template()` pruning an entire subtree from `ft_index()`.
   - `ft_list()` honoring `include_ignored`.
   - `include_ignored = TRUE` returning inert ignored rows with `.ignored`,
     `.ignore_template`, and `.ignore_type`.
   - `when` and `with` on ignore templates.
   - ignored rows not creating strict-mode or malformed-name diagnostics.
2. Verify those tests fail because the public functions/arguments do not exist.
3. Implement the minimal object/API changes in `R/filetree.R`:
   - root validation helper and `ft_set_root()`;
   - new ignore template slots in `ft_init()`;
   - recompile support for ignore template slots;
   - `ft_ignore_dir_template()` / `ft_ignore_file_template()`.
4. Add ignore classification helpers:
   - build path-derived table once;
   - evaluate directory ignore templates with subtree semantics;
   - evaluate file ignore templates against candidate file layers;
   - return `.ignored`, `.ignore_template`, and `.ignore_type`.
5. Wire classification into `ft_list()` and `ft_index()`.
6. Update schema/print summaries to expose ignored templates.
7. Regenerate docs and update durable package docs/examples.
8. Run formatting and verification commands.

## Design Questions

- Resolved: ignored rows should be dropped by default. `ft_index()` should have
  an `include_ignored` option that keeps ignored files for audit/debugging, but
  ignored rows must not participate in any validation.
- Resolved: ignored directory templates prune entire subtrees before
  file-template validation. This is the main feature.
- Resolved: ignored templates should mirror ordinary template functions,
  including `layer`, `template`, `when`, and `with`.
- Resolved: ignored templates may use captures internally for matching and
  condition evaluation, but ignored captures should not become ordinary
  extracted index columns.
- Should ignored file templates participate in sidecar file-layer resolution,
  or should they run only after `at_layer` is known?
- What should happen when a path matches both an ignore template and a normal
  validation template?
- Resolved: use `ft_set_root(ft, root)` as the official root-changing API.
- Resolved: `ft_set_root()` should mirror `ft_init()` root validation and
  normalization: require a single non-empty path and store `fs::path_abs(root)`,
  without requiring the path to exist.

## Initial Design Notes

- Prefer explicit ignore-template storage over overloading ordinary templates.
  This keeps validation templates and exclusion rules inspectable as separate
  concepts.
- Use the pre-validation classifier architecture: ignore templates live in
  separate slots and are evaluated before normal validation. This lets
  `ft_list()` prune ignored files and keeps ignored paths outside the schema
  contract.
- Use consistent diagnostic-style audit columns for ignored rows:
  `.ignored`, `.ignore_template`, and `.ignore_type`.
- When `include_ignored = TRUE`, ignored rows should still include path-derived
  fields such as `.path`, `.rel`, `at_layer`, and `layer__*`; these are
  structural audit metadata, not validation output.
- Ignored file templates should be evaluated before normal validation and
  before diagnostic creation, because the user request says files that match are
  ignored and not included in `ft_index()`.
- Ignored directory templates use subtree semantics: when a directory component
  matches an ignored directory template at its layer, all files under that
  directory are excluded from `ft_index()` unless `include_ignored = TRUE`.
- Keeping ignored templates parallel to `ft_add_*_template()` means the same
  public argument shape: `ft`, `layer`, `template`, optional `when`, optional
  `with`.
- Ignored template captures are match-time metadata. They can satisfy local
  matching and `when` semantics, but should not populate extracted columns in
  normal index output.
- A root-changing helper should avoid direct slot mutation in examples and
  tests. Existing tests currently do `ft_fail$root <- root2`, which is a signal
  that the package wants an official helper.
- Use `ft_set_root(ft, root)` for root changes. It should be pipe-friendly and
  return the updated `filetree` object.

## Decision Log

### 2026-07-01 - Request

**Request:** Add ignored file and directory template APIs that parallel
`ft_add_*_template()` and exclude matching files from `ft_index()`. Also add an
official API for changing the root of a `filetree` object.

**Context reviewed:** `inst/overview.md`, current `R/filetree.R`, and current
tests.

**Status:** Brainstorming and API design pending.

### 2026-07-01 - Ignored Rows Are Non-Validating

**Decision:** Ignored files are excluded by default. If the caller opts in with
`include_ignored = TRUE`, ignored files may appear in `ft_index()` output for
inspection, but they must not participate in directory validation, file
validation, capture extraction, conflict checks, strict-mode missing-template
diagnostics, or `.ok` problem reporting.

**Decision:** Give `ft_list()` the same `include_ignored` option so ignored files
can be pruned before `ft_index()` builds and validates its table. `ft_index()`
should pass the option through when it calls `ft_list()`.

**Rationale:** Ignored files are explicitly outside the schema contract. Showing
them is an audit/debugging feature, not a request to validate them.

### 2026-07-01 - Option Name

**Decision:** Use `include_ignored` rather than `show_ignored` for both
`ft_list()` and `ft_index()`.

**Rationale:** The option controls membership in the returned file vector or
index, so "include" is clearer and consistent across both APIs.

### 2026-07-01 - Directory Ignore Semantics

**Decision:** Ignored directory templates prune entire subtrees. When a
directory component at a configured layer matches an ignored directory template,
every file below that directory is ignored.

**Rationale:** This is the main user-facing value of directory ignores. It lets
schemas skip whole branches such as temporary, derivative, cache, or unrelated
data directories before ordinary validation work begins.

### 2026-07-01 - Ignore Templates Mirror Template API

**Decision:** `ft_ignore_file_template()` and `ft_ignore_dir_template()` should
mirror `ft_add_file_template()` and `ft_add_dir_template()`: same `ft`, `layer`,
`template`, `when`, and `with` argument shape, same template naming behavior,
and same full-component literal-template semantics.

**Decision:** Captures from ignored templates are used only to determine whether
the ignore rule matches. They should not create or update extracted columns in
the ordinary index.

**Rationale:** Mirroring the existing functions keeps the API predictable while
preserving the core meaning of ignore rules: they classify paths as outside the
validation contract.

### 2026-07-01 - Root API

**Decision:** Add `ft_set_root(ft, root)`.

**Behavior:** Validate that `ft` is a `filetree` object. Validate `root` the
same way as `ft_init()`: a single non-empty path. Store `fs::path_abs(root)`.
Do not require the path to exist.

**Rationale:** The helper is explicit, pipe-friendly, and avoids direct slot
mutation in examples/tests without introducing replacement-function complexity.

### 2026-07-01 - Architecture Choice

**Decision:** Use the pre-validation classifier design.

**Shape:** Add separate `ignore_dir_templates` and `ignore_file_templates`
slots. Add a helper that classifies files as ignored before normal validation.
`ft_list()` uses the classifier to prune early. `ft_index()` uses the same
classifier and skips all validation for ignored rows when
`include_ignored = TRUE`.

**Rejected alternatives:** Do not keep `ft_list()` unaware of ignores, because
callers need early pruning. Do not store ignore templates as flagged ordinary
templates, because ignored paths are outside the validation contract rather
than alternate valid schema shapes.

### 2026-07-01 - Implementation

**Change:** Added `ft_set_root()`, `ft_ignore_dir_template()`, and
`ft_ignore_file_template()`.

**Change:** Added `ignore_dir_templates` and `ignore_file_templates` slots to
`filetree` objects and wired them into template recompilation.

**Change:** Added pre-validation ignore classification. `ft_list()` prunes
ignored files unless `include_ignored = TRUE`; `ft_index()` prunes ignored files
by default and returns inert audit rows when `include_ignored = TRUE`.

**Change:** Schema tree and `format.filetree()` summaries now show ignored
templates. Roxygen docs, `README.Rmd`, generated `README.md`, and
`inst/overview.md` were updated.

**Testing:** New regression tests cover root updates, file ignores, directory
subtree ignores, `ft_list(include_ignored)`, inert audit rows, `when` / `with`
support, schema tree display, and formatted summaries.

### 2026-07-01 - Verification

**Formatter:** `air format .` could not run because `air` is not installed on
this machine.

**Whitespace:** `git diff --check` completed with no whitespace errors. Git
reported line-ending warnings for modified text files.

**Tests:** `devtools::test()` passed with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 167`.

**Package check:** `devtools::check(document = FALSE)` passed with
`0 errors | 0 warnings | 0 notes` when run with
`LC_ALL=English_United States.utf8`. Without that Windows-valid locale override,
R startup locale warnings caused the DESCRIPTION metadata check to fail before
package-specific checks.

### 2026-07-01 - Completed

**Status:** Done.

**Summary:** Ignore templates, ignored-row audit behavior, early pruning in
`ft_list()`, and `ft_set_root()` were implemented, documented, and verified.

### 2026-07-01 - Ignore Audit Columns

**Decision:** Use consistent dot-prefixed index metadata columns:
`.ignored`, `.ignore_template`, and `.ignore_type`.

**Rationale:** Dot-prefixed names match existing diagnostic columns like `.ok`
and `.problems` and avoid implying that ignore metadata is extracted from the
schema as user data.

### 2026-07-01 - Ignored Row Shape

**Decision:** With `include_ignored = TRUE`, ignored rows still get path-derived
columns: `.path`, `.rel`, `at_layer`, and `layer__*`.

**Decision:** Ignored rows do not get validation-derived extracted fields from
directory or file templates. They should have `.ignored = TRUE`, `.ok = TRUE`,
empty `.problems`, and populated `.ignore_template` / `.ignore_type`.

**Rationale:** Path-derived structure makes audit output useful while preserving
the rule that ignored files do not participate in validation.
