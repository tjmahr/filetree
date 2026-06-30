---
status: done
---

# Template API Rename and Semantics

Track the 2026-06-30 work that clarified the package distinction between field
regexes and directory/file templates, fixed critical review findings, and
renamed the public template-registration API.

## Work Items

- [x] Review package overview, README, and package code with a critical-review
  lens.
- [x] Create and maintain a working note for the relative-path indexing bug.
- [x] Fix relative file paths under `ft$root` in `.ft_path_rel()`.
- [x] Replace public-validator `stopifnot()` calls with argument-specific
  `rlang::abort()` errors.
- [x] Make directory and file templates literal full-string component matches.
- [x] Rename `ft_add_dir_pattern()` to `ft_add_dir_template()`.
- [x] Rename `ft_add_file_pattern()` to `ft_add_file_template()`.
- [x] Rename the public template argument from `templates` to `template`.
- [x] Rename internal storage, helpers, result columns, tests, generated docs,
  README examples, overview text, and historical notes to template terminology.
- [x] Regenerate roxygen documentation, `NAMESPACE`, and `README.md`.
- [x] Run formatter, tests, and package check.

## Design Decisions

- Use *field regexes* for reusable extraction definitions. These are registered
  with `ft_add_regex()` and become columns when captured.
- Use *templates* for expected directory or file component contents. These are
  built from fixed literal text plus `{placeholder}` references to field
  regexes.
- Directory and file templates are full-string component templates. A template
  must match the complete dirname or filename; `day{time}` must not match
  `day01b`.
- Fixed template text is literal, not regex syntax. For example,
  `{subject}_{task}.txt` treats `.txt` as a literal extension.
- Do not keep compatibility aliases for `ft_add_*_pattern()`. The package is
  still experimental and has one active user, so retaining old names would only
  preserve stale terminology.
- Rename broadly rather than only changing exported functions. The public API,
  object slots, helper names, result column, tests, README, overview, Rd files,
  and dated package notes should use the same vocabulary.
- Use singular `template` as the public argument name for
  `ft_add_dir_template()` and `ft_add_file_template()`. The function name
  already says the object being added is a template, and the singular argument
  reads better in named calls.
- Preserve legitimate non-package uses of the word `pattern`, such as the base R
  `grepl(pattern = ...)` argument, license text, and dependency metadata in
  `renv.lock`.

## Decision Log

### 2026-06-30 - Critical Review

**Review scope:** The package overview in `inst/`, `README.Rmd`/`README.md`,
and `R/filetree.R` were reviewed for correctness, API clarity, diagnostics, and
test coverage.

**Critical finding:** `.ft_path_rel()` compared raw `files` strings against an
absolute `ft$root`, so relative paths under the root were incorrectly classified
as outside or at the root.

**Required finding:** Public validation relied on `stopifnot()`, which produced
poor user-facing errors for malformed inputs.

**Design finding:** The word "pattern" conflated two concepts: reusable regexes
that define extracted fields, and layer-level expected component shapes.

### 2026-06-30 - Relative Path Fix

**Change:** Normalize both `files` and `root` with `fs::path_abs()` inside
`.ft_path_rel()` before prefix/equality checks and before the `fs::path_rel()`
fallback.

**Decision:** Preserve caller-provided `.path` values in the returned index.
Only root-membership and relative-path calculations use normalized absolute
paths.

**Tracking:** Detailed regression notes are in
`inst/2026-06-30_relative-path-indexing.md`.

### 2026-06-30 - Validator Cleanup

**Change:** Added internal validation helpers and replaced `stopifnot()` in
package code with explicit `rlang::abort()` failures naming the bad argument.

**Rationale:** Public API validation should produce actionable, stable errors
instead of generic assertion failures.

### 2026-06-30 - Template Semantics

**Decision:** Directory and file template strings describe complete path
components, not arbitrary regex fragments.

**Change:** Added template tokenization and compilation helpers so `{field}`
tokens are expanded from the regex pool while all fixed text is escaped
literally. Compiled templates are anchored with `^` and `$`.

**Regression:** Added coverage that fixed text is literal and that full-string
matching rejects suffixes, including the `day{time}` vs. `day01b` case.

### 2026-06-30 - Public API Rename

**Decision:** Rename both public registration functions:

- `ft_add_dir_pattern()` -> `ft_add_dir_template()`
- `ft_add_file_pattern()` -> `ft_add_file_template()`

**Rationale:** "Template" is the clearest label for an expected dirname or
filename shape assembled from fixed text plus field regex placeholders. Keeping
"pattern" for these functions would continue to blur templates with regexes.

**Compatibility:** No aliases were kept. The package is still pre-release and
the user confirmed there are no other users to preserve compatibility for.

**Generated files:** `devtools::document()` rewrote `NAMESPACE`, created
`man/ft_add_dir_template.Rd` and `man/ft_add_file_template.Rd`, and removed the
old `ft_add_*_pattern` help files. `devtools::build_readme()` refreshed
`README.md`.

### 2026-06-30 - Repository-Wide Terminology Sweep

**Change:** Renamed internal slots and helpers such as `dir_templates`,
`file_templates`, `.ft_compile_template()`, and
`.ft_recompile_templates()`.

**Change:** Renamed the index result column from `pattern` to `template`.

**Change:** Updated tests, README text, `inst/overview.md`, generated Rd files,
and historical `inst/2026-*` notes to use template terminology.

**Boundary:** The remaining repo-wide uses of "pattern" are unrelated to this
API vocabulary: license prose, dependency metadata in `renv.lock`, and a base R
argument name in `grepl(pattern = ...)`.

### 2026-06-30 - Singular Template Argument

**Request:** Change named calls from `templates =` to `template =`, e.g.
`ft_add_dir_template(layer = "time", template = "{time}")`.

**Decision:** Rename the public formal argument for both
`ft_add_dir_template()` and `ft_add_file_template()` to `template`. Internal
storage and helper variables may stay plural where they represent a vector or
collection of registered templates.

**Compatibility:** No `templates` alias was kept, matching the rest of the
pre-release API cleanup.

### 2026-06-30 - Verification

**Formatting:** `air format .` completed successfully.

**Tests:** `devtools::test()` passed with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 135`.

**Package check:** `devtools::check(document = FALSE)` passed with
`0 errors | 0 warnings | 0 notes`.
