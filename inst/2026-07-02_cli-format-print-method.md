---
status: done
---

# Use CLI Formatting for Filetree Print Methods

Refactor `format.filetree()` and `print.filetree()` to use `cli` formatting
tools instead of manually assembling plain strings. The goal is a semantic,
theme-aware summary that still returns a character vector from `format()` and
prints through `print()`.

The `cli::cli_format_method()` reference recommends using
`cli_format_method({ ... })` inside an S3 `format()` method, then defining
`print()` as `cat(format(x, ...), sep = "\n")`. That matches the current
package structure, where `print.filetree()` already delegates to
`format.filetree()`.

Do not use cli's `.file` inline class for `ft$root` or other paths because
`.file` creates terminal links. Prefer plain values, `.path` if it does not
link in the target contexts, or unstyled text if needed.

## Key Files

- `R/filetree.R:format.filetree()` -- currently builds a character vector by
  hand with `sprintf()` and `paste()`.
- `R/filetree.R:print.filetree()` -- already delegates to `format()` and can
  probably stay structurally unchanged.
- `tests/testthat/test-filetree.R` -- add or update tests for printed and
  formatted output.
- `man/format.filetree.Rd` and `man/print.filetree.Rd` -- regenerate if roxygen
  changes.
- `inst/overview.md` -- update only if the display contract changes enough to
  matter for future agents.

## Work Items

- [x] Add focused output tests for `format.filetree()` and/or
  `print.filetree()` before changing implementation.
- [x] Convert `format.filetree()` to `cli::cli_format_method({ ... })`.
- [x] Keep `print.filetree()` delegated to `cat(format(x, ...), sep = "\n")`
  unless implementation reveals a reason to change it.
- [x] Use semantic inline markup for package concepts: `.cls` or plain text for
  `<filetree>`, `.field`/`.var` for layer and regex names, `.val` for template
  strings and counts, and `.code` for literal API terms.
- [x] Avoid `.file` for paths because those become links.
- [x] Extract small helpers only where they remove real duplication in the four
  template sections.
- [x] Run `air format .`, `devtools::document()` if roxygen changes, and
  `devtools::test()`.

## Formatting Direction

Use `cli_format_method()` to keep `format.filetree()` as the canonical text
producer. A likely shape:

- Header: compact object identity such as `<filetree>`.
- Root: show the root as plain text or a non-link path style.
- Layers: show the ordered layer path as `subject / time / data`.
- Regex pool: show count and names, with empty state clearly marked.
- Directory templates: list every directory layer, including explicit `<none>`
  entries where the current output does.
- File templates: list only layers with registered file templates, preserving
  `at_layer=<layer>` wording if that remains the clearest label.
- Ignored directory and file templates: mirror the regular template sections.

Prefer the package's current information layout over a decorative redesign. The
main improvement should be semantic formatting and less brittle manual string
assembly, not a different object summary contract.

## Design Decisions

- Keep `format.filetree()` returning one character element per output line, as
  `cli_format_method()` does.
- Preserve `print.filetree()` as an invisible-returning wrapper around
  `format.filetree()`.
- Avoid `.file`; root paths should not become clickable links.
- Treat this as a refactor with user-visible formatting changes, so tests should
  verify stable content without becoming overly sensitive to cli theme details.

## Decision Log

### 2026-07-02 - Request

**Request:** Review the package's CLI skill and the `cli_format_method()`
reference, then create a working task for making the print method use CLI
formatting.

**Observation:** The package already imports `cli` and uses cli inline markup
in diagnostics. The print/format path is the remaining plain-string display
surface.

**Decision:** Use `cli::cli_format_method()` inside `format.filetree()` and
keep `print.filetree()` delegated to `cat(format(...), sep = "\n")`.

### 2026-07-02 - Implementation

**Change:** `format.filetree()` now builds output with
`cli::cli_format_method({ ... })` and local helpers for repeated template
sections. `print.filetree()` remains a thin `cat(format(...), sep = "\n")`
wrapper.

**Output contract:** `format.filetree()` now returns one character element per
printed line instead of a single newline-collapsed string.

**Path styling:** Root paths are interpolated as plain text. The implementation
does not use cli's `.file` inline class.

**Verification:** Added a red test for the one-element-per-line contract. It
failed before implementation because `format.filetree()` returned one string
with embedded newlines. After the refactor, `devtools::test()` passed with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 199`. After `air format .`, `devtools::test()`
passed again with `FAIL 0 | WARN 0 | SKIP 0 | PASS 199`.

### 2026-07-02 - Print and Template Styling Regression

**Observation:** `format.filetree()` returned multiple elements, but
`print.filetree()` still used `cat(format(x), "\n")`, so `cat()` collapsed the
elements with spaces and printed the summary on one line.

**Observation:** Template summary strings were interpolated as plain text, so
the file and directory template values did not get cli value styling.

**Change:** `print.filetree()` now calls
`cat(format(x, ..., width = width), sep = "\n")`. Template summaries are now
emitted with `{.val ...}` in the cli list items, while layer labels use
`{.field ...}`.

**Verification:** Added focused tests for printed multiline output and
template-line ANSI styling. Watched both fail before the fix. After the fix,
`devtools::test()` passed with `FAIL 0 | WARN 0 | SKIP 0 | PASS 201`. After
`air format .`, `devtools::test()` passed again with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 201`.

### 2026-07-02 - Display Contract Refinement

**Request:** Remove the old `file_layer` line. Format layer names consistently
in the `layers` line and template rows. Format directory and file template rows
the same way. Keep template names such as `default =` and `default_2 =`
unstyled, and put only the template strings in backticks.

**Rationale:** `file_layer` reflects the old terminal-layer mental model. The
template rows should be readable as schema declarations, not as quoted whole
key-value strings.

**Change:** Removed the `file_layer` line from `format.filetree()`. Directory
and file template rows now share the same form:
`layer: name = \`template\``. Repeated templates are joined as
`default = \`...\`, default_2 = \`...\``. Layer names in the `layers:` line and
template rows use the same `.field` styling. Root paths are coerced to plain
character strings so they are not quoted and do not use `.file` links.

**Verification:** Added failing tests for the refined display contract, watched
them fail against the prior output, then updated the formatter. After the fix,
`devtools::test()` passed with `FAIL 0 | WARN 0 | SKIP 0 | PASS 209`. After
`air format .`, `devtools::test()` passed again with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 209`.
