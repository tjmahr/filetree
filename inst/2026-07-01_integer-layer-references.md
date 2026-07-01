---
status: done
---

# Integer Layer References

Track design and implementation work for allowing positive integer layer
references in APIs that currently require layer names.

## Context

`filetree` currently uses named layers from `ft_init(root, layers)` as the
public layer model. Directory templates are registered on non-terminal layer
names; file templates and ignored file templates may be registered on any
configured layer. Ignored directory templates prune subtrees by matching
directory components at a configured layer.

The named-layer model can become awkward when the tree has structurally similar
data at different depths or under excluded/intermediate folders:

```text
subject1/
  ...
subject2/
  ...
excluded/
  subject4/
    ...
  subject5/
    ...
```

In this shape, users may want to refer to path depth directly rather than invent
semantic layer names for every depth.

## Goal

Allow integer layer references as an alternative to layer names. Layer `0` is
the implicit root layer and is inaccessible for ordinary layer-template
registration. Layer `1` is the first true configured layer, equivalent to
`ft$layers[[1]]`.

## Key Files

- `R/filetree.R` - public template APIs, layer validation, candidate layer
  resolution, path indexing, and display.
- `tests/testthat/test-filetree.R` - package regression coverage.
- `inst/overview.md` - durable architecture and indexing semantics.
- `README.Rmd` - user-facing examples after API design is approved.
- `NAMESPACE` and `man/*.Rd` - generated from roxygen after API changes.

## Work Items

- [x] Clarify which public APIs accept integer layer references.
- [x] Clarify whether layer `0` is accepted anywhere or only documented as
  inaccessible.
- [x] Decide validation/error messages for out-of-range, non-integer, and mixed
  layer references.
- [x] Decide how integer references interact with schema display and audit
  columns.
- [x] Implement the approved API with tests.
- [x] Update roxygen docs, `README.Rmd`, generated docs, and `inst/overview.md`.
- [x] Run formatter and package verification.

## Design Questions

- Resolved: integers should be accepted anywhere a public `layer` argument
  exists:
  `ft_add_dir_template()`, `ft_add_file_template()`,
  `ft_ignore_dir_template()`, and `ft_ignore_file_template()`?
- Should helper/internal functions also accept integers, or should integers be
  normalized at public API boundaries only?
- Resolved: `0` should be rejected for all existing template APIs with a
  root-specific error.
- Resolved: integer layer values must be length-one whole-number numerics.
  Accept `1L` and `1`; reject `1.5`, `NA`, `Inf`, `c(1, 2)`, and `"1"`.
- Should error messages report both accepted names and accepted integer ranges?

## Initial Design Notes

- Prefer normalizing integer layer references to canonical layer names at the
  public API boundary. This keeps the internal object structure and result
  columns name-based.
- Treat layer `0` as reserved for the implicit root. It should probably produce
  a clear error in existing template APIs rather than silently mapping to
  anything.
- Integer references should preserve existing output: stored templates,
  `at_layer`, schema tree labels, and diagnostics should continue using layer
  names.
- A small helper such as `.ft_resolve_layer()` could centralize validation and
  error messages across the four template APIs.
- Do not coerce character numerals such as `"1"` to integer layer references.
  Character layers remain name references only.

## Approved Design

- Normalize integer layer references at the public API boundary.
- Keep internals, object storage, schema display, diagnostics, and index output
  layer-name based.
- Accept integer layer references in `ft_add_dir_template()`,
  `ft_add_file_template()`, `ft_ignore_dir_template()`, and
  `ft_ignore_file_template()`.
- Reject `layer = 0` in those APIs with a root-layer-specific error.
- For directory-template APIs, valid integer range is `1` through
  `length(ft$layers) - 1`.
- For file-template APIs, valid integer range is `1` through
  `length(ft$layers)`.
- Character `"1"` remains a layer-name lookup, not an integer reference.

## Implementation Plan

1. Add failing tests in `tests/testthat/test-filetree.R`:
   - integer references work for ordinary directory and file templates;
   - integer references work for ignored directory and file templates;
   - integer references store/display canonical layer names;
   - `layer = 0` errors clearly;
   - out-of-range, fractional, missing, infinite, vector, and character numeral
     inputs error correctly.
2. Verify the new tests fail against the current implementation.
3. Implement a shared resolver in `R/filetree.R`, likely
   `.ft_resolve_layer(ft, layer, allowed, layer_kind)`.
4. Replace per-function layer validation in the four template APIs with the
   resolver.
5. Update roxygen docs for the four `layer` arguments.
6. Update `README.Rmd` and `inst/overview.md`.
7. Regenerate docs and run verification.

## Decision Log

### 2026-07-01 - Request

**Request:** Allow positive integers to specify layers instead of layer names.
Layer `0` is the implicit inaccessible root layer; layer `1` is the first true
configured layer.

**Motivation:** Integer layer references help when the semantic layer model
breaks down, such as when ordinary subject folders live at root but excluded
branches contain subject folders one level deeper.

**Context reviewed:** `inst/overview.md`, current public API shape, and recent
ignore-template work.

### 2026-07-01 - Public API Scope

**Decision:** Accept integer layer references in every public API that has a
`layer` argument:

- `ft_add_dir_template()`
- `ft_add_file_template()`
- `ft_ignore_dir_template()`
- `ft_ignore_file_template()`

**Rationale:** Keeping the behavior consistent across the template APIs makes
integer references predictable. Internals and output should still use canonical
layer names.

### 2026-07-01 - Root Layer Zero

**Decision:** Layer `0` is documented as the implicit root layer, but it is not
accepted by the existing template APIs.

**Rationale:** The current APIs register directory and file templates on
configured layers. Accepting `0` would imply root-level template semantics that
have not been designed. A clear error preserves room for a future explicit
root-file or root-directory API.

### 2026-07-01 - Integer Validation

**Decision:** Accept only length-one whole-number numeric layer references.
`1L` and `1` are valid. Reject fractional values, missing values, infinite
values, vectors longer than one, and character numerals.

**Rationale:** Avoiding character-to-number coercion keeps layer names and layer
positions distinct and makes invalid inputs easier to diagnose.

### 2026-07-01 - Design Approved

**Decision:** Use public-boundary normalization. Integers are accepted in the
four public template APIs, converted immediately to canonical layer names, and
never stored as integers.

### 2026-07-01 - Implementation

**Change:** Added shared layer-reference resolution in `R/filetree.R`.

**Change:** Updated `ft_add_dir_template()`, `ft_add_file_template()`,
`ft_ignore_dir_template()`, and `ft_ignore_file_template()` to accept integer
layer positions and normalize them to canonical layer names.

**Testing:** Added regression tests for successful integer references across
all four public template APIs, canonical name storage/display, root layer `0`
errors, out-of-range positions, invalid numeric inputs, and character numerals.

### 2026-07-01 - Verification

**Formatter:** `air format .` could not run because `air` is not installed on
this machine.

**Whitespace:** `git diff --check` completed with no whitespace errors. Git
reported line-ending warnings for modified text files.

**Tests:** `devtools::test()` passed with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 183`.

**Package check:** `devtools::check(document = FALSE)` passed with
`0 errors | 0 warnings | 0 notes` when run with
`LC_ALL=English_United States.utf8`.

### 2026-07-01 - Completed

**Status:** Done.

**Summary:** Public template APIs now accept positive integer layer references,
normalize them to canonical layer names, reject root layer `0`, and preserve
name-based internals and output.
