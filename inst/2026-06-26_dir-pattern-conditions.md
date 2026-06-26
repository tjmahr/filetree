---
status: done
---

# Directory Pattern Conditions

Track adding `when` and `with` support to `ft_add_dir_pattern()`.

## Work Items

- [x] Let directory patterns store per-pattern `when` conditions.
- [x] Let directory patterns store per-pattern `with` regex overrides.
- [x] Apply conditional directory patterns during `ft_index()`.
- [x] Use pattern-local regex pools when validating directory captures.
- [x] Show conditional directory pattern annotations in schema trees.
- [x] Refresh generated documentation after the API change.
- [x] Run local regression tests.

## Design Decisions

- Directory patterns now use the same `when` and `with` normalization helpers
  as file patterns.
- A later directory pattern with the same name replaces the earlier pattern on
  that layer. This preserves the existing directory-pattern overwrite behavior.
- Distinctly named directory patterns can coexist on the same layer, which is
  needed for conditional schemas.

## Decision Log

### 2026-06-26 - Request

**Request:** Add `when` and `with` arguments to `ft_add_dir_pattern()`, matching
the conditional and pattern-local override behavior already available for file
patterns.

**Rationale:** Large real-world trees may need different directory naming rules
under different parent layers, while still extracting the same logical fields.

### 2026-06-26 - Verification

**Verification:** `devtools::document()` regenerated
`man/ft_add_dir_pattern.Rd`.

**Verification:** `devtools::test()` passes with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 119`.
