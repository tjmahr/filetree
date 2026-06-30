---
status: done
---

# Relative Path Indexing

Track fixing `ft_index()` handling of relative file paths under `ft$root`.

## Work Items

- [x] Add a regression test for relative file paths under the configured root.
- [x] Verify the regression test fails with the original root-classification
  problem.
- [x] Normalize paths before root-membership checks in `.ft_path_rel()`.
- [x] Run local regression tests.

## Design Decisions

- Keep the fix internal to `.ft_path_rel()`. No public API change is needed.
- Preserve caller-provided `.path` values in `ft_index()` output.
- Compute membership and fallback relative paths from absolute-normalized
  `files` and `root` paths.
- Leave template literal semantics from critical review issue 1 out of
  scope for this fix.

## Decision Log

### 2026-06-30 - Request

**Request:** Fix critical review issue 2: `ft_index()` rejects relative paths
that are actually under `ft$root`.

**Root cause:** `.ft_path_rel()` compared raw `files` strings against absolute
`ft$root`, so relative inputs failed the prefix check even when `fs::path_rel()`
later computed the expected `.rel`.

### 2026-06-30 - Red Test

**Regression:** Added a test showing
`ft_index(ft, "test-trees/demo-1/ab-01/day01/ab-01_red.txt")` should produce
`.ok = TRUE` and `.rel = "ab-01/day01/ab-01_red.txt"` when `ft$root` is
`"test-trees/demo-1"`.

**Verification:** `devtools::test(filter = 'filetree')` failed on the new
regression with `.ok = FALSE` and
`file is at or above root; no matching layer`.

### 2026-06-30 - Fix

**Change:** Normalize both `files` and `root` with `fs::path_abs()` inside
`.ft_path_rel()` before prefix/equality checks and before the `fs::path_rel()`
fallback.

**Test adjustment:** Successful rows store `NULL` in `.problems`, so the
regression asserts zero length instead of `character()`.

**Verification:** `devtools::test()` passes with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 137`.
