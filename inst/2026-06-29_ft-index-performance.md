---
status: done
---

# `ft_index()` Performance

Track first-pass speedups for indexing large file vectors.

## Work Items

- [x] Benchmark a synthetic 200,000-file corpus before changing internals.
- [x] Avoid row-wise file-layer resolution when no immediate parent layer has
  registered file patterns.
- [x] Use a fast relative-path path for files already under `ft$root`.
- [x] Simplify per-row path table construction.
- [x] Vectorize file-layer resolver prefiltering for parent-layer sidecar
  patterns.
- [x] Run local regression tests.
- [x] Run package check.

## Design Decisions

- Keep performance changes internal to `ft_index()` and helpers. No public API
  changes are needed for this pass.
- Preserve `fs::path_rel()` fallback behavior for paths that are not directly
  under `ft$root`.
- Treat parent-layer file-pattern resolution as necessary only when the
  immediate parent layer for a row has registered file patterns.

## Decision Log

### 2026-06-29 - First Pass

**Observation:** A 20,000-file synthetic corpus took about 29.6 seconds before
optimization. A 200,000-file run was too slow to keep waiting for interactively.

**Finding:** Profiling showed path normalization and splitting dominated early
runtime, mainly through `fs::path_rel()` and fs path handling.

**Change:** Added a fast relative-path helper for files already under
`ft$root`, falling back to `fs::path_rel()` otherwise.

**Change:** Added fast returns and vectorized prefiltering in
`.ft_resolve_file_layers()` so ordinary data files are not checked against
parent-layer sidecar patterns unless their immediate parent layer has file
patterns.

**Performance check:** On this laptop, a synthetic 200,000-row well-formed
data-file index runs in about 3-4 seconds. A similar 200,200-row sidecar
scenario also runs in about 4 seconds.

**Verification:** `devtools::test()` passes with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 128`.

**Verification:** `devtools::check(document = FALSE)` passes with
`0 errors | 0 warnings | 0 notes`.
