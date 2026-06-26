---
status: done
---

# Problem Glimpse Batching

Track grouping `ft_glimpse_problems()` output by parent directory and layer.

## Work Items

- [x] Group problem output by `at_layer` and parent directory.
- [x] Let `n` control the number of problem batches previewed.
- [x] Add `n_lines` to control the number of problem lines per batch.
- [x] Print all remaining items when the hidden remainder is less than 20%.
- [x] Shorten problem lines by printing file names relative to the batch
  directory and removing repeated filenames from filename-match messages.
- [x] Refresh generated documentation.
- [x] Run local regression tests.

## Design Decisions

- A problem batch is the combination of a problem row's `at_layer` and parent
  directory from `.rel`.
- Problem lines include the relative file path before the problem message.
- Within a batch, problem lines use the path relative to the batch directory.
- Printed filename pattern-miss messages omit the quoted filename because the
  line prefix already identifies the file.
- `ft_glimpse_problems()` still returns all problem rows invisibly; `n` and
  `n_lines` affect printed output only.

## Decision Log

### 2026-06-26 - Request

**Request:** Avoid printing one full block per file when many files in the same
folder have problems. Group by parent directory and layer, print a head-like
preview, and summarize hidden problems with `[..., n more problems]`.

### 2026-06-26 - Verification

**Verification:** `devtools::document()` regenerated
`man/ft_glimpse_problems.Rd`.

**Verification:** `devtools::test()` passes with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 125`.

**Verification:** `devtools::check(document = FALSE)` passes with
`0 errors | 0 warnings | 0 notes`.
