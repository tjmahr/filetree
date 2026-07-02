---
status: done
severity: major bug
---

# Do Not Hang Filenames From the Last Layer

`ft_index()` previously stored every basename in the raw column for the final
configured layer, even when the file template that classifies the file belonged
to an earlier layer. This made the final `layer__*` column look like a
tree-position column and a filename column at the same time.

The bug is visible when an extra layer is declared after the terminal data
files. With layers `subject / time / data / extra`, a data file such as
`ab-01/day01/ab-01_01_green.txt` is correctly classified as `at_layer = "data"`,
but its basename is stored in `layer__extra`. In the three-layer version of the
same schema, that basename appears in `layer__data`. The final column is
therefore being used as a basename sink rather than as the raw component for the
layer where the file matched.

## Key Files

- `inst/overview.md` -- durable package context and current indexing semantics.
- `R/filetree.R:.ft_path_table()` -- builds `.rel`, `at_layer`, and
  `layer__*` raw directory component columns plus `.filename`.
- `R/filetree.R:.ft_resolve_file_layers()` -- refines `at_layer` for files
  whose basename is owned by a nearby layer.
- `R/filetree.R:ft_index()` -- validates directory components, resolves file
  layer, validates filename templates, and orders output columns.
- `R/filetree.R:.ft_classify_ignored()` -- separately classifies ignored file
  templates and must follow the same filename-location semantics.
- `tests/testthat/test-filetree.R` -- existing regression coverage for sidecar
  files, ignored templates, and `layer__*` output.

## Work Items

- [x] Add a regression for a schema with an extra layer after the intended file
  template layer.
- [x] Decide the output contract for raw filename storage.
- [x] Update indexing internals so the basename is not always assigned to the
  final configured `layer__*` column.
- [x] Simplify away sidecar-specific indexing logic that becomes unnecessary
  under the shared `.filename` plus `at_layer` model.
- [x] Ensure sidecar files, terminal files, ignored files, strict mode, and
  problem rows all use the same rule.
- [x] Update `inst/overview.md` after the implementation is complete.
- [x] Run focused tests and the package test suite.

## Brainstorm

### Option A: Store Filenames in `layer__<at_layer>`

After file-layer resolution, move or write the basename into the raw
`layer__<at_layer>` column because the matching template belongs to that layer.
Under this model, `layer__data` contains `ab-01_01_green.txt` for a data file
even when the schema also declares `extra`; a subject manifest would have the
manifest basename in `layer__subject`.

This makes each file template's raw component easy to find from `at_layer`, but
it weakens the current meaning of `layer__*` as raw path components by depth.
For sidecar files, `layer__subject` would need to contain both the parent
directory `ab-01` and the basename `ab-01-manifest.txt`, which is impossible in
one scalar column without losing information. Keeping both values would require
another convention anyway, such as overwriting only empty layer columns or
adding parallel columns.

### Option B: Add a Dedicated Filename Column and Keep `at_layer`

Add a dedicated raw basename column, tentatively `.filename`, and keep
`at_layer` as the owner/classification of that basename. Then `layer__*`
columns remain raw directory components by position, while `.filename` is the
raw file component used for file-template matching. A data file with an extra
declared layer would have `layer__subject = "ab-01"`,
`layer__time = "day01"`, `layer__data = NA`, `layer__extra = NA`,
`.filename = "ab-01_01_green.txt"`, and `at_layer = "data"`.

This is the cleaner model because directory position and file ownership are
separate facts. It also generalizes sidecar and terminal files: both are just
filenames with an owning `at_layer`; the only difference is how many directory
components precede them. Existing file-template validation can read `.filename`
instead of `layer__<last layer>`, and candidate resolution can continue to use
directory depth plus registered templates.

The main cost is an output contract change. Users who currently look for the
basename in the final `layer__*` column would need to use `.filename`. Given the
current output is misleading for extra-layer schemas, that change is probably
worth making before the API hardens.

### Option C: Keep Final-Layer Basename but Add `at_layer`

Leave the basename in the final configured `layer__*` column and rely on
`at_layer` to explain which template matched. This minimizes output churn, but
it preserves the confusing behavior: adding an unused trailing layer changes
which raw column receives filenames, even though the file template and file
classification did not change.

This option does not fix the major bug; it only documents it.

## Recommendation

Use Option B: introduce a dedicated `.filename` column and treat `at_layer` as
the file component's owning layer. Keep `layer__<name>` columns for raw
directory components only. This produces a stable interpretation:

- `.filename` is the basename matched by file and ignored-file templates.
- `at_layer` says which layer owns that basename.
- `layer__*` columns describe directories encountered on the way to the file.
- Extracted fields such as `subject`, `time`, and `task` remain semantic values
  captured from directory or filename templates.

This avoids making sidecar files exceptional. A subject manifest and a terminal
data file follow the same rule: directory components fill `layer__*`,
`.filename` stores the basename, and `at_layer` identifies the layer whose file
templates validate the basename.

## Design Decisions

- Treat this as a major output-contract bug, not a display issue.
- Prefer preserving the depth-based meaning of `layer__*` over preserving the
  historical use of the last `layer__*` column as a basename sink.
- Avoid adding sidecar-only behavior. The corrected model should apply to all
  files.

## Decision Log

### 2026-07-02 - Request

**Request:** Refresh on `inst/overview.md`, create a working-on note, and
brainstorm how to avoid hanging the final part of a filename in the last layer
of the tree.

**Observation:** The current overview says layers include the terminal
file-name layer and `layer__<name>` columns are raw path components. The extra
layer example shows these two ideas conflict when the basename is always stored
in the final configured layer column.

**Recommendation:** Add `.filename` and use `at_layer` to associate it with the
matching file-template layer.

### 2026-07-02 - Option B Approved

**Decision:** Implement Option B. Add a dedicated `.filename` column for the raw
basename and use `at_layer` to determine which layer owns that basename.

**Rationale:** There is no need to preserve the previous output contract for
external users. This bug fix should simplify the internal model where possible,
especially any special sidecar-specific logic that can be replaced by the
general filename ownership model.

### 2026-07-02 - Implementation

**Change:** `.ft_path_table()` now stores the basename in `.filename` and fills
`layer__*` columns only from directory components. File-template validation,
ignored-file classification, and file-layer resolution now read `.filename`
instead of the final configured `layer__*` column.

**Simplification:** Removed the internal `file_layer` plumbing from
`ft_index()`. The matching path now follows one rule for both ordinary data
files and parent-layer files: `.filename` is the component being matched, and
`at_layer` is the owner layer.

**Documentation:** Updated `inst/overview.md`, roxygen comments, and generated
Rd files to describe `.filename` and the directory-only meaning of `layer__*`.

**Verification:** Added a regression for an extra declared layer after the data
file layer. Watched it fail before implementation because `layer__extra`
contained the basename and `.filename` was absent. After the fix,
`devtools::test()` passed with `FAIL 0 | WARN 0 | SKIP 0 | PASS 196`. After
`air format .`, `devtools::test()` passed again with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 196`.
