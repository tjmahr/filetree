---
status: done
---

# Large Corpus Roadblocks

Track practical issues found while using `filetree` on a 200,000-file corpus.

Durable package context, architecture, API, key files, and local test guidance
now live in `inst/overview.md`. This note has been streamlined to remove
troubleshooting noise and keep only session-specific decisions.

The theme for the next few sessions is fixing practical roadblocks found while
using `filetree` on a large corpus. The first roadblock was printing and
validating file patterns that belong to non-terminal layers, such as subject
manifest files that sit beside child directories.

The work was briefly sidetracked by a subgoal: making sure Codex can reliably
run the R package development workflow before its code edits are trusted.
Troubleshooting `.libPaths()`, PATH, and Codex terminal sandbox behavior led to
adopting `renv` with a repo-local package library.

## Work Items

- [x] Setup R package build tool chain for agent
- [x] Flag subject-level file pattern capture conflicts against parent directory values.
- [x] Show file patterns registered on non-terminal layers in `ft_schema_tree()`.
- [x] Add regression tests using the manifest/data-file scenario.

## Design Decisions

- README examples/output are user-owned. Do not edit or regenerate README files unless explicitly asked.
- Treat the Quarto `TMPDIR`/`sessioninfo` check issue as external and not a local blocker.

## Decision Log

### 2026-06-25 - First Roadblock

**Observation:** In `inst/demo-3/ab-02/aa-02-manifest.txt`, the manifest file matches a subject-layer file pattern, but the extracted `subject` value from the filename should conflict with the parent directory subject `ab-02`.

**Observation:** `ft_schema_tree()` omits the subject-layer manifest rule because it only prints final-layer file patterns.

### 2026-06-25 - Parent-Layer File Patterns

**Decision:** Let registered file patterns refine `at_layer` after directory patterns have extracted parent values. Prefer the existing depth-based layer when it matches, then fall back to the parent layer when that layer has registered file patterns.

**Rationale:** Files like `ab-02/aa-02-manifest.txt` are sidecar files for the `subject` layer, not child `time` entries. Classifying them by the registered subject-layer file pattern lets the existing capture-conflict logic report the wrong subject value.

**Decision:** Print file patterns registered on each directory layer before printing that layer's child directory branch in `ft_schema_tree()`.

**Rationale:** The schema tree should expose subject-level manifest files as part of the declared schema instead of hiding every non-terminal file pattern.

**Follow-up decision:** Print file patterns in the parent directory where files
for that layer live, using labels such as `` `time` file:`` and
`` `data` file:``.

**Rationale:** A flat printout made sidecar file patterns and child-layer file
patterns look like they occupied the same hierarchy level. The final format
keeps ownership clear without adding extra grouping rows: time-level manifests
are shown under the subject directory as `` `time` file:``, while terminal data
files are shown under the time directory as `` `data` file:``.

**Verification:** `Rscript -e "testthat::test_local()"` and
`Rscript -e "devtools::test()"` pass with
`FAIL 0 | WARN 0 | SKIP 0 | PASS 109`.

**Verification:** `R/filetree.R` and `tests/testthat/test-filetree.R` parse successfully. Full test execution initially could not run in this Codex environment because the sandbox could not see the user's R library.

### 2026-06-25 - Agent R Workflow and `renv`

**Environment note:** This session is running on the user's laptop, not the office PC used in the prior session. Avoid assuming machine-level R library paths, PATH behavior, or sandbox permissions are identical across those machines.

**Observation:** Sandboxed `Rscript` initially only saw the system R library, so packages from the user's global library appeared missing. This was a sandbox visibility issue, not a missing local R installation.

**Decision:** Use `renv` with a repo-local project library so Codex can run R package code inside the workspace sandbox without relying on the user's global library.

**Decision:** Force `renv` library and sandbox paths into the repo for this Codex workspace and disable the AppData-backed cache (`renv/settings.json`: `"use.cache": false`).

**Decision:** Keep the R package development toolchain in the repo-local `renv` library so Codex can run documentation and tests without the user's global library. The visible toolchain now includes `devtools`, `pkgbuild`, `pkgload`, `rcmdcheck`, `usethis`, and `roxygen2`.

**Documentation:** Updated roxygen text for parent-layer sidecar file patterns and refreshed `inst/overview.md` test guidance.

**Verification:** `Rscript -e "devtools::document()"` runs successfully inside the sandboxed repo-local `renv`.

**Verification:** `Rscript -e "testthat::test_local()"` passes.

**Verification:** `Rscript -e "devtools::test()"` passes.

**Remaining issue:** `Rscript -e "devtools::check(document = FALSE, manual = FALSE, error_on = 'never')"` now reaches `R CMD build` and starts checking, but stops because Rtools is not available on this laptop. `pkgbuild::check_build_tools(debug = TRUE)` tries to compile a simple C file and fails with `gcc: command not found`, while R points at `C:/rtools45/x86_64-w64-mingw32.static.posix/include`. This is now a system toolchain/PATH issue rather than an R package library or Codex sandbox issue.
