---
status: done
---

# Invalid UTF-8 in Filetree Summaries

Track the fix for an invalid UTF-8 error when printing a filetree with long
template summaries on Windows.

## Context

A real TD schema could be constructed successfully, but printing the resulting
object failed with:

```text
Error in sub(re, "", x, perl = TRUE) : input string 1 is invalid UTF-8
```

The schema registered enough named templates for a formatted template summary
to exceed the 90-character truncation threshold in `format.filetree()`.

## Root Cause

Template compilation was not responsible for the error. The CLI-based
`format.filetree()` delegates repeated template formatting to its local
`template_summary()` helper. When a summary was too long, that helper truncated
it and appended a Unicode ellipsis using `"\u2026"`.

On a non-UTF-8 Windows locale, the ellipsis could be converted to native byte
`0x85`. Console formatting code that removes ANSI sequences then passed the
string to a Perl-compatible regular expression as UTF-8, which rejected the
invalid byte.

## Decision

Use the ASCII truncation marker `...` in `template_summary()`. This keeps all
directory, file, ignored-directory, and ignored-file template summaries
portable because they share the same helper. It also keeps the intended
90-character limit: 87 retained characters plus three periods.

## Key Files

- `R/filetree.R` - uses an ASCII truncation marker in the shared
  `format.filetree()` template-summary helper.
- `tests/testthat/test-filetree.R` - verifies that the complete vector of
  CLI-formatted lines uses portable bytes and contains `...` when a template is
  truncated.

## Work Items

- [x] Read `inst/overview.md` and reproduce the reported schema failure path.
- [x] Separate template construction from object display.
- [x] Trace the invalid byte to the Unicode truncation marker.
- [x] Update the local branch from `origin/main` before implementing the fix.
- [x] Add a failing, vector-aware regression test for the CLI formatter.
- [x] Replace the shared Unicode truncation marker with `...`.
- [x] Run focused and full package verification.

## Verification

- The corrected regression test failed before the production change because
  the formatted output contained U+2026 and no `...` marker.
- The focused regression passed with `FAIL 0 | WARN 0 | SKIP 0 | PASS 2` after
  the fix.
- `devtools::test()` passed with
  `FAIL 0 | WARN 0 | SKIP 0 | PASS 246` after the fix.
- `air format .` could not run because `air` is not installed on this machine.
