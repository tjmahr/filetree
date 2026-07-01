---
status: done
---

# Template Replacement Warning

Track the small follow-up to make template-name replacement visible to users.

## Context

Directory templates are keyed by template name within a layer. Unnamed
templates are normalized to `default`, so registering two unnamed directory
templates on the same layer silently replaced the first with the second. This
made a mixed tree design hard to debug because a later conditional template
could accidentally replace the ordinary template for that layer.

## Goal

Warn when registering a template name that already exists at the same layer,
while preserving the current replacement behavior.

## Decisions

- Keep replacement behavior unchanged: the newest template wins.
- Warn at the shared template-registration helper where replacement is decided.
- Cover both explicit duplicate names and the unnamed `default` collision.
- Document that directory template registration is name-keyed and that unique
  template names are needed for alternatives on the same layer.

## Key Files

- `R/filetree.R` - warning in `.ft_add_template_specs()`.
- `tests/testthat/test-filetree.R` - duplicate-name and unnamed-template
  regression tests.
- `inst/overview.md` - durable design note.

## Work Items

- [x] Add regression coverage for explicit duplicate directory template names.
- [x] Add regression coverage for unnamed `default` directory template
  replacement.
- [x] Add a warning before replacing an existing template.
- [x] Update the overview document.
- [x] Run package verification.

## Verification

- `devtools::test()` passed with `FAIL 0 | WARN 0 | SKIP 0 | PASS 186`.
- `git diff --check` was clean aside from Git LF-to-CRLF notices.
- `devtools::check(document = FALSE)` passed with
  `0 errors | 0 warnings | 0 notes`.
