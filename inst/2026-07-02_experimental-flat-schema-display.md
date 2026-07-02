---
status: done
---

# Experimental Flat Schema Display

Create an experimental alternative to `ft_schema_tree()` that uses a flat,
path-oriented layout and CLI styling. The current tree display loses too much
horizontal width on deeply nested schemas, especially when layers have many
conditional templates.

## Work Items

- [x] Add tests for the new flat schema output.
- [x] Add exported experimental formatter and printer functions.
- [x] Reuse existing template annotation semantics.
- [x] Use CLI styling consistent with `format.filetree()`.
- [x] Keep `ft_schema_tree()` unchanged.
- [x] Update generated documentation.
- [x] Run formatter and tests.

## Design Decisions

- Name the experimental functions `ft_format_schema_flat()` and
  `ft_schema_flat()`.
- Return one character element per output line from the formatter.
- Print one section per layer path, e.g. `. / sample / speaker / visit`.
- Emphasize the last layer in each path and style layer names consistently.
- Use `dirs:` and `files:` blocks inside each path section.
- Format template entries as `name = `template`` with only template strings in
  backticks.
- Show `<no templates>` when a layer has no directory, file, or ignored
  templates.
- Include ignored templates as `ignored dirs:` and `ignored files:` when present.

## Decision Log

### 2026-07-02 - Request

**Request:** Add an experimental CLI-styled alternative to `ft_schema_tree()`
that is easier to read for deep schemas with long template lists.

**Rationale:** A flat path-oriented view preserves horizontal width better than
tree indentation and lets directory/file templates be scanned in grouped
blocks.

### 2026-07-02 - Implementation

**Change:** Added `ft_format_schema_flat()` and `ft_schema_flat()` as exported
experimental functions. The formatter returns CLI-formatted lines; the printer
prints those lines and invisibly returns the input `filetree`.

**Display contract:** Each configured layer gets a path section such as
`. / subject / time / data`. Sections group `dirs:`, `files:`,
`ignored dirs:`, and `ignored files:` blocks. Template names are plain text,
template strings are backticked/styled, and `when`/`with` annotations reuse the
existing schema-tree wording.

**Documentation:** Updated roxygen documentation, regenerated `NAMESPACE` and
Rd files, and refreshed `inst/overview.md`.

**Verification:** Added red tests for path grouping, conditional and ignored
templates, and printer invisible return behavior. After implementation,
`devtools::document()` ran cleanly on the second pass, `air format .` ran, and
`devtools::test()` passed with `FAIL 0 | WARN 0 | SKIP 0 | PASS 224`.
