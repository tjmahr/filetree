# filetree prototype (layers + global regex pool + patterns with {name} expansion)
# layers = what you see when you `ls` at each step, including the final file-name layer
#
# Key simplification: NO "group" concept. Patterns are just named patterns.
#
# deps: fs, stringr, tibble, dplyr, rlang


# ---- constructors ----

#' Create a filetree specification
#'
#' Build a `filetree` object that records the root directory, ordered layers,
#' and slots for regex templates and patterns.
#'
#' @param root Path to the root directory used as the base for indexing.
#' @param layers Character vector naming each path layer; the last element
#'   represents the file-name layer.
#' @return A `filetree` object describing the tree layout.
#' @export
ft_init <- function(root, layers) {
  stopifnot(is.character(layers), length(layers) >= 1, all(nzchar(layers)))
  stopifnot(length(unique(layers)) == length(layers))

  dir_layers <- if (length(layers) >= 2) layers[-length(layers)] else character()

  structure(
    list(
      root = fs::path_abs(root),
      layers = layers,                 # includes final file-name layer
      regex_pool = rlang::set_names(list(), character()),
      dir_patterns = rlang::set_names(vector("list", length(dir_layers)), dir_layers),
      file_patterns = rlang::set_names(vector("list", length(layers)), layers) # patterns at any layer
    ),
    class = "filetree"
  )
}
#' Register regex templates used by patterns
#'
#' Add named regular expressions to the pool that can be referenced by
#' placeholders such as `{subject}` inside directory or file patterns.
#'
#' @param ft A `filetree` object.
#' @param regexes Named character vector of regular expressions to store.
#' @return The updated `filetree` object.
#' @export
ft_add_regex <- function(ft, regexes) {
  stopifnot(inherits(ft, "filetree"), is.character(regexes), !is.null(names(regexes)))
  for (nm in names(regexes)) {
    rx <- regexes[[nm]]
    stopifnot(length(rx) == 1, nzchar(rx))
    ft$regex_pool[[nm]] <- rx
  }
  ft
}

# ---- pattern compilation helpers ----

.ft_placeholders <- function(x) {
  m <- stringr::str_match_all(x, "\\{([A-Za-z][A-Za-z0-9_]*)\\}")[[1]]
  if (nrow(m) == 0) character() else unique(m[, 2])
}

.ft_compile_pattern <- function(pattern, regex_pool) {
  ph <- .ft_placeholders(pattern)
  missing <- setdiff(ph, names(regex_pool))
  if (length(missing)) {
    stop("Pattern references unknown regex name(s): ", paste(missing, collapse = ", "),
         "\n  pattern: ", pattern)
  }

  compiled <- pattern
  for (nm in ph) {
    rx <- regex_pool[[nm]]
    compiled <- stringr::str_replace_all(
      compiled,
      stringr::fixed(paste0("{", nm, "}")),
      paste0("(?<", nm, ">(?:", rx, "))")
    )
  }
  paste0("^", compiled, "$")
}

.ft_normalize_patterns <- function(patterns) {
  stopifnot(is.character(patterns))
  if (length(patterns) == 1 && is.null(names(patterns))) names(patterns) <- "default"
  stopifnot(!is.null(names(patterns)))
  patterns
}

# ---- directory patterns ----
# patterns validate (and may extract from) the directory name at `layer`

#' Register directory name patterns for a layer
#'
#' Compile named patterns that validate and extract captures from directory
#' names at a given layer in the tree.
#'
#' @param ft A `filetree` object.
#' @param layer Directory layer name (must be one of the non-file layers).
#' @param patterns Named character vector of patterns using `{placeholder}`
#'   references that point into `ft`'s `regex_pool`.
#' @return The updated `filetree` object.
#' @export
ft_add_dir_pattern <- function(ft, layer, patterns) {
  stopifnot(inherits(ft, "filetree"))
  stopifnot(is.character(layer), length(layer) == 1, layer %in% names(ft$dir_patterns))

  patterns <- .ft_normalize_patterns(patterns)
  compiled <- lapply(patterns, .ft_compile_pattern, regex_pool = ft$regex_pool)

  ft$dir_patterns[[layer]] <- list(raw = patterns, compiled = compiled)
  ft
}

# ---- file patterns ----
# patterns validate (and may extract from) the file name at `at_layer`

#' Register file-name patterns for a layer
#'
#' Compile named patterns that validate and extract captures from file names
#' for files that belong to a specific layer.
#'
#' @param ft A `filetree` object.
#' @param layer Layer at which the files live (must be listed in `ft$layers`).
#' @param patterns Named character vector of file-name patterns that may use
#'   `{placeholder}` references tied to `ft`'s regex pool.
#' @return The updated `filetree` object.
#' @export
ft_add_file_pattern <- function(ft, layer, patterns) {
  stopifnot(inherits(ft, "filetree"))
  stopifnot(is.character(layer), length(layer) == 1, layer %in% ft$layers)

  patterns <- .ft_normalize_patterns(patterns)
  compiled <- lapply(patterns, .ft_compile_pattern, regex_pool = ft$regex_pool)

  # overwrite patterns at this layer (easy/explicit). If you want additive behavior later,
  # we can add ft_add_pattern_append().
  ft$file_patterns[[layer]] <- list(raw = patterns, compiled = compiled)
  ft
}

# ---- file enumeration ----

#' List files under the filetree root
#'
#' Return all files under the filetree root using the configured `fs` helper.
#'
#' @param ft A `filetree` object.
#' @return Character vector of file paths relative to the working directory.
#' @export
ft_list <- function(ft) {
  stopifnot(inherits(ft, "filetree"))
  fs::dir_ls(ft$root, recurse = TRUE, type = "file")
}

# ---- indexing / parsing / validation ----

#' Map path components to a layer name
#'
#' Determine which configured layer a path belongs to given its components.
#'
#' @param ft A `filetree` object.
#' @param parts Character vector of path components including the file name.
#' @return The layer name, `".__too_deep__"` if the path exceeds known layers,
#'   or `NA_character_` when the path cannot be matched.
#' @keywords internal
#' @export
.ft_at_layer_from_parts <- function(ft, parts) {
  # parts includes filename at end
  n_dir <- max(length(parts) - 1L, 0L)
  idx <- n_dir + 1L
  if (idx <= 0L) return(NA_character_)
  if (idx > length(ft$layers)) return(".__too_deep__")
  ft$layers[[idx]]
}


.ft_all_placeholder_names <- function(ft) {
  out <- character()

  # dir patterns
  for (layer in names(ft$dir_patterns)) {
    spec <- ft$dir_patterns[[layer]]
    if (is.null(spec) || length(spec) == 0) next
    out <- c(out, unlist(lapply(spec$raw, .ft_placeholders), use.names = FALSE))
  }

  # file patterns
  for (at_layer in names(ft$file_patterns)) {
    spec <- ft$file_patterns[[at_layer]]
    if (is.null(spec) || length(spec) == 0) next
    out <- c(out, unlist(lapply(spec$raw, .ft_placeholders), use.names = FALSE))
  }

  unique(out)
}

#' Index files against a filetree specification
#'
#' Validate file paths against the configured directory and file-name patterns,
#' extract placeholder captures, and report any problems.
#'
#' @param ft A `filetree` object.
#' @param files Optional character vector of file paths to check. Defaults to
#'   all files under `ft$root` via [ft_list()].
#' @return A tibble with layer columns (`layer__<name>`), captured placeholders,
#'   the matched pattern name, `.ok` flag, and `.problems` list-column.
#' @export
ft_index <- function(ft, files = ft_list(ft)) {
  stopifnot(inherits(ft, "filetree"))

  rel <- fs::path_rel(files, start = ft$root)
  parts_list <- strsplit(rel, .Platform$file.sep, fixed = TRUE)

  layers <- ft$layers
  dir_layers <- if (length(layers) >= 2) layers[-length(layers)] else character()
  file_layer <- layers[length(layers)]

  layer_cols <- paste0("layer__", layers)

  # build one column per layer name (raw component), including final file-name layer
  layer_mat <- matrix(NA_character_, nrow = length(parts_list), ncol = length(layers))
  colnames(layer_mat) <- layer_cols

  at_layer <- character(length(parts_list))

  for (i in seq_along(parts_list)) {
    parts <- parts_list[[i]]
    fname <- utils::tail(parts, 1)

    n_dir <- max(length(parts) - 1L, 0L)
    if (n_dir > 0L && length(dir_layers) > 0L) {
      n_fill <- min(n_dir, length(dir_layers))
      layer_mat[i, seq_len(n_fill)] <- parts[seq_len(n_fill)]
    }
    layer_mat[i, length(layers)] <- fname

    at_layer[[i]] <- .ft_at_layer_from_parts(ft, parts)
  }

  tbl <- tibble::tibble(
    .path = files,
    .rel = rel,
    at_layer = at_layer
  ) |>
    dplyr::bind_cols(tibble::as_tibble(layer_mat))

  # extracted fields = placeholders not in layers (but captures may include layer names too;
  # those should become extracted fields columns, not collide with layer__ columns)
  all_placeholders <- .ft_all_placeholder_names(ft)

  # create columns for ALL placeholders (speaker/visit/task/item/etc.)
  # even if a placeholder name equals a layer name, it is still an extracted field column,
  # because the raw layer component is stored in layer__<layer>.
  if (length(all_placeholders)) {
    for (nm in all_placeholders) {
      if (is.null(tbl[[nm]])) tbl[[nm]] <- NA_character_
    }
  }

  n <- nrow(tbl)
  matched_pattern <- rep(NA_character_, n)
  problems <- vector("list", n)

  # helper: write captures with conflict checking against existing extracted field
  set_capture_vec <- function(tbl, idx, cn, values, msgs) {
    if (!any(idx)) return(list(tbl = tbl, msgs = msgs))
    values <- as.character(unname(values))
    existing <- tbl[[cn]]

    conflicts <- idx & !is.na(existing) & !is.na(values) & existing != values
    if (any(conflicts)) {
      for (j in which(conflicts)) {
        msgs[[j]] <- c(
          msgs[[j]],
          sprintf("capture %s='%s' conflicts with %s='%s'", cn, values[[j]], cn, existing[[j]])
        )
      }
    }

    replace_idx <- idx & (is.na(existing) | existing == values)
    if (any(replace_idx)) {
      tbl[[cn]][replace_idx] <- values[replace_idx]
    }

    rx <- ft$regex_pool[[cn]]
    if (!is.null(rx)) {
      bad_rx <- idx & !is.na(tbl[[cn]]) & !stringr::str_detect(tbl[[cn]], paste0("^", rx, "$"))
      if (any(bad_rx)) {
        for (j in which(bad_rx)) {
          msgs[[j]] <- c(
            msgs[[j]],
            sprintf("capture %s='%s' fails /%s/", cn, tbl[[cn]][[j]], rx)
          )
        }
      }
    }

    list(tbl = tbl, msgs = msgs)
  }

  # pre-flag structural problems
  too_deep <- tbl$at_layer == ".__too_deep__"
  bad_root <- is.na(tbl$at_layer)
  if (any(too_deep)) {
    for (j in which(too_deep)) {
      problems[[j]] <- c(problems[[j]], sprintf("path deeper than layers (%d)", length(layers)))
    }
  }
  if (any(bad_root)) {
    for (j in which(bad_root)) {
      problems[[j]] <- c(problems[[j]], "file is at or above root; no matching at_layer")
    }
  }
  active <- !(too_deep | bad_root)

  # ---- validate / extract from directory names (dir_layers only) ----
  for (layer in dir_layers) {
    raw_vals <- tbl[[paste0("layer__", layer)]]
    spec <- ft$dir_patterns[[layer]]
    if (is.null(spec) || length(spec) == 0) next

    layer_active <- active & !is.na(raw_vals)
    if (!any(layer_active)) next

    matched <- rep(FALSE, n)
    for (pat_nm in names(spec$compiled)) {
      m <- stringr::str_match(raw_vals, spec$compiled[[pat_nm]])
      ok <- layer_active & !is.na(m[, 1]) & !matched
      if (!any(ok)) next

      cap_names <- setdiff(colnames(m), "")
      for (cn in cap_names) {
        vals <- m[, cn]
        res <- set_capture_vec(tbl, ok & !is.na(vals), cn, vals, problems)
        tbl <- res$tbl
        problems <- res$msgs
      }
      matched <- matched | ok
    }

    unmatched <- layer_active & !matched
    if (any(unmatched)) {
      for (j in which(unmatched)) {
        problems[[j]] <- c(
          problems[[j]],
          sprintf("directory %s='%s' matches no pattern", layer, raw_vals[[j]])
        )
      }
    }
  }

  # ---- validate / extract from file name via patterns at at_layer ----
  fname <- tbl[[paste0("layer__", file_layer)]]
  for (layer in names(ft$file_patterns)) {
    spec <- ft$file_patterns[[layer]]
    layer_rows <- active & tbl$at_layer == layer
    if (!any(layer_rows)) next

    if (is.null(spec) || length(spec) == 0L) {
      for (j in which(layer_rows)) {
        problems[[j]] <- c(
          problems[[j]],
          sprintf("no file patterns registered at_layer='%s'", tbl$at_layer[[j]])
        )
      }
      next
    }

    matched <- rep(FALSE, n)
    for (pat_name in names(spec$compiled)) {
      m <- stringr::str_match(fname, spec$compiled[[pat_name]])
      ok <- layer_rows & !is.na(m[, 1]) & !matched
      if (!any(ok)) next

      matched_pattern[ok] <- pat_name
      cap_names <- setdiff(colnames(m), "")
      for (cn in cap_names) {
        vals <- m[, cn]
        res <- set_capture_vec(tbl, ok & !is.na(vals), cn, vals, problems)
        tbl <- res$tbl
        problems <- res$msgs
      }

      matched <- matched | ok
    }

    unmatched <- layer_rows & !matched
    if (any(unmatched)) {
      for (j in which(unmatched)) {
        problems[[j]] <- c(
          problems[[j]],
          sprintf("file '%s' matches no pattern at_layer='%s'", fname[[j]], tbl$at_layer[[j]])
        )
      }
    }
  }

  tbl$pattern <- matched_pattern
  tbl$.problems <- problems
  tbl$.ok <- lengths(problems) == 0

  # order columns: raw layer__ columns, then extracted fields, then diagnostics
  core <- c(".path", ".rel", "at_layer", layer_cols)
  diag <- c("pattern", ".ok", ".problems")
  extracted <- setdiff(names(tbl), c(core, diag))
  tbl <- tbl[, c(core, extracted, diag)]

  tbl
}

# ---- nice format + print ----

#' Format a filetree summary
#'
#' Create a human-readable summary of the filetree configuration, including
#' layers, regex pool size, and registered patterns.
#'
#' @param x A `filetree` object.
#' @param ... Unused, included for method signature compatibility.
#' @param width Optional output width forwarded to formatting helpers.
#' @return Character vector with the formatted summary.
#' @export
format.filetree <- function(x, ..., width = getOption("width")) {
  stopifnot(inherits(x, "filetree"))

  layers <- x$layers
  dir_layers <- if (length(layers) >= 2) layers[-length(layers)] else character()
  file_layer <- layers[length(layers)]

  lines <- character()
  lines <- c(lines, sprintf("<filetree> root: %s", x$root))
  lines <- c(lines, sprintf("  layers: %s", paste(layers, collapse = " / ")))
  lines <- c(lines, sprintf("  file_layer: %s", file_layer))

  pool_names <- names(x$regex_pool)
  if (length(pool_names) == 0) {
    lines <- c(lines, "  regex_pool: <empty>")
  } else {
    shown <- paste(pool_names, collapse = ", ")
    lines <- c(lines, sprintf("  regex_pool: %d (%s)", length(pool_names), shown))
  }

  # dir patterns
  if (length(dir_layers) == 0) {
    lines <- c(lines, "  dir_patterns: <none> (no dir layers)")
  } else {
    any_dir <- any(vapply(dir_layers, function(layer) {
      spec <- x$dir_patterns[[layer]]
      !(is.null(spec) || length(spec) == 0)
    }, logical(1)))

    if (!any_dir) {
      lines <- c(lines, "  dir_patterns: <none>")
    } else {
      lines <- c(lines, "  dir_patterns:")
      for (layer in dir_layers) {
        spec <- x$dir_patterns[[layer]]
        if (is.null(spec) || length(spec) == 0) {
          lines <- c(lines, sprintf("    - %s: <none>", layer))
        } else {
          kv <- paste0(names(spec$raw), "=\"", unname(spec$raw), "\"")
          s <- paste(kv, collapse = ", ")
          if (nchar(s) > 90) s <- paste0(substr(s, 1, 87), "\u2026")
          lines <- c(lines, sprintf("    - %s: %s", layer, s))
        }
      }
    }
  }

  # file patterns
  any_file <- any(vapply(names(x$file_patterns), function(layer) {
    spec <- x$file_patterns[[layer]]
    !(is.null(spec) || length(spec) == 0)
  }, logical(1)))

  if (!any_file) {
    lines <- c(lines, "  file_patterns: <none>")
  } else {
    lines <- c(lines, "  file_patterns:")
    for (layer in names(x$file_patterns)) {
      spec <- x$file_patterns[[layer]]
      if (is.null(spec) || length(spec) == 0) next
      kv <- paste0(names(spec$raw), "=\"", unname(spec$raw), "\"")
      s <- paste(kv, collapse = ", ")
      if (nchar(s) > 90) s <- paste0(substr(s, 1, 87), "\u2026")
      lines <- c(lines, sprintf("    - at_layer=%s: %s", layer, s))
    }
  }

  paste(lines, collapse = "\n")
}

#' Print a filetree summary
#'
#' Print the formatted summary of a `filetree` object to the console.
#'
#' @param x A `filetree` object.
#' @param ... Unused, included for method signature compatibility.
#' @param width Optional output width forwarded to [format.filetree()].
#' @return The input `filetree` object, invisibly.
#' @export
print.filetree <- function(x, ..., width = getOption("width")) {
  cat(format(x, ..., width = width), "\n")
  invisible(x)
}
