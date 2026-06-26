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
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(
#'   root = root,
#'   layers = c("subject", "time", "data")
#' )
#' ft
#' @export
ft_init <- function(root, layers) {
  stopifnot(is.character(layers), length(layers) >= 1, all(nzchar(layers)))
  stopifnot(length(unique(layers)) == length(layers))

  dir_layers <- if (length(layers) >= 2) {
    layers[-length(layers)]
  } else {
    character()
  }

  structure(
    list(
      root = fs::path_abs(root),
      layers = layers, # includes final file-name layer
      regex_pool = rlang::set_names(list(), character()),
      dir_patterns = rlang::set_names(
        vector("list", length(dir_layers)),
        dir_layers
      ),
      file_patterns = rlang::set_names(vector("list", length(layers)), layers) # patterns at any layer
    ),
    class = "filetree"
  )
}
#' Register regex templates used by patterns
#'
#' Add named regular expressions to the pool that can be referenced by
#' placeholders such as `{subject}` inside directory or file patterns. Regexes
#' can also reference other regexes in the pool with the same placeholder syntax.
#'
#' @param ft A `filetree` object.
#' @param regexes Named character vector of regular expressions to store.
#' @return The updated `filetree` object.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     wtocs = "wT\\d\\d",
#'     stocs = "s\\dT[01]\\d",
#'     tocs = "{wtocs}|{stocs}",
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}",
#'     task = "red|green"
#'   ))
#'
#' names(ft$regex_pool)
#' @export
ft_add_regex <- function(ft, regexes) {
  stopifnot(
    inherits(ft, "filetree"),
    is.character(regexes),
    !is.null(names(regexes))
  )
  for (nm in names(regexes)) {
    rx <- regexes[[nm]]
    stopifnot(length(rx) == 1, nzchar(rx))
    ft$regex_pool[[nm]] <- rx
  }
  .ft_validate_regex_pool(ft$regex_pool)
  ft <- .ft_recompile_patterns(ft)
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
    stop(
      "Pattern references unknown regex name(s): ",
      paste(missing, collapse = ", "),
      "\n  pattern: ",
      pattern
    )
  }

  compiled <- pattern
  capture_names <- paste0("ftcap", seq_along(ph))
  names(capture_names) <- ph
  for (nm in ph) {
    rx <- .ft_expand_pool_regex(nm, regex_pool)
    # Rewrite anchors so nested regexes still respect layer boundaries when
    # embedded into a full path pattern.
    rx <- .ft_rewrite_pool_anchors(rx)
    compiled <- stringr::str_replace_all(
      compiled,
      stringr::fixed(paste0("{", nm, "}")),
      paste0("(?<", capture_names[[nm]], ">(?:", rx, "))")
    )
  }
  compiled <- paste0("^", compiled, "$")
  attr(compiled, "capture_names") <- stats::setNames(
    names(capture_names),
    capture_names
  )
  compiled
}

.ft_restore_capture_names <- function(match, compiled) {
  capture_names <- attr(compiled, "capture_names", exact = TRUE)
  if (is.null(capture_names)) {
    return(match)
  }

  cols <- colnames(match)
  remap <- match(cols, names(capture_names))
  replace <- !is.na(remap)
  cols[replace] <- unname(capture_names[remap[replace]])
  colnames(match) <- cols
  match
}

.ft_expand_pool_regex <- function(name, regex_pool, stack = character()) {
  if (!name %in% names(regex_pool)) {
    stop("Regex pool entry references unknown regex name: ", name)
  }
  if (name %in% stack) {
    cycle <- c(stack[match(name, stack):length(stack)], name)
    stop("Cyclic regex reference: ", paste(cycle, collapse = " -> "))
  }

  rx <- regex_pool[[name]]
  ph <- .ft_placeholders(rx)
  missing <- setdiff(ph, names(regex_pool))
  if (length(missing)) {
    stop(
      "Regex pool entry '",
      name,
      "' references unknown regex name(s): ",
      paste(missing, collapse = ", ")
    )
  }

  stack <- c(stack, name)
  for (nm in ph) {
    nested <- .ft_expand_pool_regex(nm, regex_pool, stack)
    nested <- .ft_rewrite_pool_anchors(nested)
    rx <- stringr::str_replace_all(
      rx,
      stringr::fixed(paste0("{", nm, "}")),
      paste0("(?:", nested, ")")
    )
  }
  rx
}

.ft_validate_regex_pool <- function(regex_pool) {
  for (nm in names(regex_pool)) {
    .ft_expand_pool_regex(nm, regex_pool)
  }
  invisible(regex_pool)
}

.ft_rewrite_pool_anchors <- function(rx) {
  if (stringr::str_starts(rx, stringr::fixed("^"))) {
    rx <- stringr::str_replace(rx, "^\\^", "(?:(?<=/)|^)")
  }
  if (stringr::str_ends(rx, stringr::fixed("$"))) {
    rx <- stringr::str_replace(rx, "\\$$", "(?:(?=/)|$)")
  }
  rx
}

.ft_normalize_patterns <- function(patterns) {
  stopifnot(is.character(patterns))
  if (length(patterns) == 1 && is.null(names(patterns))) {
    names(patterns) <- "default"
  }
  stopifnot(!is.null(names(patterns)))
  patterns
}

.ft_recompile_patterns <- function(ft) {
  for (layer in names(ft$dir_patterns)) {
    spec <- ft$dir_patterns[[layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }
    compiled <- lapply(
      spec$raw,
      .ft_compile_pattern,
      regex_pool = ft$regex_pool
    )
    ft$dir_patterns[[layer]]$compiled <- compiled
  }

  for (at_layer in names(ft$file_patterns)) {
    spec <- ft$file_patterns[[at_layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }
    if (is.null(spec$with)) {
      spec$with <- rep(
        list(rlang::set_names(character(), character())),
        length(spec$raw)
      )
      names(spec$with) <- names(spec$raw)
    }
    regex_pool <- lapply(
      spec$with,
      .ft_merge_regex_pool,
      regex_pool = ft$regex_pool
    )
    compiled <- Map(.ft_compile_pattern, spec$raw, regex_pool)
    ft$file_patterns[[at_layer]]$compiled <- compiled
    ft$file_patterns[[at_layer]]$regex_pool <- regex_pool
  }

  ft
}

.ft_merge_regex_pool <- function(with, regex_pool) {
  out <- regex_pool
  for (nm in names(with)) {
    out[[nm]] <- with[[nm]]
  }
  .ft_validate_regex_pool(out)
  out
}

.ft_normalize_when <- function(when) {
  if (is.null(when)) {
    return(rlang::set_names(character(), character()))
  }
  stopifnot(!is.null(names(when)), all(nzchar(names(when))))
  if (is.character(when)) {
    stopifnot(all(nzchar(when)))
    when <- as.list(when)
  } else {
    stopifnot(is.list(when))
    stopifnot(all(vapply(when, is.character, logical(1))))
    stopifnot(all(vapply(
      when,
      function(x) length(x) > 0 && all(nzchar(x)),
      logical(1)
    )))
  }
  when
}

.ft_normalize_local_regex <- function(with) {
  if (is.null(with)) {
    return(rlang::set_names(character(), character()))
  }
  stopifnot(is.character(with), !is.null(names(with)))
  stopifnot(all(nzchar(names(with))), all(nzchar(with)))
  with
}

.ft_unique_pattern_names <- function(new_names, existing_names = character()) {
  out <- new_names
  seen <- existing_names
  for (i in seq_along(out)) {
    base <- out[[i]]
    candidate <- base
    suffix <- 2L
    while (candidate %in% seen) {
      candidate <- paste0(base, "_", suffix)
      suffix <- suffix + 1L
    }
    out[[i]] <- candidate
    seen <- c(seen, candidate)
  }
  out
}

.ft_when_matches <- function(tbl, when) {
  if (is.null(when) || length(when) == 0) {
    return(rep(TRUE, nrow(tbl)))
  }

  ok <- rep(TRUE, nrow(tbl))
  for (nm in names(when)) {
    values <- if (!is.null(tbl[[nm]])) {
      tbl[[nm]]
    } else {
      tbl[[paste0("layer__", nm)]]
    }

    if (is.null(values)) {
      ok <- ok & FALSE
    } else {
      ok <- ok & !is.na(values) & values %in% unname(when[[nm]])
    }
  }
  ok
}

.ft_file_pattern_matches <- function(file_name, row, spec) {
  if (is.null(spec) || length(spec) == 0) {
    return(FALSE)
  }

  row_tbl <- row[1, , drop = FALSE]
  for (pat_i in seq_along(spec$compiled)) {
    when <- spec$when[[pat_i]]
    if (!.ft_when_matches(row_tbl, when)) {
      next
    }

    m <- stringr::str_match(file_name, spec$compiled[[pat_i]])
    if (!is.na(m[, 1])) return(TRUE)
  }

  FALSE
}

.ft_candidate_file_layers <- function(ft, n_dir) {
  layers <- ft$layers
  default_idx <- n_dir + 1L
  parent_idx <- n_dir
  idx <- c(default_idx, parent_idx)
  idx <- idx[idx >= 1L & idx <= length(layers)]
  unique(layers[idx])
}

.ft_resolve_file_layers <- function(tbl, ft, active) {
  layer_cols <- paste0("layer__", ft$layers)
  dir_layer_cols <- layer_cols[-length(layer_cols)]
  fname <- tbl[[layer_cols[[length(layer_cols)]]]]

  for (j in which(active)) {
    n_dir <- if (length(dir_layer_cols)) {
      sum(!is.na(unlist(tbl[j, dir_layer_cols, drop = FALSE])))
    } else {
      0L
    }
    candidates <- .ft_candidate_file_layers(ft, n_dir)
    if (!length(candidates)) {
      next
    }

    fallback_layer <- NA_character_
    for (layer in candidates) {
      spec <- ft$file_patterns[[layer]]
      if (is.na(fallback_layer) && .ft_has_file_patterns(ft, layer)) {
        fallback_layer <- layer
      }
      if (.ft_file_pattern_matches(fname[[j]], tbl[j, , drop = FALSE], spec)) {
        tbl$at_layer[[j]] <- layer
        break
      }
    }
    if (
      !is.na(fallback_layer) && !.ft_has_file_patterns(ft, tbl$at_layer[[j]])
    ) {
      tbl$at_layer[[j]] <- fallback_layer
    }
  }

  tbl
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
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}"
#'   )) |>
#'   ft_add_dir_pattern("subject", "{subject}") |>
#'   ft_add_dir_pattern("time", "{time}")
#'
#' ft
#' @export
ft_add_dir_pattern <- function(ft, layer, patterns) {
  stopifnot(inherits(ft, "filetree"))
  stopifnot(
    is.character(layer),
    length(layer) == 1,
    layer %in% names(ft$dir_patterns)
  )

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
#' for files that belong to a specific layer. File patterns may be registered
#' on any configured layer, including non-terminal directory layers, so sidecar
#' files such as subject-level manifests can live beside child directories.
#'
#' @param ft A `filetree` object.
#' @param layer Layer at which the files live (must be listed in `ft$layers`).
#' @param patterns Named character vector of file-name patterns that may use
#'   `{placeholder}` references tied to `ft`'s regex pool.
#' @param when Optional named character vector or named list of exact-match
#'   conditions. A conditional file pattern is applied only when every condition
#'   matches an extracted field or raw layer value with the same name. Use a
#'   list when a condition can match any of several values.
#' @param with Optional named character vector of pattern-local regex
#'   definitions. These definitions override `ft`'s regex pool for this file
#'   pattern only.
#' @return The updated `filetree` object.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}",
#'     task = "red|green"
#'   )) |>
#'   ft_add_file_pattern(
#'     "data",
#'     c(txt = "{subject}_{task}.txt"),
#'     when = list(time = c("day01", "day02"))
#'   ) |>
#'   ft_add_file_pattern(
#'     "data",
#'     c(wav = "{subject}_{task}.wav"),
#'     when = c(time = "day03"),
#'     with = c(task = "yellow")
#'   )
#'
#' ft
#' @export
ft_add_file_pattern <- function(ft, layer, patterns, when = NULL, with = NULL) {
  stopifnot(inherits(ft, "filetree"))
  stopifnot(is.character(layer), length(layer) == 1, layer %in% ft$layers)

  patterns <- .ft_normalize_patterns(patterns)
  when <- .ft_normalize_when(when)
  with <- .ft_normalize_local_regex(with)
  regex_pool <- .ft_merge_regex_pool(with, ft$regex_pool)
  compiled <- lapply(patterns, .ft_compile_pattern, regex_pool = regex_pool)

  existing <- ft$file_patterns[[layer]]
  existing_names <- if (is.null(existing) || length(existing) == 0) {
    character()
  } else {
    names(existing$raw)
  }
  names(patterns) <- .ft_unique_pattern_names(names(patterns), existing_names)
  names(compiled) <- names(patterns)

  when_list <- rep(list(when), length(patterns))
  names(when_list) <- names(patterns)
  regex_pool_list <- rep(list(regex_pool), length(patterns))
  names(regex_pool_list) <- names(patterns)
  with_list <- rep(list(with), length(patterns))
  names(with_list) <- names(patterns)

  if (is.null(existing) || length(existing) == 0) {
    ft$file_patterns[[layer]] <- list(
      raw = patterns,
      compiled = compiled,
      when = when_list,
      with = with_list,
      regex_pool = regex_pool_list
    )
  } else {
    ft$file_patterns[[layer]]$raw <- c(existing$raw, patterns)
    ft$file_patterns[[layer]]$compiled <- c(existing$compiled, compiled)
    existing_when <- existing$when
    if (is.null(existing_when)) {
      existing_when <- rep(
        list(rlang::set_names(character(), character())),
        length(existing$raw)
      )
      names(existing_when) <- names(existing$raw)
    }
    ft$file_patterns[[layer]]$when <- c(existing_when, when_list)
    existing_with <- existing$with
    if (is.null(existing_with)) {
      existing_with <- rep(
        list(rlang::set_names(character(), character())),
        length(existing$raw)
      )
      names(existing_with) <- names(existing$raw)
    }
    ft$file_patterns[[layer]]$with <- c(existing_with, with_list)
    existing_regex_pool <- existing$regex_pool
    if (is.null(existing_regex_pool)) {
      existing_regex_pool <- rep(list(ft$regex_pool), length(existing$raw))
      names(existing_regex_pool) <- names(existing$raw)
    }
    ft$file_patterns[[layer]]$regex_pool <- c(
      existing_regex_pool,
      regex_pool_list
    )
  }
  ft
}

# ---- file enumeration ----

#' List files under the filetree root
#'
#' Return all files under the filetree root using the configured `fs` helper.
#'
#' @param ft A `filetree` object.
#' @return Character vector of file paths relative to the working directory.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data"))
#'
#' head(ft_list(ft))
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
#' @examples
#' ft <- ft_init(tempdir(), c("subject", "time", "data"))
#'
#' .ft_at_layer_from_parts(ft, c("ab-01", "day01", "ab-01_red.txt"))
#' .ft_at_layer_from_parts(ft, c("ab-01", "day01", "extra", "file.txt"))
#' @export
.ft_at_layer_from_parts <- function(ft, parts) {
  # parts includes filename at end
  n_dir <- max(length(parts) - 1L, 0L)
  idx <- n_dir + 1L
  if (idx <= 0L) {
    return(NA_character_)
  }
  if (idx > length(ft$layers)) {
    return(".__too_deep__")
  }
  ft$layers[[idx]]
}


.ft_all_placeholder_names <- function(ft) {
  out <- character()

  # dir patterns
  for (layer in names(ft$dir_patterns)) {
    spec <- ft$dir_patterns[[layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }
    out <- c(out, unlist(lapply(spec$raw, .ft_placeholders), use.names = FALSE))
  }

  # file patterns
  for (at_layer in names(ft$file_patterns)) {
    spec <- ft$file_patterns[[at_layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }
    out <- c(out, unlist(lapply(spec$raw, .ft_placeholders), use.names = FALSE))
  }

  unique(out)
}

#' Index files against a filetree specification
#'
#' Validate file paths against the configured directory and file-name patterns,
#' extract placeholder captures, and report any problems. File patterns
#' registered on parent layers are considered for sidecar files before
#' unmatched files are reported.
#'
#' @param ft A `filetree` object.
#' @param files Optional character vector of file paths to check. Defaults to
#'   all files under `ft$root` via [ft_list()].
#' @param strict Logical. If `TRUE`, files at layers without registered file
#'   patterns are reported as problems. If `FALSE`, missing file patterns are
#'   accepted so partial schemas can be used for exploratory indexing.
#' @return A tibble with layer columns (`layer__<name>`), captured placeholders,
#'   the matched pattern name, `.ok` flag, and `.problems` list-column.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}",
#'     task = "red|green"
#'   )) |>
#'   ft_add_dir_pattern("subject", "{subject}") |>
#'   ft_add_dir_pattern("time", "{time}") |>
#'   ft_add_file_pattern("data", "{subject}_{task}.txt")
#'
#' ft_index(ft)
#'
#' ft_partial <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}"
#'   )) |>
#'   ft_add_dir_pattern("subject", "{subject}") |>
#'   ft_add_dir_pattern("time", "{time}")
#'
#' ft_index(ft_partial)
#' ft_index(ft_partial, strict = TRUE)
#' @export
ft_index <- function(ft, files = ft_list(ft), strict = FALSE) {
  stopifnot(inherits(ft, "filetree"))
  stopifnot(is.logical(strict), length(strict) == 1, !is.na(strict))

  rel <- fs::path_rel(files, start = ft$root)
  parts_list <- strsplit(rel, .Platform$file.sep, fixed = TRUE)

  layers <- ft$layers
  dir_layers <- if (length(layers) >= 2) {
    layers[-length(layers)]
  } else {
    character()
  }
  file_layer <- layers[length(layers)]

  layer_cols <- paste0("layer__", layers)

  # build one column per layer name (raw component), including final file-name layer
  layer_mat <- matrix(
    NA_character_,
    nrow = length(parts_list),
    ncol = length(layers)
  )
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
  set_capture_vec <- function(
    tbl,
    idx,
    cn,
    values,
    msgs,
    regex_pool,
    source_label = "extracted value",
    existing_label = "earlier path layer"
  ) {
    if (!any(idx)) {
      return(list(tbl = tbl, msgs = msgs))
    }
    values <- as.character(unname(values))
    existing <- tbl[[cn]]

    conflicts <- idx & !is.na(existing) & !is.na(values) & existing != values
    if (any(conflicts)) {
      for (j in which(conflicts)) {
        msgs[[j]] <- c(
          msgs[[j]],
          sprintf(
            "%s has {.var %s} {.val %s}, but %s has {.var %s} {.val %s}",
            source_label,
            cn,
            values[[j]],
            existing_label,
            cn,
            existing[[j]]
          )
        )
      }
    }

    replace_idx <- idx & (is.na(existing) | existing == values)
    if (any(replace_idx)) {
      tbl[[cn]][replace_idx] <- values[replace_idx]
    }

    rx <- regex_pool[[cn]]
    if (!is.null(rx)) {
      rx <- .ft_expand_pool_regex(cn, regex_pool)
      bad_rx <- idx &
        !is.na(tbl[[cn]]) &
        !stringr::str_detect(tbl[[cn]], paste0("^(?:", rx, ")$"))
      if (any(bad_rx)) {
        for (j in which(bad_rx)) {
          msgs[[j]] <- c(
            msgs[[j]],
            sprintf("{.var %s} {.val %s} fails /%s/", cn, tbl[[cn]][[j]], rx)
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
      problems[[j]] <- c(
        problems[[j]],
        sprintf("path deeper than layers (%d)", length(layers))
      )
    }
  }
  if (any(bad_root)) {
    for (j in which(bad_root)) {
      problems[[j]] <- c(
        problems[[j]],
        "file is at or above root; no matching layer"
      )
    }
  }
  active <- !(too_deep | bad_root)

  # ---- validate / extract from directory names (dir_layers only) ----
  for (layer in dir_layers) {
    raw_vals <- tbl[[paste0("layer__", layer)]]
    spec <- ft$dir_patterns[[layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }

    layer_active <- active & !is.na(raw_vals)
    if (!any(layer_active)) {
      next
    }

    matched <- rep(FALSE, n)
    for (pat_nm in names(spec$compiled)) {
      m <- stringr::str_match(raw_vals, spec$compiled[[pat_nm]])
      m <- .ft_restore_capture_names(m, spec$compiled[[pat_nm]])
      ok <- layer_active & !is.na(m[, 1]) & !matched
      if (!any(ok)) {
        next
      }

      cap_names <- setdiff(colnames(m), "")
      for (cn in cap_names) {
        vals <- m[, cn]
        res <- set_capture_vec(
          tbl,
          ok & !is.na(vals),
          cn,
          vals,
          problems,
          regex_pool = ft$regex_pool,
          source_label = sprintf("directory %s", layer)
        )
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
          sprintf(
            "directory name '%s' does not match a dir pattern at layer `%s`",
            raw_vals[[j]],
            layer
          )
        )
      }
    }
  }

  tbl <- .ft_resolve_file_layers(tbl, ft, active)

  # ---- validate / extract from file name via patterns at at_layer ----
  fname <- tbl[[paste0("layer__", file_layer)]]
  for (layer in names(ft$file_patterns)) {
    spec <- ft$file_patterns[[layer]]
    layer_rows <- active & tbl$at_layer == layer
    if (!any(layer_rows)) {
      next
    }

    if (is.null(spec) || length(spec) == 0L) {
      if (!strict) {
        next
      }
      for (j in which(layer_rows)) {
        problems[[j]] <- c(
          problems[[j]],
          sprintf(
            "no file patterns registered for `%s` files",
            tbl$at_layer[[j]]
          )
        )
      }
      next
    }

    matched <- rep(FALSE, n)
    applicable <- rep(FALSE, n)
    for (pat_i in seq_along(spec$compiled)) {
      pat_name <- names(spec$compiled)[[pat_i]]
      when <- spec$when[[pat_i]]
      pattern_rows <- layer_rows & .ft_when_matches(tbl, when)
      applicable <- applicable | pattern_rows
      if (!any(pattern_rows)) {
        next
      }

      m <- stringr::str_match(fname, spec$compiled[[pat_i]])
      m <- .ft_restore_capture_names(m, spec$compiled[[pat_i]])
      ok <- pattern_rows & !is.na(m[, 1]) & !matched
      if (!any(ok)) {
        next
      }

      matched_pattern[ok] <- pat_name
      cap_names <- setdiff(colnames(m), "")
      regex_pool <- spec$regex_pool[[pat_i]]
      if (is.null(regex_pool)) {
        regex_pool <- ft$regex_pool
      }
      for (cn in cap_names) {
        vals <- m[, cn]
        res <- set_capture_vec(
          tbl,
          ok & !is.na(vals),
          cn,
          vals,
          problems,
          regex_pool = regex_pool,
          source_label = "filename",
          existing_label = "a parent directory"
        )
        tbl <- res$tbl
        problems <- res$msgs
      }

      matched <- matched | ok
    }

    no_applicable <- layer_rows & !applicable
    if (strict && any(no_applicable)) {
      for (j in which(no_applicable)) {
        problems[[j]] <- c(
          problems[[j]],
          sprintf(
            "filename '%s' does not match an applicable file pattern at layer `%s`",
            fname[[j]],
            tbl$at_layer[[j]]
          )
        )
      }
    }

    unmatched <- applicable & !matched
    if (any(unmatched)) {
      for (j in which(unmatched)) {
        problems[[j]] <- c(
          problems[[j]],
          sprintf(
            "filename '%s' does not match a file pattern at layer `%s`",
            fname[[j]],
            tbl$at_layer[[j]]
          )
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

#' Glimpse filetree indexing problems
#'
#' Print a compact summary of problem files and their problem messages. Accepts
#' either a `filetree` object or the tibble returned by [ft_index()].
#'
#' @param x A `filetree` object or an index tibble returned by [ft_index()].
#' @param n Maximum number of problem files to print.
#' @param ... Additional arguments passed to [ft_index()] when `x` is a
#'   `filetree` object.
#' @return A tibble containing all problem rows, invisibly.
#' @examples
#' root <- system.file("demo-2", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}",
#'     task = "red|green"
#'   )) |>
#'   ft_add_dir_pattern("subject", "{subject}") |>
#'   ft_add_dir_pattern("time", "{time}") |>
#'   ft_add_file_pattern("data", "{subject}_{task}.txt")
#'
#' ft_glimpse_problems(ft, n = 3)
#' @export
ft_glimpse_problems <- function(x, n = 10, ...) {
  stopifnot(is.numeric(n), length(n) == 1, !is.na(n), n >= 0)
  n <- as.integer(n)

  if (inherits(x, "filetree")) {
    index <- ft_index(x, ...)
  } else {
    index <- x
  }
  .ft_validate_index(index)

  problem_rows <- index[!index$.ok, , drop = FALSE]
  total_files <- nrow(index)
  problem_files <- nrow(problem_rows)
  total_problems <- sum(lengths(problem_rows$.problems))

  cat(sprintf(
    "%d/%d files with %d problems.\n",
    problem_files,
    total_files,
    total_problems
  ))

  shown <- min(problem_files, n)
  if (problem_files > shown) {
    cat(sprintf("Showing %d of %d problem files.\n", shown, problem_files))
  }

  if (shown > 0) {
    for (i in seq_len(shown)) {
      cat("\n")
      cat(as.character(problem_rows$.rel[[i]]), "\n", sep = "")
      for (problem in problem_rows$.problems[[i]]) {
        cli::cli_bullets(c("*" = problem))
      }
    }
  }

  invisible(problem_rows)
}

.ft_validate_index <- function(index) {
  needed <- c(".rel", ".ok", ".problems")
  missing <- setdiff(needed, names(index))
  if (length(missing)) {
    stop(
      "Index is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(index)
}

#' Format a filetree schema as a tree
#'
#' Create a tree-shaped summary of the declared directory and file patterns.
#' File patterns are shown in the parent directory where files for that layer
#' live, with labels such as `` `time` file:`` and `` `data` file:``.
#'
#' @param ft A `filetree` object.
#' @return Character vector containing the schema tree lines.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}",
#'     task = "red|green"
#'   )) |>
#'   ft_add_dir_pattern("subject", "{subject}") |>
#'   ft_add_dir_pattern("time", "{time}") |>
#'   ft_add_file_pattern("data", "{subject}_{task}.txt")
#'
#' ft_format_schema_tree(ft)
#' @export
ft_format_schema_tree <- function(ft) {
  stopifnot(inherits(ft, "filetree"))

  layers <- ft$layers
  dir_layers <- if (length(layers) >= 2) {
    layers[-length(layers)]
  } else {
    character()
  }

  lines <- as.character(ft$root)

  if (!length(dir_layers)) {
    return(c(
      lines,
      .ft_format_schema_items(.ft_format_file_schema(ft, layers[[1]]), "")
    ))
  }

  root_file_lines <- .ft_format_file_schema(ft, layers[[1]])
  dir_lines <- .ft_format_schema_dir(ft, 1L, "", has_following = FALSE)
  c(
    lines,
    .ft_format_schema_items(
      root_file_lines,
      "",
      has_following = length(dir_lines) > 0
    ),
    dir_lines
  )
}

#' Print a filetree schema tree
#'
#' Print the result of [ft_format_schema_tree()] and invisibly return the input
#' `filetree`.
#'
#' @param ft A `filetree` object.
#' @return The input `filetree`, invisibly.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#' ft <- ft_init(root, c("subject", "time", "data"))
#'
#' ft_schema_tree(ft)
#' @export
ft_schema_tree <- function(ft) {
  cat(ft_format_schema_tree(ft), sep = "\n")
  cat("\n")
  invisible(ft)
}

.ft_has_file_patterns <- function(ft, layer) {
  spec <- ft$file_patterns[[layer]]
  !(is.null(spec) || length(spec) == 0)
}

.ft_format_dir_schema <- function(ft, layer) {
  spec <- ft$dir_patterns[[layer]]
  if (is.null(spec) || length(spec) == 0) {
    return(paste0(layer, ": <none>"))
  }
  paste0(layer, ": ", paste(unname(spec$raw), collapse = " | "))
}

.ft_format_file_schema <- function(ft, layer) {
  spec <- ft$file_patterns[[layer]]
  if (is.null(spec) || length(spec) == 0) {
    return(character())
  }

  out <- character(length(spec$raw))
  for (i in seq_along(spec$raw)) {
    nm <- names(spec$raw)[[i]]
    pattern_label <- if (identical(nm, "default") && length(spec$raw) == 1) {
      unname(spec$raw[[i]])
    } else {
      paste0(nm, " = ", unname(spec$raw[[i]]))
    }
    label <- paste0("`", layer, "` file: ", pattern_label)
    annotations <- c(
      .ft_format_when_annotation(spec$when[[i]]),
      .ft_format_with_annotation(spec$with[[i]])
    )
    annotations <- annotations[nzchar(annotations)]
    if (length(annotations)) {
      label <- paste0(label, " [", paste(annotations, collapse = "; "), "]")
    }
    out[[i]] <- label
  }
  out
}

.ft_format_schema_dir <- function(ft, dir_i, prefix, has_following = FALSE) {
  dir_layers <- ft$layers[-length(ft$layers)]
  layer <- dir_layers[[dir_i]]

  branch <- if (has_following) {
    "\u251c\u2500\u2500 "
  } else {
    "\u2514\u2500\u2500 "
  }
  lines <- paste0(prefix, branch, .ft_format_dir_schema(ft, layer))
  child_prefix <- paste0(prefix, if (has_following) "\u2502   " else "    ")

  child_layer <- ft$layers[[dir_i + 1L]]
  file_lines <- .ft_format_file_schema(ft, child_layer)
  next_lines <- if (dir_i < length(dir_layers)) {
    .ft_format_schema_dir(ft, dir_i + 1L, child_prefix)
  } else {
    character()
  }

  c(
    lines,
    .ft_format_schema_items(
      file_lines,
      child_prefix,
      has_following = length(next_lines) > 0
    ),
    next_lines
  )
}

.ft_format_schema_items <- function(items, prefix, has_following = FALSE) {
  if (!length(items)) {
    return(character())
  }

  out <- character(length(items))
  for (i in seq_along(items)) {
    is_last <- i == length(items) && !has_following
    branch <- if (is_last) {
      "\u2514\u2500\u2500 "
    } else {
      "\u251c\u2500\u2500 "
    }
    out[[i]] <- paste0(prefix, branch, items[[i]])
  }
  out
}

.ft_format_when_annotation <- function(when) {
  if (is.null(when) || length(when) == 0) {
    return("")
  }
  pieces <- character(length(when))
  for (i in seq_along(when)) {
    values <- unname(when[[i]])
    op <- if (length(values) == 1) " == " else " in "
    pieces[[i]] <- paste0(names(when)[[i]], op, paste(values, collapse = ", "))
  }
  paste("when", paste(pieces, collapse = " and "))
}

.ft_format_with_annotation <- function(with) {
  if (is.null(with) || length(with) == 0) {
    return("")
  }
  pieces <- paste0(names(with), " = ", unname(with))
  paste("with", paste(pieces, collapse = ", "))
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
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data"))
#'
#' cat(format(ft))
#' @export
format.filetree <- function(x, ..., width = getOption("width")) {
  stopifnot(inherits(x, "filetree"))

  layers <- x$layers
  dir_layers <- if (length(layers) >= 2) {
    layers[-length(layers)]
  } else {
    character()
  }
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
    lines <- c(
      lines,
      sprintf("  regex_pool: %d (%s)", length(pool_names), shown)
    )
  }

  # dir patterns
  if (length(dir_layers) == 0) {
    lines <- c(lines, "  dir_patterns: <none> (no dir layers)")
  } else {
    any_dir <- any(vapply(
      dir_layers,
      function(layer) {
        spec <- x$dir_patterns[[layer]]
        !(is.null(spec) || length(spec) == 0)
      },
      logical(1)
    ))

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
          if (nchar(s) > 90) {
            s <- paste0(substr(s, 1, 87), "\u2026")
          }
          lines <- c(lines, sprintf("    - %s: %s", layer, s))
        }
      }
    }
  }

  # file patterns
  any_file <- any(vapply(
    names(x$file_patterns),
    function(layer) {
      spec <- x$file_patterns[[layer]]
      !(is.null(spec) || length(spec) == 0)
    },
    logical(1)
  ))

  if (!any_file) {
    lines <- c(lines, "  file_patterns: <none>")
  } else {
    lines <- c(lines, "  file_patterns:")
    for (layer in names(x$file_patterns)) {
      spec <- x$file_patterns[[layer]]
      if (is.null(spec) || length(spec) == 0) {
        next
      }
      kv <- paste0(names(spec$raw), "=\"", unname(spec$raw), "\"")
      s <- paste(kv, collapse = ", ")
      if (nchar(s) > 90) {
        s <- paste0(substr(s, 1, 87), "\u2026")
      }
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
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data"))
#'
#' print(ft)
#' @export
print.filetree <- function(x, ..., width = getOption("width")) {
  cat(format(x, ..., width = width), "\n")
  invisible(x)
}
