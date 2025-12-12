# filetree prototype (layers + global regex pool + patterns with {name} expansion)
# layers = what you see when you `ls` at each step, including the final file-name layer
#
# Key simplification: NO "group" concept. Patterns are just named patterns.
#
# deps: fs, stringr, tibble, dplyr, rlang

# ---- constructors ----
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

#' @export
ft_list <- function(ft) {
  stopifnot(inherits(ft, "filetree"))
  fs::dir_ls(ft$root, recurse = TRUE, type = "file")
}

# ---- indexing / parsing / validation ----

#' @export
.ft_at_layer_from_parts <- function(ft, parts) {
  # parts includes filename at end
  n_dir <- max(length(parts) - 1L, 0L)
  idx <- n_dir + 1L
  if (idx <= 0L) return(NA_character_)
  if (idx > length(ft$layers)) return(".__too_deep__")
  ft$layers[[idx]]
}

#' @export
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

#' @export
ft_index <- function(ft, files = ft_list(ft)) {
  stopifnot(inherits(ft, "filetree"))

  rel <- fs::path_rel(files, start = ft$root)
  parts_list <- strsplit(rel, .Platform$file.sep, fixed = TRUE)

  layers <- ft$layers
  dir_layers <- if (length(layers) >= 2) layers[-length(layers)] else character()
  file_layer <- layers[length(layers)]

  # build one column per layer, including the final file-name layer
  layer_mat <- matrix(NA_character_, nrow = length(parts_list), ncol = length(layers))
  colnames(layer_mat) <- layers

  at_layer <- character(length(parts_list))

  for (i in seq_along(parts_list)) {
    parts <- parts_list[[i]]
    fname <- tail(parts, 1)

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

  # extracted fields = placeholders not in layers
  all_placeholders <- .ft_all_placeholder_names(ft)
  extracted_fields <- setdiff(all_placeholders, layers)
  if (length(extracted_fields)) {
    for (nm in extracted_fields) tbl[[nm]] <- NA_character_
  }

  matched_pattern <- rep(NA_character_, nrow(tbl))
  problems <- vector("list", nrow(tbl))

  for (i in seq_len(nrow(tbl))) {
    msgs <- character()

    if (tbl$at_layer[[i]] == ".__too_deep__") {
      msgs <- c(msgs, sprintf("path deeper than layers (%d)", length(layers)))
      problems[[i]] <- msgs
      next
    }
    if (is.na(tbl$at_layer[[i]])) {
      msgs <- c(msgs, "file is at or above root; no matching at_layer")
      problems[[i]] <- msgs
      next
    }

    # ---- validate / extract from directory names (dir_layers only) ----
    for (layer in dir_layers) {
      val <- tbl[[layer]][i]
      if (is.na(val)) next

      spec <- ft$dir_patterns[[layer]]
      if (is.null(spec) || length(spec) == 0) next # unconstrained dir layer

      found <- FALSE
      for (pat_nm in names(spec$compiled)) {
        m <- stringr::str_match(val, spec$compiled[[pat_nm]])
        if (!all(is.na(m))) {
          found <- TRUE

          cap_names <- setdiff(colnames(m), "")
          for (cn in cap_names) {
            capv <- as.character(unname(m[1, cn]))
            if (cn %in% layers) {
              existing <- as.character(tbl[[cn]][i])
              if (!is.na(existing) && !identical(existing, capv)) {
                msgs <- c(msgs, sprintf("dir capture %s='%s' conflicts with %s='%s'",
                                        cn, capv, cn, existing))
              }
            } else {
              tbl[[cn]][i] <- capv
              rx <- ft$regex_pool[[cn]]
              if (!is.null(rx) && !stringr::str_detect(capv, paste0("^", rx, "$"))) {
                msgs <- c(msgs, sprintf("dir capture %s='%s' fails /%s/", cn, capv, rx))
              }
            }
          }

          break
        }
      }

      if (!found) {
        msgs <- c(msgs, sprintf("directory %s='%s' matches no pattern", layer, val))
      }
    }

    # ---- validate / extract from file name via patterns at at_layer ----
    spec <- ft$file_patterns[[tbl$at_layer[[i]]]]
    if (is.null(spec) || length(spec) == 0L) {
      msgs <- c(msgs, sprintf("no file patterns registered at_layer='%s'", tbl$at_layer[[i]]))
      problems[[i]] <- msgs
      next
    }

    fname <- tbl[[file_layer]][i]
    found_file <- FALSE

    for (pat_name in names(spec$compiled)) {
      m <- stringr::str_match(fname, spec$compiled[[pat_name]])
      if (!all(is.na(m))) {
        found_file <- TRUE
        matched_pattern[[i]] <- pat_name

        cap_names <- setdiff(colnames(m), "")
        for (cn in cap_names) {
          capv <- as.character(unname(m[1, cn]))
          if (cn %in% layers) {
            existing <- as.character(tbl[[cn]][i])
            if (!is.na(existing) && !identical(existing, capv)) {
              msgs <- c(msgs, sprintf("file capture %s='%s' conflicts with %s='%s'",
                                      cn, capv, cn, existing))
            }
          } else {
            tbl[[cn]][i] <- capv
            rx <- ft$regex_pool[[cn]]
            if (!is.null(rx) && !stringr::str_detect(capv, paste0("^", rx, "$"))) {
              msgs <- c(msgs, sprintf("file capture %s='%s' fails /%s/", cn, capv, rx))
            }
          }
        }

        break
      }
    }

    if (!found_file) {
      msgs <- c(msgs, sprintf("file '%s' matches no pattern at_layer='%s'",
                              fname, tbl$at_layer[[i]]))
    }

    problems[[i]] <- msgs
  }

  tbl$pattern <- matched_pattern
  tbl$.problems <- problems
  tbl$.ok <- lengths(problems) == 0

  tbl |>
    dplyr::relocate(dplyr::all_of(layers), .after = at_layer) |>
    dplyr::relocate(pattern, .ok, .problems, .after = dplyr::all_of(layers))
}

# ---- nice format + print ----

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
          if (nchar(s) > 90) s <- paste0(substr(s, 1, 87), "…")
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
      if (nchar(s) > 90) s <- paste0(substr(s, 1, 87), "…")
      lines <- c(lines, sprintf("    - at_layer=%s: %s", layer, s))
    }
  }

  paste(lines, collapse = "\n")
}

#' @export
print.filetree <- function(x, ..., width = getOption("width")) {
  cat(format(x, ..., width = width), "\n")
  invisible(x)
}

# ---- example (your demo-1) ----
# ft <- ft_init("./inst/demo-1", layers = c("subject", "time", "data"))
# ft <- ft |>
#   ft_add_regex(c(
#     subject = "\\w{2}-\\d{2}",
#     time    = "day\\d{2}",
#     task    = "red|green"
#   )) |>
#   ft_add_dir_pattern("subject", "{subject}") |>
#   ft_add_dir_pattern("time", "{time}") |>
#   ft_add_pattern(at_layer = "data", patterns = "{subject}_{task}\\.txt")
# ft
# idx <- ft_index(ft)
# idx |> dplyr::filter(!.ok) |> dplyr::select(.rel, at_layer, .problems) |> print(n = 50)
