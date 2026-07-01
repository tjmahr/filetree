# filetree prototype (layers + global regex pool + templates with {name} expansion)
# layers = what you see when you `ls` at each step, including the final file-name layer
#
# Key simplification: NO "group" concept. Templates are stored directly by layer.
#
# deps: fs, stringr, tibble, dplyr, rlang

.ft_abort_arg <- function(arg, message, call = rlang::caller_env()) {
  rlang::abort(paste0("`", arg, "` ", message), call = call)
}

.ft_check_filetree <- function(x, arg = "ft", call = rlang::caller_env()) {
  if (!inherits(x, "filetree")) {
    .ft_abort_arg(arg, "must be a filetree object.", call = call)
  }
}

.ft_check_root <- function(root, call = rlang::caller_env()) {
  if (
    !is.character(root) || length(root) != 1 || is.na(root) || !nzchar(root)
  ) {
    .ft_abort_arg("root", "must be a single non-empty path.", call = call)
  }
}

.ft_collapse_or <- function(x) {
  if (!length(x)) {
    return("<none>")
  }
  if (length(x) == 1) {
    return(x)
  }
  paste0(paste(x[-length(x)], collapse = ", "), " or ", x[[length(x)]])
}

# ---- constructors ----

#' Create a filetree specification
#'
#' Build a `filetree` object that records the root directory, ordered layers,
#' and slots for field regexes and templates.
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
  .ft_check_root(root)
  if (
    !is.character(layers) ||
      length(layers) < 1 ||
      any(is.na(layers)) ||
      !all(nzchar(layers))
  ) {
    .ft_abort_arg(
      "layers",
      "must be a non-empty character vector with no empty values."
    )
  }
  if (length(unique(layers)) != length(layers)) {
    .ft_abort_arg("layers", "must not contain duplicate values.")
  }

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
      dir_templates = rlang::set_names(
        vector("list", length(dir_layers)),
        dir_layers
      ),
      file_templates = rlang::set_names(vector("list", length(layers)), layers), # templates at any layer
      ignore_dir_templates = rlang::set_names(
        vector("list", length(dir_layers)),
        dir_layers
      ),
      ignore_file_templates = rlang::set_names(
        vector("list", length(layers)),
        layers
      )
    ),
    class = "filetree"
  )
}

#' Change a filetree root
#'
#' Return a copy of a `filetree` object with a new root directory.
#'
#' @param ft A `filetree` object.
#' @param root Path to the new root directory used as the base for indexing.
#' @return The updated `filetree` object.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#' ft <- ft_init(root, c("subject", "time", "data"))
#'
#' ft_set_root(ft, tempdir())
#' @export
ft_set_root <- function(ft, root) {
  .ft_check_filetree(ft)
  .ft_check_root(root)
  ft$root <- fs::path_abs(root)
  ft
}
#' Register field regexes used by component templates
#'
#' Add named regular expressions for values that should be extracted from
#' directory or file names. Field regexes can be referenced by placeholders such
#' as `{subject}` inside directory or file component templates. Field regexes can
#' also reference other field regexes with the same placeholder syntax.
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
  .ft_check_filetree(ft)
  if (
    !is.character(regexes) ||
      length(regexes) < 1 ||
      is.null(names(regexes)) ||
      any(is.na(names(regexes))) ||
      !all(nzchar(names(regexes)))
  ) {
    .ft_abort_arg("regexes", "must be a named character vector.")
  }
  for (nm in names(regexes)) {
    rx <- regexes[[nm]]
    if (length(rx) != 1 || is.na(rx) || !nzchar(rx)) {
      .ft_abort_arg("regexes", "values must be non-empty strings.")
    }
    ft$regex_pool[[nm]] <- rx
  }
  .ft_validate_regex_pool(ft$regex_pool)
  ft <- .ft_recompile_templates(ft)
  ft
}

# ---- template compilation helpers ----

.ft_placeholders <- function(x) {
  m <- stringr::str_match_all(x, "\\{([A-Za-z][A-Za-z0-9_]*)\\}")[[1]]
  if (nrow(m) == 0) character() else unique(m[, 2])
}

.ft_compile_template <- function(template, regex_pool) {
  tokens <- .ft_template_tokens(template)
  ph <- unique(tokens$value[tokens$type == "field"])
  missing <- setdiff(ph, names(regex_pool))
  if (length(missing)) {
    stop(
      "template references unknown regex name(s): ",
      paste(missing, collapse = ", "),
      "\n  template: ",
      template
    )
  }

  capture_names <- paste0("ftcap", seq_along(ph))
  names(capture_names) <- ph

  compiled_parts <- character(length(tokens$type))
  for (i in seq_along(tokens$type)) {
    if (identical(tokens$type[[i]], "literal")) {
      compiled_parts[[i]] <- .ft_escape_regex(tokens$value[[i]])
      next
    }

    nm <- tokens$value[[i]]
    rx <- .ft_expand_pool_regex(nm, regex_pool)
    # Rewrite anchors so nested regexes still respect component boundaries when
    # embedded into a complete dirname or filename template.
    rx <- .ft_rewrite_pool_anchors(rx)
    compiled_parts[[i]] <- paste0(
      "(?<",
      capture_names[[nm]],
      ">(?:",
      rx,
      "))"
    )
  }

  compiled <- paste0(compiled_parts, collapse = "")
  compiled <- paste0("^", compiled, "$")
  attr(compiled, "capture_names") <- stats::setNames(
    names(capture_names),
    capture_names
  )
  compiled
}

.ft_template_tokens <- function(template) {
  n <- nchar(template)
  type <- character()
  value <- character()

  add_token <- function(token_type, token_value) {
    type <<- c(type, token_type)
    value <<- c(value, token_value)
  }

  i <- 1L
  while (i <= n) {
    two <- substr(template, i, min(i + 1L, n))
    if (identical(two, "{{")) {
      add_token("literal", "{")
      i <- i + 2L
      next
    }
    if (identical(two, "}}")) {
      add_token("literal", "}")
      i <- i + 2L
      next
    }

    ch <- substr(template, i, i)
    if (identical(ch, "{")) {
      rest <- substr(template, i + 1L, n)
      end <- regexpr("}", rest, fixed = TRUE)[[1]]
      if (end < 0) {
        .ft_abort_arg(
          "template",
          "contains an opening `{` without a closing `}`."
        )
      }

      name <- substr(rest, 1L, end - 1L)
      if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) {
        .ft_abort_arg(
          "template",
          "placeholders must look like `{name}`."
        )
      }
      add_token("field", name)
      i <- i + end + 1L
      next
    }
    if (identical(ch, "}")) {
      .ft_abort_arg(
        "template",
        "contains a closing `}` without an opening `{`."
      )
    }

    rest <- substr(template, i, n)
    next_special <- regexpr("[{}]", rest)[[1]]
    if (next_special < 0) {
      add_token("literal", rest)
      i <- n + 1L
    } else {
      literal <- substr(rest, 1L, next_special - 1L)
      add_token("literal", literal)
      i <- i + next_special - 1L
    }
  }

  data.frame(type = type, value = value, stringsAsFactors = FALSE)
}

.ft_template_placeholders <- function(template) {
  tokens <- .ft_template_tokens(template)
  unique(tokens$value[tokens$type == "field"])
}

.ft_escape_regex <- function(x) {
  stringr::str_replace_all(
    x,
    "([\\\\.\\^$|?*+(){}\\[\\]])",
    "\\\\\\1"
  )
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

.ft_normalize_templates <- function(template) {
  if (
    !is.character(template) ||
      length(template) < 1 ||
      any(is.na(template)) ||
      !all(nzchar(template))
  ) {
    .ft_abort_arg("template", "must be a non-empty character vector.")
  }
  if (length(template) == 1 && is.null(names(template))) {
    names(template) <- "default"
  }
  if (
    is.null(names(template)) ||
      any(is.na(names(template))) ||
      !all(nzchar(names(template)))
  ) {
    .ft_abort_arg(
      "template",
      "must be named, unless it is a single unnamed template."
    )
  }
  template
}

.ft_recompile_templates <- function(ft) {
  for (layer in names(ft$dir_templates)) {
    spec <- ft$dir_templates[[layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }
    if (is.null(spec$when)) {
      spec$when <- rep(
        list(rlang::set_names(character(), character())),
        length(spec$raw)
      )
      names(spec$when) <- names(spec$raw)
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
    compiled <- Map(.ft_compile_template, spec$raw, regex_pool)
    ft$dir_templates[[layer]]$compiled <- compiled
    ft$dir_templates[[layer]]$when <- spec$when
    ft$dir_templates[[layer]]$with <- spec$with
    ft$dir_templates[[layer]]$regex_pool <- regex_pool
  }

  for (at_layer in names(ft$file_templates)) {
    spec <- ft$file_templates[[at_layer]]
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
    compiled <- Map(.ft_compile_template, spec$raw, regex_pool)
    ft$file_templates[[at_layer]]$compiled <- compiled
    ft$file_templates[[at_layer]]$regex_pool <- regex_pool
  }

  for (layer in names(ft$ignore_dir_templates)) {
    spec <- ft$ignore_dir_templates[[layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }
    if (is.null(spec$when)) {
      spec$when <- rep(
        list(rlang::set_names(character(), character())),
        length(spec$raw)
      )
      names(spec$when) <- names(spec$raw)
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
    compiled <- Map(.ft_compile_template, spec$raw, regex_pool)
    ft$ignore_dir_templates[[layer]]$compiled <- compiled
    ft$ignore_dir_templates[[layer]]$when <- spec$when
    ft$ignore_dir_templates[[layer]]$with <- spec$with
    ft$ignore_dir_templates[[layer]]$regex_pool <- regex_pool
  }

  for (at_layer in names(ft$ignore_file_templates)) {
    spec <- ft$ignore_file_templates[[at_layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }
    if (is.null(spec$when)) {
      spec$when <- rep(
        list(rlang::set_names(character(), character())),
        length(spec$raw)
      )
      names(spec$when) <- names(spec$raw)
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
    compiled <- Map(.ft_compile_template, spec$raw, regex_pool)
    ft$ignore_file_templates[[at_layer]]$compiled <- compiled
    ft$ignore_file_templates[[at_layer]]$when <- spec$when
    ft$ignore_file_templates[[at_layer]]$with <- spec$with
    ft$ignore_file_templates[[at_layer]]$regex_pool <- regex_pool
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
  if (
    is.null(names(when)) ||
      any(is.na(names(when))) ||
      !all(nzchar(names(when)))
  ) {
    .ft_abort_arg("when", "must be named.")
  }
  if (is.character(when)) {
    if (any(is.na(when)) || !all(nzchar(when))) {
      .ft_abort_arg("when", "values must be non-empty character vectors.")
    }
    when <- as.list(when)
  } else {
    if (!is.list(when)) {
      .ft_abort_arg(
        "when",
        "must be a named character vector or named list of character vectors."
      )
    }
    if (!all(vapply(when, is.character, logical(1)))) {
      .ft_abort_arg("when", "values must be non-empty character vectors.")
    }
    valid_values <- all(vapply(
      when,
      function(x) length(x) > 0 && !any(is.na(x)) && all(nzchar(x)),
      logical(1)
    ))
    if (!valid_values) {
      .ft_abort_arg("when", "values must be non-empty character vectors.")
    }
  }
  when
}

.ft_normalize_local_regex <- function(with) {
  if (is.null(with)) {
    return(rlang::set_names(character(), character()))
  }
  if (
    !is.character(with) ||
      length(with) < 1 ||
      is.null(names(with)) ||
      any(is.na(names(with))) ||
      !all(nzchar(names(with)))
  ) {
    .ft_abort_arg("with", "must be a named character vector.")
  }
  if (any(is.na(with)) || !all(nzchar(with))) {
    .ft_abort_arg("with", "values must be non-empty strings.")
  }
  with
}

.ft_unique_template_names <- function(new_names, existing_names = character()) {
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

.ft_add_template_specs <- function(
  existing,
  templates,
  compiled,
  when,
  with,
  regex_pool,
  base_regex_pool = regex_pool
) {
  when_list <- rep(list(when), length(templates))
  names(when_list) <- names(templates)
  regex_pool_list <- rep(list(regex_pool), length(templates))
  names(regex_pool_list) <- names(templates)
  with_list <- rep(list(with), length(templates))
  names(with_list) <- names(templates)

  if (is.null(existing) || length(existing) == 0) {
    return(list(
      raw = templates,
      compiled = compiled,
      when = when_list,
      with = with_list,
      regex_pool = regex_pool_list
    ))
  }

  existing_when <- existing$when
  if (is.null(existing_when)) {
    existing_when <- rep(
      list(rlang::set_names(character(), character())),
      length(existing$raw)
    )
    names(existing_when) <- names(existing$raw)
  }
  existing_with <- existing$with
  if (is.null(existing_with)) {
    existing_with <- rep(
      list(rlang::set_names(character(), character())),
      length(existing$raw)
    )
    names(existing_with) <- names(existing$raw)
  }
  existing_regex_pool <- existing$regex_pool
  if (is.null(existing_regex_pool)) {
    existing_regex_pool <- rep(list(base_regex_pool), length(existing$raw))
    names(existing_regex_pool) <- names(existing$raw)
  }

  out <- list(
    raw = c(existing$raw, templates),
    compiled = c(existing$compiled, compiled),
    when = c(existing_when, when_list),
    with = c(existing_with, with_list),
    regex_pool = c(existing_regex_pool, regex_pool_list)
  )

  duplicate <- duplicated(names(out$raw), fromLast = TRUE)
  lapply(out, function(x) x[!duplicate])
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

.ft_file_template_matches <- function(file_name, row, spec) {
  if (is.null(spec) || length(spec) == 0) {
    return(FALSE)
  }

  row_tbl <- row[1, , drop = FALSE]
  for (template_i in seq_along(spec$compiled)) {
    when <- spec$when[[template_i]]
    if (!.ft_when_matches(row_tbl, when)) {
      next
    }

    m <- stringr::str_match(file_name, spec$compiled[[template_i]])
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
  layers <- ft$layers
  parent_layers <- layers[-length(layers)]
  has_parent_file_templates <- any(vapply(
    parent_layers,
    .ft_has_file_templates,
    logical(1),
    ft = ft
  ))
  if (!has_parent_file_templates) {
    return(tbl)
  }

  layer_cols <- paste0("layer__", layers)
  dir_layer_cols <- layer_cols[-length(layer_cols)]
  fname <- tbl[[layer_cols[[length(layer_cols)]]]]
  n_dirs <- if (length(dir_layer_cols)) {
    rowSums(!is.na(tbl[, dir_layer_cols, drop = FALSE]))
  } else {
    rep(0L, nrow(tbl))
  }

  parent_has_templates <- rep(FALSE, nrow(tbl))
  candidate_n_dir <- which(n_dirs >= 1L & n_dirs < length(layers))
  if (length(candidate_n_dir)) {
    parent_has_templates[candidate_n_dir] <- vapply(
      layers[n_dirs[candidate_n_dir]],
      .ft_has_file_templates,
      logical(1),
      ft = ft
    )
  }

  for (j in which(active & parent_has_templates)) {
    n_dir <- n_dirs[[j]]
    candidates <- .ft_candidate_file_layers(ft, n_dir)
    if (!length(candidates)) {
      next
    }

    fallback_layer <- NA_character_
    for (layer in candidates) {
      spec <- ft$file_templates[[layer]]
      if (is.na(fallback_layer) && .ft_has_file_templates(ft, layer)) {
        fallback_layer <- layer
      }
      if (.ft_file_template_matches(fname[[j]], tbl[j, , drop = FALSE], spec)) {
        tbl$at_layer[[j]] <- layer
        break
      }
    }
    if (
      !is.na(fallback_layer) && !.ft_has_file_templates(ft, tbl$at_layer[[j]])
    ) {
      tbl$at_layer[[j]] <- fallback_layer
    }
  }

  tbl
}

# ---- directory templates ----
# templates validate (and may extract from) the directory name at `layer`

#' Register directory name templates for a layer
#'
#' Compile named component templates that validate and extract captures from
#' complete directory names at a given layer in the tree. Fixed text in a
#' template is matched literally and each template must match the full directory
#' name. For example, `day{time}` matches `day01` when `time = "\\d{2}"`, but
#' not `day01b`.
#'
#' @param ft A `filetree` object.
#' @param layer Directory layer name (must be one of the non-file layers).
#' @param template Named character vector of full-string component templates
#'   using fixed text and `{placeholder}` references that point into `ft`'s
#'   `regex_pool`.
#' @param when Optional named character vector or named list of exact-match
#'   conditions. A conditional directory template is applied only when every
#'   condition matches an already extracted field or raw layer value with the
#'   same name. Use a list when a condition can match any of several values.
#' @param with Optional named character vector of template-local regex
#'   definitions. These definitions override `ft`'s regex pool for this
#'   directory template only.
#' @return The updated `filetree` object.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}"
#'   )) |>
#'   ft_add_dir_template("subject", "{subject}") |>
#'   ft_add_dir_template("time", "{time}")
#'
#' ft
#' @export
ft_add_dir_template <- function(
  ft,
  layer,
  template,
  when = NULL,
  with = NULL
) {
  .ft_check_filetree(ft)
  if (
    !is.character(layer) ||
      length(layer) != 1 ||
      is.na(layer) ||
      !nzchar(layer) ||
      !layer %in% names(ft$dir_templates)
  ) {
    .ft_abort_arg(
      "layer",
      paste0(
        "must be one of the directory layers: ",
        .ft_collapse_or(names(ft$dir_templates)),
        "."
      )
    )
  }

  templates <- .ft_normalize_templates(template)
  when <- .ft_normalize_when(when)
  with <- .ft_normalize_local_regex(with)
  regex_pool <- .ft_merge_regex_pool(with, ft$regex_pool)
  compiled <- lapply(templates, .ft_compile_template, regex_pool = regex_pool)

  existing <- ft$dir_templates[[layer]]
  ft$dir_templates[[layer]] <- .ft_add_template_specs(
    existing,
    templates,
    compiled,
    when,
    with,
    regex_pool,
    base_regex_pool = ft$regex_pool
  )
  ft
}

# ---- file templates ----
# templates validate (and may extract from) the file name at `at_layer`

#' Register file-name templates for a layer
#'
#' Compile named component templates that validate and extract captures from
#' complete file names for files that belong to a specific layer. Fixed text in
#' a template is matched literally and each template must match the full file
#' name. For example, `{subject}_{task}.txt` treats `.txt` as a literal
#' extension, not a regular expression. File templates may be registered on any
#' configured layer, including non-terminal directory layers, so sidecar files
#' such as subject-level manifests can live beside child directories.
#'
#' @param ft A `filetree` object.
#' @param layer Layer at which the files live (must be listed in `ft$layers`).
#' @param template Named character vector of full-string file-name component
#'   templates using fixed text and `{placeholder}` references tied to `ft`'s
#'   regex pool.
#' @param when Optional named character vector or named list of exact-match
#'   conditions. A conditional file template is applied only when every condition
#'   matches an extracted field or raw layer value with the same name. Use a
#'   list when a condition can match any of several values.
#' @param with Optional named character vector of template-local regex
#'   definitions. These definitions override `ft`'s regex pool for this file
#'   template only.
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
#'   ft_add_file_template(
#'     "data",
#'     c(txt = "{subject}_{task}.txt"),
#'     when = list(time = c("day01", "day02"))
#'   ) |>
#'   ft_add_file_template(
#'     "data",
#'     c(wav = "{subject}_{task}.wav"),
#'     when = c(time = "day03"),
#'     with = c(task = "yellow")
#'   )
#'
#' ft
#' @export
ft_add_file_template <- function(
  ft,
  layer,
  template,
  when = NULL,
  with = NULL
) {
  .ft_check_filetree(ft)
  if (
    !is.character(layer) ||
      length(layer) != 1 ||
      is.na(layer) ||
      !nzchar(layer) ||
      !layer %in% ft$layers
  ) {
    .ft_abort_arg(
      "layer",
      paste0(
        "must be one of the configured layers: ",
        .ft_collapse_or(ft$layers),
        "."
      )
    )
  }

  templates <- .ft_normalize_templates(template)
  when <- .ft_normalize_when(when)
  with <- .ft_normalize_local_regex(with)
  regex_pool <- .ft_merge_regex_pool(with, ft$regex_pool)
  compiled <- lapply(templates, .ft_compile_template, regex_pool = regex_pool)

  existing <- ft$file_templates[[layer]]
  if (!(is.null(existing) || length(existing) == 0)) {
    names(templates) <- .ft_unique_template_names(
      names(templates),
      names(existing$raw)
    )
    names(compiled) <- names(templates)
  }
  ft$file_templates[[layer]] <- .ft_add_template_specs(
    existing,
    templates,
    compiled,
    when,
    with,
    regex_pool,
    base_regex_pool = ft$regex_pool
  )
  ft
}

# ---- ignore templates ----

#' Register ignored directory templates for a layer
#'
#' Compile named component templates that identify directory subtrees to ignore.
#' When a directory component matches an ignored directory template, files below
#' that directory are excluded from [ft_list()] and [ft_index()] unless
#' `include_ignored = TRUE`.
#'
#' @param ft A `filetree` object.
#' @param layer Directory layer name (must be one of the non-file layers).
#' @param template Named character vector of full-string component templates.
#' @param when Optional named character vector or named list of exact-match
#'   conditions, as in [ft_add_dir_template()].
#' @param with Optional named character vector of template-local regex
#'   definitions, as in [ft_add_dir_template()].
#' @return The updated `filetree` object.
#' @export
ft_ignore_dir_template <- function(
  ft,
  layer,
  template,
  when = NULL,
  with = NULL
) {
  .ft_check_filetree(ft)
  if (
    !is.character(layer) ||
      length(layer) != 1 ||
      is.na(layer) ||
      !nzchar(layer) ||
      !layer %in% names(ft$ignore_dir_templates)
  ) {
    .ft_abort_arg(
      "layer",
      paste0(
        "must be one of the directory layers: ",
        .ft_collapse_or(names(ft$ignore_dir_templates)),
        "."
      )
    )
  }

  templates <- .ft_normalize_templates(template)
  when <- .ft_normalize_when(when)
  with <- .ft_normalize_local_regex(with)
  regex_pool <- .ft_merge_regex_pool(with, ft$regex_pool)
  compiled <- lapply(templates, .ft_compile_template, regex_pool = regex_pool)

  existing <- ft$ignore_dir_templates[[layer]]
  ft$ignore_dir_templates[[layer]] <- .ft_add_template_specs(
    existing,
    templates,
    compiled,
    when,
    with,
    regex_pool,
    base_regex_pool = ft$regex_pool
  )
  ft
}

#' Register ignored file-name templates for a layer
#'
#' Compile named component templates that identify files to ignore. Ignored
#' files are excluded from [ft_list()] and [ft_index()] unless
#' `include_ignored = TRUE`.
#'
#' @param ft A `filetree` object.
#' @param layer Layer at which the files live (must be listed in `ft$layers`).
#' @param template Named character vector of full-string file-name component
#'   templates.
#' @param when Optional named character vector or named list of exact-match
#'   conditions, as in [ft_add_file_template()].
#' @param with Optional named character vector of template-local regex
#'   definitions, as in [ft_add_file_template()].
#' @return The updated `filetree` object.
#' @export
ft_ignore_file_template <- function(
  ft,
  layer,
  template,
  when = NULL,
  with = NULL
) {
  .ft_check_filetree(ft)
  if (
    !is.character(layer) ||
      length(layer) != 1 ||
      is.na(layer) ||
      !nzchar(layer) ||
      !layer %in% ft$layers
  ) {
    .ft_abort_arg(
      "layer",
      paste0(
        "must be one of the configured layers: ",
        .ft_collapse_or(ft$layers),
        "."
      )
    )
  }

  templates <- .ft_normalize_templates(template)
  when <- .ft_normalize_when(when)
  with <- .ft_normalize_local_regex(with)
  regex_pool <- .ft_merge_regex_pool(with, ft$regex_pool)
  compiled <- lapply(templates, .ft_compile_template, regex_pool = regex_pool)

  existing <- ft$ignore_file_templates[[layer]]
  if (!(is.null(existing) || length(existing) == 0)) {
    names(templates) <- .ft_unique_template_names(
      names(templates),
      names(existing$raw)
    )
    names(compiled) <- names(templates)
  }
  ft$ignore_file_templates[[layer]] <- .ft_add_template_specs(
    existing,
    templates,
    compiled,
    when,
    with,
    regex_pool,
    base_regex_pool = ft$regex_pool
  )
  ft
}

.ft_has_ignore_templates <- function(ft) {
  has_dir <- any(vapply(
    ft$ignore_dir_templates,
    function(x) !(is.null(x) || length(x) == 0),
    logical(1)
  ))
  has_file <- any(vapply(
    ft$ignore_file_templates,
    function(x) !(is.null(x) || length(x) == 0),
    logical(1)
  ))
  has_dir || has_file
}

.ft_path_table <- function(ft, files) {
  path_info <- .ft_path_rel(files, ft$root)
  rel <- path_info$rel
  parts_list <- strsplit(rel, "/", fixed = TRUE)

  layers <- ft$layers
  dir_layers <- if (length(layers) >= 2) {
    layers[-length(layers)]
  } else {
    character()
  }
  layer_cols <- paste0("layer__", layers)

  layer_mat <- matrix(
    NA_character_,
    nrow = length(parts_list),
    ncol = length(layers)
  )
  colnames(layer_mat) <- layer_cols

  at_layer <- character(length(parts_list))
  for (i in seq_along(parts_list)) {
    parts <- parts_list[[i]]
    n_parts <- length(parts)
    fname <- parts[[n_parts]]

    n_dir <- max(n_parts - 1L, 0L)
    if (n_dir > 0L && length(dir_layers) > 0L) {
      n_fill <- min(n_dir, length(dir_layers))
      layer_mat[i, seq_len(n_fill)] <- parts[seq_len(n_fill)]
    }
    layer_mat[i, length(layers)] <- fname

    idx <- n_dir + 1L
    at_layer[[i]] <- if (idx > length(layers)) {
      ".__too_deep__"
    } else {
      layers[[idx]]
    }
  }

  tbl <- tibble::tibble(
    .path = files,
    .rel = rel,
    at_layer = at_layer
  ) |>
    dplyr::bind_cols(tibble::as_tibble(layer_mat))

  list(
    tbl = tbl,
    path_info = path_info,
    parts_list = parts_list,
    dir_layers = dir_layers,
    layer_cols = layer_cols
  )
}

.ft_add_placeholder_columns <- function(tbl, placeholders) {
  for (nm in placeholders) {
    if (is.null(tbl[[nm]])) {
      tbl[[nm]] <- NA_character_
    }
  }
  tbl
}

.ft_apply_dir_captures_for_matching <- function(tbl, ft, active) {
  dir_layers <- ft$layers[-length(ft$layers)]
  placeholders <- .ft_all_placeholder_names(ft)
  tbl <- .ft_add_placeholder_columns(tbl, placeholders)

  for (layer in dir_layers) {
    raw_vals <- tbl[[paste0("layer__", layer)]]
    spec <- ft$dir_templates[[layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }

    layer_active <- active & !is.na(raw_vals)
    if (!any(layer_active)) {
      next
    }

    matched <- rep(FALSE, nrow(tbl))
    for (template_i in seq_along(spec$compiled)) {
      when <- spec$when[[template_i]]
      template_rows <- layer_active & .ft_when_matches(tbl, when)
      if (!any(template_rows)) {
        next
      }

      m <- stringr::str_match(raw_vals, spec$compiled[[template_i]])
      m <- .ft_restore_capture_names(m, spec$compiled[[template_i]])
      ok <- template_rows & !is.na(m[, 1]) & !matched
      if (!any(ok)) {
        next
      }

      cap_names <- setdiff(colnames(m), "")
      for (cn in cap_names) {
        if (is.null(tbl[[cn]])) {
          tbl[[cn]] <- NA_character_
        }
        vals <- m[, cn]
        replace_idx <- ok & !is.na(vals) & is.na(tbl[[cn]])
        tbl[[cn]][replace_idx] <- as.character(unname(vals[replace_idx]))
      }
      matched <- matched | ok
    }
  }

  tbl
}

.ft_candidate_file_layers_for_row <- function(ft, tbl, row) {
  dir_layer_cols <- paste0("layer__", ft$layers[-length(ft$layers)])
  n_dir <- if (length(dir_layer_cols)) {
    sum(!is.na(tbl[row, dir_layer_cols, drop = TRUE]))
  } else {
    0L
  }
  .ft_candidate_file_layers(ft, n_dir)
}

.ft_classify_ignored <- function(tbl, ft, active) {
  n <- nrow(tbl)
  ignored <- rep(FALSE, n)
  ignore_template <- rep(NA_character_, n)
  ignore_type <- rep(NA_character_, n)

  if (!.ft_has_ignore_templates(ft)) {
    return(list(
      ignored = ignored,
      ignore_template = ignore_template,
      ignore_type = ignore_type
    ))
  }

  match_tbl <- .ft_apply_dir_captures_for_matching(tbl, ft, active)

  for (layer in names(ft$ignore_dir_templates)) {
    spec <- ft$ignore_dir_templates[[layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }

    raw_vals <- match_tbl[[paste0("layer__", layer)]]
    layer_rows <- active & !ignored & !is.na(raw_vals)
    if (!any(layer_rows)) {
      next
    }

    for (template_i in seq_along(spec$compiled)) {
      template_rows <- layer_rows & .ft_when_matches(match_tbl, spec$when[[template_i]])
      if (!any(template_rows)) {
        next
      }

      m <- stringr::str_match(raw_vals, spec$compiled[[template_i]])
      ok <- template_rows & !is.na(m[, 1]) & !ignored
      if (!any(ok)) {
        next
      }

      ignored[ok] <- TRUE
      ignore_template[ok] <- names(spec$compiled)[[template_i]]
      ignore_type[ok] <- "dir"
    }
  }

  file_name <- match_tbl[[paste0("layer__", ft$layers[[length(ft$layers)]])]]
  for (row in which(active & !ignored)) {
    candidates <- .ft_candidate_file_layers_for_row(ft, match_tbl, row)
    for (layer in candidates) {
      spec <- ft$ignore_file_templates[[layer]]
      if (is.null(spec) || length(spec) == 0) {
        next
      }

      row_tbl <- match_tbl[row, , drop = FALSE]
      for (template_i in seq_along(spec$compiled)) {
        if (!.ft_when_matches(row_tbl, spec$when[[template_i]])) {
          next
        }

        m <- stringr::str_match(file_name[[row]], spec$compiled[[template_i]])
        if (is.na(m[, 1])) {
          next
        }

        ignored[[row]] <- TRUE
        ignore_template[[row]] <- names(spec$compiled)[[template_i]]
        ignore_type[[row]] <- "file"
        break
      }
      if (ignored[[row]]) {
        break
      }
    }
  }

  list(
    ignored = ignored,
    ignore_template = ignore_template,
    ignore_type = ignore_type
  )
}

# ---- file enumeration ----

#' List files under the filetree root
#'
#' Return all files under the filetree root using the configured `fs` helper.
#'
#' @param ft A `filetree` object.
#' @param include_ignored Logical. If `TRUE`, return files that match ignored
#'   file or directory templates. If `FALSE`, ignored files are pruned.
#' @return An `fs_path` character vector of file paths under `ft$root`.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data"))
#'
#' head(ft_list(ft))
#' @export
ft_list <- function(ft, include_ignored = FALSE) {
  .ft_check_filetree(ft)
  if (
    !is.logical(include_ignored) ||
      length(include_ignored) != 1 ||
      is.na(include_ignored)
  ) {
    .ft_abort_arg("include_ignored", "must be `TRUE` or `FALSE`.")
  }

  files <- fs::dir_ls(ft$root, recurse = TRUE, type = "file")
  if (include_ignored || !length(files) || !.ft_has_ignore_templates(ft)) {
    return(files)
  }

  path_data <- .ft_path_table(ft, files)
  path_info <- path_data$path_info
  active <- path_info$under_root &
    !path_info$at_root &
    path_data$tbl$at_layer != ".__too_deep__"
  ignore <- .ft_classify_ignored(path_data$tbl, ft, active)
  files[!ignore$ignored]
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

  # dir templates
  for (layer in names(ft$dir_templates)) {
    spec <- ft$dir_templates[[layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }
    out <- c(
      out,
      unlist(lapply(spec$raw, .ft_template_placeholders), use.names = FALSE)
    )
  }

  # file templates
  for (at_layer in names(ft$file_templates)) {
    spec <- ft$file_templates[[at_layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }
    out <- c(
      out,
      unlist(lapply(spec$raw, .ft_template_placeholders), use.names = FALSE)
    )
  }

  unique(out)
}

#' Index files against a filetree specification
#'
#' Validate file paths against the configured directory and file-name templates,
#' extract placeholder captures, and report any problems. File templates
#' registered on parent layers are considered for sidecar files before
#' unmatched files are reported.
#'
#' @param ft A `filetree` object.
#' @param files Optional character vector of file paths to check. Defaults to
#'   all files under `ft$root` via [ft_list()].
#' @param strict Logical. If `TRUE`, files at layers without registered file
#'   templates are reported as problems. If `FALSE`, missing file templates are
#'   accepted so partial schemas can be used for exploratory indexing.
#' @param include_ignored Logical. If `TRUE`, include ignored files as inert
#'   audit rows. If `FALSE`, ignored files are pruned before validation.
#' @return A tibble with `.path`, `.rel`, `at_layer`, layer columns
#'   (`layer__<name>`), captured placeholders, the matched template name, `.ok`
#'   flag, and `.problems` list-column.
#' @examples
#' root <- system.file("demo-1", package = "filetree")
#'
#' ft <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}",
#'     task = "red|green"
#'   )) |>
#'   ft_add_dir_template("subject", "{subject}") |>
#'   ft_add_dir_template("time", "{time}") |>
#'   ft_add_file_template("data", "{subject}_{task}.txt")
#'
#' ft_index(ft)
#'
#' ft_partial <- ft_init(root, c("subject", "time", "data")) |>
#'   ft_add_regex(c(
#'     subject = "\\w{2}-\\d{2}",
#'     time = "day\\d{2}"
#'   )) |>
#'   ft_add_dir_template("subject", "{subject}") |>
#'   ft_add_dir_template("time", "{time}")
#'
#' ft_index(ft_partial)
#' ft_index(ft_partial, strict = TRUE)
#' @export
ft_index <- function(
  ft,
  files = ft_list(ft, include_ignored = include_ignored),
  strict = FALSE,
  include_ignored = FALSE
) {
  .ft_check_filetree(ft)
  if (!is.logical(strict) || length(strict) != 1 || is.na(strict)) {
    .ft_abort_arg("strict", "must be `TRUE` or `FALSE`.")
  }
  if (
    !is.logical(include_ignored) ||
      length(include_ignored) != 1 ||
      is.na(include_ignored)
  ) {
    .ft_abort_arg("include_ignored", "must be `TRUE` or `FALSE`.")
  }

  path_data <- .ft_path_table(ft, files)
  path_info <- path_data$path_info
  outside_root <- !path_info$under_root
  at_or_above_root <- path_info$at_root | outside_root

  layers <- ft$layers
  dir_layers <- path_data$dir_layers
  file_layer <- layers[length(layers)]

  layer_cols <- path_data$layer_cols
  tbl <- path_data$tbl

  # extracted fields = placeholders not in layers (but captures may include layer names too;
  # those should become extracted fields columns, not collide with layer__ columns)
  all_placeholders <- .ft_all_placeholder_names(ft)

  # create columns for ALL placeholders (speaker/visit/task/item/etc.)
  # even if a placeholder name equals a layer name, it is still an extracted field column,
  # because the raw layer component is stored in layer__<layer>.
  if (length(all_placeholders)) {
    tbl <- .ft_add_placeholder_columns(tbl, all_placeholders)
  }

  n <- nrow(tbl)
  matched_template <- rep(NA_character_, n)
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
  bad_root <- is.na(tbl$at_layer) | at_or_above_root
  too_deep <- tbl$at_layer == ".__too_deep__" & !bad_root
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

  ignore <- .ft_classify_ignored(tbl, ft, active)
  if (!include_ignored && any(ignore$ignored)) {
    keep <- !ignore$ignored
    tbl <- tbl[keep, , drop = FALSE]
    at_or_above_root <- at_or_above_root[keep]
    problems <- problems[keep]
    matched_template <- matched_template[keep]
    active <- active[keep]
    n <- nrow(tbl)
  } else if (include_ignored) {
    tbl$.ignored <- ignore$ignored
    tbl$.ignore_template <- ignore$ignore_template
    tbl$.ignore_type <- ignore$ignore_type
    active <- active & !tbl$.ignored
  }

  # ---- validate / extract from directory names (dir_layers only) ----
  for (layer in dir_layers) {
    raw_vals <- tbl[[paste0("layer__", layer)]]
    spec <- ft$dir_templates[[layer]]
    if (is.null(spec) || length(spec) == 0) {
      next
    }

    layer_active <- active & !is.na(raw_vals)
    if (!any(layer_active)) {
      next
    }

    matched <- rep(FALSE, n)
    for (template_i in seq_along(spec$compiled)) {
      when <- spec$when[[template_i]]
      template_rows <- layer_active & .ft_when_matches(tbl, when)
      if (!any(template_rows)) {
        next
      }

      m <- stringr::str_match(raw_vals, spec$compiled[[template_i]])
      m <- .ft_restore_capture_names(m, spec$compiled[[template_i]])
      ok <- template_rows & !is.na(m[, 1]) & !matched
      if (!any(ok)) {
        next
      }

      cap_names <- setdiff(colnames(m), "")
      regex_pool <- spec$regex_pool[[template_i]]
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
            "directory name '%s' does not match a dir template at layer `%s`",
            raw_vals[[j]],
            layer
          )
        )
      }
    }
  }

  tbl <- .ft_resolve_file_layers(tbl, ft, active)

  # ---- validate / extract from file name via templates at at_layer ----
  fname <- tbl[[paste0("layer__", file_layer)]]
  for (layer in names(ft$file_templates)) {
    spec <- ft$file_templates[[layer]]
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
            "no file templates registered for `%s` files",
            tbl$at_layer[[j]]
          )
        )
      }
      next
    }

    matched <- rep(FALSE, n)
    applicable <- rep(FALSE, n)
    for (template_i in seq_along(spec$compiled)) {
      template_name <- names(spec$compiled)[[template_i]]
      when <- spec$when[[template_i]]
      template_rows <- layer_rows & .ft_when_matches(tbl, when)
      applicable <- applicable | template_rows
      if (!any(template_rows)) {
        next
      }

      m <- stringr::str_match(fname, spec$compiled[[template_i]])
      m <- .ft_restore_capture_names(m, spec$compiled[[template_i]])
      ok <- template_rows & !is.na(m[, 1]) & !matched
      if (!any(ok)) {
        next
      }

      matched_template[ok] <- template_name
      cap_names <- setdiff(colnames(m), "")
      regex_pool <- spec$regex_pool[[template_i]]
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
            "filename '%s' does not match an applicable file template at layer `%s`",
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
            "filename '%s' does not match a file template at layer `%s`",
            fname[[j]],
            tbl$at_layer[[j]]
          )
        )
      }
    }
  }

  tbl$template <- matched_template
  tbl$.problems <- problems
  tbl$.ok <- lengths(problems) == 0

  # order columns: raw layer__ columns, then extracted fields, then diagnostics
  core <- c(".path", ".rel", "at_layer", layer_cols)
  diag <- c("template")
  if (include_ignored) {
    diag <- c(diag, ".ignored", ".ignore_template", ".ignore_type")
  }
  diag <- c(diag, ".ok", ".problems")
  extracted <- setdiff(names(tbl), c(core, diag))
  tbl <- tbl[, c(core, extracted, diag)]

  tbl
}

.ft_path_rel <- function(files, root) {
  files_abs <- fs::path_abs(files)
  root_abs <- fs::path_abs(root)

  files_chr <- chartr("\\", "/", as.character(files_abs))
  root_chr <- chartr("\\", "/", as.character(root_abs))
  root_chr <- sub("/+$", "", root_chr)
  prefix <- paste0(root_chr, "/")

  under_root <- startsWith(files_chr, prefix)
  at_root <- files_chr == root_chr
  if (all(under_root)) {
    return(list(
      rel = substring(files_chr, nchar(prefix) + 1L),
      under_root = under_root,
      at_root = at_root
    ))
  }

  list(
    rel = as.character(fs::path_rel(files_abs, start = root_abs)),
    under_root = under_root,
    at_root = at_root
  )
}

#' Glimpse filetree indexing problems
#'
#' Print a compact summary of problem files and their problem messages. Accepts
#' either a `filetree` object or the tibble returned by [ft_index()].
#'
#' @param x A `filetree` object or an index tibble returned by [ft_index()].
#' @param n Maximum number of problem batches to print. Batches group problem
#'   files by `at_layer` and parent directory.
#' @param n_lines Maximum number of problem lines to print in each batch. When
#'   the hidden remainder would be less than 20% of the batch, all lines are
#'   printed.
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
#'   ft_add_dir_template("subject", "{subject}") |>
#'   ft_add_dir_template("time", "{time}") |>
#'   ft_add_file_template("data", "{subject}_{task}.txt")
#'
#' ft_glimpse_problems(ft, n = 3)
#' @export
ft_glimpse_problems <- function(x, n = 10, n_lines = 10, ...) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0) {
    .ft_abort_arg("n", "must be a non-negative number.")
  }
  if (
    !is.numeric(n_lines) ||
      length(n_lines) != 1 ||
      is.na(n_lines) ||
      n_lines < 0
  ) {
    .ft_abort_arg("n_lines", "must be a non-negative number.")
  }
  n <- as.integer(n)
  n_lines <- as.integer(n_lines)

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

  batches <- .ft_problem_batch_index(problem_rows)
  total_batches <- batches$total
  shown <- .ft_preview_count(total_batches, n)
  if (total_batches > shown) {
    cat(sprintf("Showing %d of %d problem batches.\n", shown, total_batches))
  }

  if (shown > 0) {
    for (i in seq_len(shown)) {
      batch <- .ft_problem_batch(problem_rows, batches, i)
      cat("\n")
      cat(batch$label, "\n", sep = "")

      shown_lines <- .ft_preview_count(batch$total_lines, n_lines)
      if (shown_lines > 0) {
        lines <- .ft_problem_batch_lines(problem_rows, batch, shown_lines)
        for (j in seq_along(lines$name)) {
          cli::cli_bullets(c(
            "*" = paste0(lines$name[[j]], ": ", lines$problem[[j]])
          ))
        }
      }

      hidden <- batch$total_lines - shown_lines
      if (hidden > 0) {
        cat(sprintf("[... %d more problems]\n", hidden))
      }
    }
  }

  invisible(problem_rows)
}

.ft_preview_count <- function(total, n) {
  shown <- min(total, n)
  hidden <- total - shown
  if (hidden > 0 && hidden < total * 0.2) {
    return(total)
  }
  shown
}

.ft_problem_batch_index <- function(problem_rows) {
  if (nrow(problem_rows) == 0) {
    return(list(total = 0L))
  }

  at_layer <- if ("at_layer" %in% names(problem_rows)) {
    problem_rows$at_layer
  } else {
    rep(NA_character_, nrow(problem_rows))
  }
  parent <- .ft_problem_parent_dir(problem_rows$.rel)
  key <- paste(at_layer, parent, sep = "\r")
  unique_key <- unique(key)
  group <- match(key, unique_key)
  first <- match(unique_key, key)

  list(
    total = length(unique_key),
    group = group,
    at_layer = at_layer,
    parent = parent,
    first = first
  )
}

.ft_problem_batch <- function(problem_rows, batches, i) {
  idx <- which(batches$group == i)
  first <- batches$first[[i]]
  layer <- batches$at_layer[[first]]
  if (is.na(layer) || !nzchar(layer)) {
    layer <- "<unknown>"
  }
  dir <- batches$parent[[first]]

  list(
    idx = idx,
    parent = dir,
    label = sprintf("%s (`%s` layer)", dir, layer),
    total_lines = sum(lengths(problem_rows$.problems[idx]))
  )
}

.ft_problem_batch_lines <- function(problem_rows, batch, n) {
  name <- character()
  problem <- character()

  for (i in batch$idx) {
    row_problems <- problem_rows$.problems[[i]]
    remaining <- n - length(problem)
    if (remaining <= 0) {
      break
    }
    if (length(row_problems) > remaining) {
      row_problems <- row_problems[seq_len(remaining)]
    }
    problem <- c(problem, .ft_format_problem_line(row_problems))
    name <- c(
      name,
      rep(
        .ft_problem_file_label(problem_rows$.rel[[i]], batch$parent),
        length(row_problems)
      )
    )
  }

  list(name = name, problem = problem)
}

.ft_problem_parent_dir <- function(path) {
  parent <- dirname(chartr("\\", "/", as.character(path)))
  parent[parent == "." | !nzchar(parent)] <- "<root>"
  parent
}

.ft_problem_file_label <- function(path, parent) {
  path <- chartr("\\", "/", as.character(path))
  if (identical(parent, "<root>")) {
    return(path)
  }

  prefix <- paste0(parent, "/")
  if (startsWith(path, prefix)) {
    return(substring(path, nchar(prefix) + 1L))
  }

  basename(path)
}

.ft_format_problem_line <- function(problem) {
  sub(
    "^filename '[^']+' does not match ",
    "filename does not match ",
    as.character(problem)
  )
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
#' Create a tree-shaped summary of the declared directory and file templates.
#' File templates are shown in the parent directory where files for that layer
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
#'   ft_add_dir_template("subject", "{subject}") |>
#'   ft_add_dir_template("time", "{time}") |>
#'   ft_add_file_template("data", "{subject}_{task}.txt")
#'
#' ft_format_schema_tree(ft)
#' @export
ft_format_schema_tree <- function(ft) {
  .ft_check_filetree(ft)

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
      .ft_format_schema_items(
        c(
          .ft_format_file_schema(ft, layers[[1]]),
          .ft_format_ignore_file_schema(ft, layers[[1]])
        ),
        ""
      )
    ))
  }

  root_file_lines <- c(
    .ft_format_file_schema(ft, layers[[1]]),
    .ft_format_ignore_file_schema(ft, layers[[1]]),
    .ft_format_ignore_dir_schema(ft, layers[[1]])
  )
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

.ft_has_file_templates <- function(ft, layer) {
  spec <- ft$file_templates[[layer]]
  !(is.null(spec) || length(spec) == 0)
}

.ft_format_dir_schema <- function(ft, layer) {
  spec <- ft$dir_templates[[layer]]
  if (is.null(spec) || length(spec) == 0) {
    return(paste0(layer, ": <none>"))
  }

  template_labels <- character(length(spec$raw))
  for (i in seq_along(spec$raw)) {
    nm <- names(spec$raw)[[i]]
    template_label <- if (identical(nm, "default") && length(spec$raw) == 1) {
      unname(spec$raw[[i]])
    } else {
      paste0(nm, " = ", unname(spec$raw[[i]]))
    }
    annotations <- c(
      .ft_format_when_annotation(spec$when[[i]]),
      .ft_format_with_annotation(spec$with[[i]])
    )
    annotations <- annotations[nzchar(annotations)]
    if (length(annotations)) {
      template_label <- paste0(
        template_label,
        " [",
        paste(annotations, collapse = "; "),
        "]"
      )
    }
    template_labels[[i]] <- template_label
  }

  paste0(layer, ": ", paste(template_labels, collapse = " | "))
}

.ft_format_file_schema <- function(ft, layer) {
  spec <- ft$file_templates[[layer]]
  if (is.null(spec) || length(spec) == 0) {
    return(character())
  }

  out <- character(length(spec$raw))
  for (i in seq_along(spec$raw)) {
    nm <- names(spec$raw)[[i]]
    template_label <- if (identical(nm, "default") && length(spec$raw) == 1) {
      unname(spec$raw[[i]])
    } else {
      paste0(nm, " = ", unname(spec$raw[[i]]))
    }
    label <- paste0("`", layer, "` file: ", template_label)
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

.ft_format_ignore_dir_schema <- function(ft, layer) {
  spec <- ft$ignore_dir_templates[[layer]]
  if (is.null(spec) || length(spec) == 0) {
    return(character())
  }

  paste0("ignored `", layer, "` dir: ", .ft_format_template_spec(spec))
}

.ft_format_ignore_file_schema <- function(ft, layer) {
  spec <- ft$ignore_file_templates[[layer]]
  if (is.null(spec) || length(spec) == 0) {
    return(character())
  }

  paste0("ignored `", layer, "` file: ", .ft_format_template_spec(spec))
}

.ft_format_template_spec <- function(spec) {
  template_labels <- character(length(spec$raw))
  for (i in seq_along(spec$raw)) {
    nm <- names(spec$raw)[[i]]
    template_label <- if (identical(nm, "default") && length(spec$raw) == 1) {
      unname(spec$raw[[i]])
    } else {
      paste0(nm, " = ", unname(spec$raw[[i]]))
    }
    annotations <- c(
      .ft_format_when_annotation(spec$when[[i]]),
      .ft_format_with_annotation(spec$with[[i]])
    )
    annotations <- annotations[nzchar(annotations)]
    if (length(annotations)) {
      template_label <- paste0(
        template_label,
        " [",
        paste(annotations, collapse = "; "),
        "]"
      )
    }
    template_labels[[i]] <- template_label
  }

  paste(template_labels, collapse = " | ")
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
  file_lines <- c(
    .ft_format_file_schema(ft, child_layer),
    .ft_format_ignore_file_schema(ft, child_layer),
    .ft_format_ignore_dir_schema(ft, child_layer)
  )
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
#' layers, regex pool size, and registered templates.
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
  .ft_check_filetree(x, arg = "x")

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

  # dir templates
  if (length(dir_layers) == 0) {
    lines <- c(lines, "  dir_templates: <none> (no dir layers)")
  } else {
    any_dir <- any(vapply(
      dir_layers,
      function(layer) {
        spec <- x$dir_templates[[layer]]
        !(is.null(spec) || length(spec) == 0)
      },
      logical(1)
    ))

    if (!any_dir) {
      lines <- c(lines, "  dir_templates: <none>")
    } else {
      lines <- c(lines, "  dir_templates:")
      for (layer in dir_layers) {
        spec <- x$dir_templates[[layer]]
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

  # file templates
  any_file <- any(vapply(
    names(x$file_templates),
    function(layer) {
      spec <- x$file_templates[[layer]]
      !(is.null(spec) || length(spec) == 0)
    },
    logical(1)
  ))

  if (!any_file) {
    lines <- c(lines, "  file_templates: <none>")
  } else {
    lines <- c(lines, "  file_templates:")
    for (layer in names(x$file_templates)) {
      spec <- x$file_templates[[layer]]
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

  # ignored dir templates
  if (length(dir_layers) == 0) {
    lines <- c(lines, "  ignore_dir_templates: <none> (no dir layers)")
  } else {
    any_ignore_dir <- any(vapply(
      dir_layers,
      function(layer) {
        spec <- x$ignore_dir_templates[[layer]]
        !(is.null(spec) || length(spec) == 0)
      },
      logical(1)
    ))

    if (!any_ignore_dir) {
      lines <- c(lines, "  ignore_dir_templates: <none>")
    } else {
      lines <- c(lines, "  ignore_dir_templates:")
      for (layer in dir_layers) {
        spec <- x$ignore_dir_templates[[layer]]
        if (is.null(spec) || length(spec) == 0) {
          next
        }
        kv <- paste0(names(spec$raw), "=\"", unname(spec$raw), "\"")
        s <- paste(kv, collapse = ", ")
        if (nchar(s) > 90) {
          s <- paste0(substr(s, 1, 87), "\u2026")
        }
        lines <- c(lines, sprintf("    - %s: %s", layer, s))
      }
    }
  }

  # ignored file templates
  any_ignore_file <- any(vapply(
    names(x$ignore_file_templates),
    function(layer) {
      spec <- x$ignore_file_templates[[layer]]
      !(is.null(spec) || length(spec) == 0)
    },
    logical(1)
  ))

  if (!any_ignore_file) {
    lines <- c(lines, "  ignore_file_templates: <none>")
  } else {
    lines <- c(lines, "  ignore_file_templates:")
    for (layer in names(x$ignore_file_templates)) {
      spec <- x$ignore_file_templates[[layer]]
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
