root1 <- testthat::test_path("test-trees/demo-1")
root2 <- testthat::test_path("test-trees/demo-2")
root3 <- testthat::test_path("test-trees/demo-3")

ft <- ft_init(
  root = root1,
  layers = c("subject", "time", "data")
)
ft <- ft |>
  ft_add_regex(c(
    subject = "\\w{2}-\\d{2}",
    time = "day\\d{2}",
    task = "red|green"
  )) |>
  ft_add_dir_template(
    layer = "time",
    template = "{time}"
  ) |>
  ft_add_dir_template(
    layer = "subject",
    template = "{subject}"
  ) |>
  ft_add_file_template(
    layer = "data",
    template = "{subject}_{task}.txt"
  )


test_that("There are no problems in a well-formed tree", {
  # This test also runs through a full tree construction
  ft_okay <- ft_init(
    root = root1,
    layers = c("subject", "time", "data")
  )
  ft_okay <- ft_okay |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template(
      layer = "time",
      template = "{time}"
    ) |>
    ft_add_dir_template(
      layer = "subject",
      template = "{subject}"
    ) |>
    ft_add_file_template(
      layer = "data",
      template = "{subject}_{task}.txt"
    )
  index <- ft_okay |> ft_index()

  # subject to change
  index |>
    hasName(c("subject", "time", "task", "template", ".ok", ".problems")) |>
    all() |>
    expect_true()

  index$.problems |>
    lengths() |>
    unique() |>
    expect_equal(0)

  expect_all_true(index$.ok)
})


test_that("Parsing is accurate in well-formed tree", {
  index <- ft |> ft_index()

  subjects <- ft$root |> list.files() |> sort()
  days <- ft$root |>
    list.files(recursive = TRUE) |>
    dirname() |>
    basename() |>
    unique() |>
    sort()

  index$subject |>
    expect_contains(list.files(ft$root))

  index$time |>
    unique() |>
    sort() |>
    expect_equal(days)

  index$task |>
    unique() |>
    sort() |>
    expect_equal(c("green", "red"))

  expect_equal(
    grepl(index$.rel, pattern = "red"),
    index$task == "red"
  )
})


test_that("There are problems in a bad tree", {
  ft_fail <- ft
  ft_fail$root <- root2

  index <- ft_fail |> ft_index()
  expect_true(any(lengths(index$.problems) > 0))
  expect_false(all(index$.ok))

  ft_fail <- ft
  ft_fail$root <- root3

  index <- ft_fail |> ft_index()
  expect_true(any(lengths(index$.problems) > 0))
  expect_false(all(index$.ok))
})

test_that("template registration overwrites existing names and recompiles after regex updates", {
  ft_overwrite <- ft_init(
    root = root1,
    layers = c("subject", "time", "data")
  )

  ft_overwrite <- ft_overwrite |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}"
    )) |>
    ft_add_dir_template(
      layer = "subject",
      template = c(main = "{subject}")
    )

  expect_warning(
    ft_overwrite <- ft_overwrite |>
      ft_add_dir_template(
        layer = "subject",
        template = c(main = "{time}")
      ),
    "already registered"
  )

  expect_equal(ft_overwrite$dir_templates$subject$raw[["main"]], "{time}")
  compiled <- ft_overwrite$dir_templates$subject$compiled[["main"]]
  expect_equal(as.character(compiled), "^(?<ftcap1>(?:day\\d{2}))$")
  expect_equal(attr(compiled, "capture_names"), c(ftcap1 = "time"))

  ft_overwrite <- ft_overwrite |>
    ft_add_regex(c(time = "day\\d{3}"))

  compiled <- ft_overwrite$dir_templates$subject$compiled[["main"]]
  expect_equal(as.character(compiled), "^(?<ftcap1>(?:day\\d{3}))$")
  expect_equal(attr(compiled, "capture_names"), c(ftcap1 = "time"))
})

test_that("unnamed directory template replacement warns", {
  ft_default <- ft_init(
    root = root1,
    layers = c("subject", "time", "data")
  )

  ft_default <- ft_default |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}"
    )) |>
    ft_add_dir_template(
      layer = "subject",
      template = "{subject}"
    )

  expect_warning(
    ft_default <- ft_default |>
      ft_add_dir_template(
        layer = "subject",
        template = "{time}"
      ),
    "default"
  )

  expect_equal(ft_default$dir_templates$subject$raw[["default"]], "{time}")
})

test_that("Regex pool anchors match layer boundaries when composed", {
  ft_anchor <- ft_init(
    root = root1,
    layers = c("subject", "data")
  )

  ft_anchor <- ft_anchor |>
    ft_add_regex(c(subject = "^[A-Z]{3}\\d{2}$")) |>
    ft_add_dir_template(
      layer = "subject",
      template = "{subject}"
    )

  compiled <- ft_anchor$dir_templates$subject$compiled[["default"]]

  expect_true(stringr::str_detect("ABC12", compiled))
  expect_false(stringr::str_detect("AAA123", compiled))
  expect_false(stringr::str_detect("AAAA12", compiled))
})

test_that("Component templates use literal fixed text and match full strings", {
  root <- fs::path_temp("filetree-component-template")
  ft_template <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{task}.txt")

  index <- ft_index(
    ft_template,
    fs::path(
      root,
      c(
        "ab-01/day01/ab-01_red.txt",
        "ab-01/day01/ab-01_redXtxt",
        "ab-01/day01/ab-01_red.txt.bak",
        "ab-01/day01b/ab-01_red.txt"
      )
    )
  )

  expect_equal(index$.ok, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(index$time, c("01", "01", "01", NA))
  expect_true(any(grepl(
    "filename 'ab-01_redXtxt' does not match a file template",
    index$.problems[[2]],
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "filename 'ab-01_red.txt.bak' does not match a file template",
    index$.problems[[3]],
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "directory name 'day01b' does not match a dir template",
    index$.problems[[4]],
    fixed = TRUE
  )))
})

test_that("Missing file templates are okay by default and problems in strict mode", {
  ft_partial <- ft_init(
    root = root1,
    layers = c("subject", "time", "data")
  )
  ft_partial <- ft_partial |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}"
    )) |>
    ft_add_dir_template(
      layer = "subject",
      template = "{subject}"
    ) |>
    ft_add_dir_template(
      layer = "time",
      template = "{time}"
    )

  index <- ft_partial |> ft_index()

  expect_true(all(index$.ok))
  expect_equal(unique(lengths(index$.problems)), 0)

  index_strict <- ft_partial |> ft_index(strict = TRUE)

  expect_false(all(index_strict$.ok))
  expect_true(all(lengths(index_strict$.problems) > 0))
  expect_true(all(
    vapply(
      index_strict$.problems,
      function(x) {
        any(grepl(
          "no file templates registered for `data` files",
          x,
          fixed = TRUE
        ))
      },
      logical(1)
    )
  ))
})

test_that("Files outside or at the root are structural problems", {
  root <- fs::path_temp("filetree-outside-root")
  outside <- fs::path(fs::path_dir(root), "outside", "ab-01_red.txt")

  ft_root <- ft_init(root, c("subject", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_file_template("data", "{subject}_{task}.txt")

  index <- ft_index(
    ft_root,
    c(
      fs::path(root, "ab-01", "ab-01_red.txt"),
      outside,
      root
    )
  )

  expect_true(index$.ok[[1]])
  expect_false(index$.ok[[2]])
  expect_false(index$.ok[[3]])
  expect_equal(
    index$.problems[[2]],
    "file is at or above root; no matching layer"
  )
  expect_equal(
    index$.problems[[3]],
    "file is at or above root; no matching layer"
  )
  expect_false(any(grepl(
    "path deeper than layers",
    index$.problems[[2]],
    fixed = TRUE
  )))
})

test_that("Relative files under the root are accepted", {
  ft_relative <- ft_init("test-trees/demo-1", c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "{time}") |>
    ft_add_file_template("data", "{subject}_{task}.txt")

  index <- ft_index(
    ft_relative,
    "test-trees/demo-1/ab-01/day01/ab-01_red.txt"
  )

  expect_true(index$.ok[[1]])
  expect_equal(index$.rel[[1]], "ab-01/day01/ab-01_red.txt")
  expect_length(index$.problems[[1]], 0)
})

test_that("Public validators report argument-specific errors", {
  expect_error(
    ft_init(root1, character()),
    "`layers` must be a non-empty character vector with no empty values.",
    fixed = TRUE
  )
  expect_error(
    ft_add_regex(ft, c("\\d+")),
    "`regexes` must be a named character vector.",
    fixed = TRUE
  )
  expect_error(
    ft_add_dir_template(ft, "data", "{task}"),
    "`layer` must be one of the directory layers: subject or time, or an integer from 1 to 2.",
    fixed = TRUE
  )
  expect_error(
    ft_add_file_template(ft, "data", character()),
    "`template` must be a non-empty character vector.",
    fixed = TRUE
  )
  expect_error(
    ft_add_file_template(
      ft,
      "data",
      "{subject}_{task}.txt",
      when = list(time = integer())
    ),
    "`when` values must be non-empty character vectors.",
    fixed = TRUE
  )
  expect_error(
    ft_index(ft, strict = NA),
    "`strict` must be `TRUE` or `FALSE`.",
    fixed = TRUE
  )
  expect_error(
    ft_glimpse_problems(ft_index(ft), n = NA),
    "`n` must be a non-negative number.",
    fixed = TRUE
  )
})

test_that("Template APIs accept integer layer references", {
  ft_integer <- ft_init(root1, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template(1, "{subject}") |>
    ft_add_dir_template(2, "{time}") |>
    ft_add_file_template(3, "{subject}_{task}.txt")

  index <- ft_index(ft_integer)

  expect_true(all(index$.ok))
  expect_equal(ft_integer$dir_templates$subject$raw[["default"]], "{subject}")
  expect_equal(ft_integer$dir_templates$time$raw[["default"]], "{time}")
  expect_equal(
    ft_integer$file_templates$data$raw[["default"]],
    "{subject}_{task}.txt"
  )

  ft_ignored <- ft_integer |>
    ft_ignore_dir_template(2, c(scratch = "tmp")) |>
    ft_ignore_file_template(3, c(notes = "{subject}_notes.txt"))

  expect_equal(ft_ignored$ignore_dir_templates$time$raw[["scratch"]], "tmp")
  expect_equal(
    ft_ignored$ignore_file_templates$data$raw[["notes"]],
    "{subject}_notes.txt"
  )
  expect_true(any(grepl(
    "ignored `time` dir: scratch = tmp",
    ft_format_schema_tree(ft_ignored),
    fixed = TRUE
  )))
})

test_that("Integer layer references have clear validation errors", {
  expect_error(
    ft_add_dir_template(ft, 0, "{subject}"),
    "`layer` 0 is the implicit root layer and cannot be used here.",
    fixed = TRUE
  )
  expect_error(
    ft_add_file_template(ft, 0, "{subject}_{task}.txt"),
    "`layer` 0 is the implicit root layer and cannot be used here.",
    fixed = TRUE
  )
  expect_error(
    ft_add_dir_template(ft, 3, "{task}"),
    "`layer` must be one of the directory layers: subject or time, or an integer from 1 to 2.",
    fixed = TRUE
  )
  expect_error(
    ft_add_file_template(ft, 4, "{subject}_{task}.txt"),
    "`layer` must be one of the configured layers: subject, time or data, or an integer from 1 to 3.",
    fixed = TRUE
  )
  expect_error(
    ft_ignore_dir_template(ft, 1.5, "{subject}"),
    "`layer` must be a layer name or a whole-number layer position.",
    fixed = TRUE
  )
  expect_error(
    ft_ignore_file_template(ft, NA_real_, "{subject}_{task}.txt"),
    "`layer` must be a layer name or a whole-number layer position.",
    fixed = TRUE
  )
  expect_error(
    ft_add_file_template(ft, Inf, "{subject}_{task}.txt"),
    "`layer` must be a layer name or a whole-number layer position.",
    fixed = TRUE
  )
  expect_error(
    ft_add_file_template(ft, c(1, 2), "{subject}_{task}.txt"),
    "`layer` must be a layer name or a whole-number layer position.",
    fixed = TRUE
  )
  expect_error(
    ft_add_file_template(ft, "1", "{subject}_{task}.txt"),
    "`layer` must be one of the configured layers: subject, time or data, or an integer from 1 to 3.",
    fixed = TRUE
  )
})

test_that("Regex pool entries can reference other regex pool entries", {
  ft_recursive <- ft_init(
    root = root1,
    layers = c("data")
  )

  ft_recursive <- ft_recursive |>
    ft_add_regex(c(
      wtocs = "wT\\d\\d",
      stocs = "s\\dT[01]\\d",
      xxv00 = "[A-Z()a-z]+v\\d\\d$",
      tocs = "{wtocs}|{stocs}"
    )) |>
    ft_add_file_template(
      layer = "data",
      template = "{tocs}_{xxv00}"
    )

  index <- ft_index(
    ft_recursive,
    file.path(ft_recursive$root, c("wT12_AlphaVv01", "s3T09_Beta(v02"))
  )

  expect_true(all(index$.ok))
  expect_equal(index$tocs, c("wT12", "s3T09"))
  expect_equal(index$xxv00, c("AlphaVv01", "Beta(v02"))
  expect_false("wtocs" %in% names(index))
  expect_false("stocs" %in% names(index))
})

test_that("Recursive regex pool entries recompile when dependencies change", {
  ft_recursive <- ft_init(
    root = root1,
    layers = c("data")
  )

  ft_recursive <- ft_recursive |>
    ft_add_regex(c(
      wtocs = "wT\\d\\d",
      stocs = "s\\dT[01]\\d",
      tocs = "{wtocs}|{stocs}"
    )) |>
    ft_add_file_template(
      layer = "data",
      template = "{tocs}.txt"
    )

  expect_true(stringr::str_detect(
    "wT12.txt",
    ft_recursive$file_templates$data$compiled$default
  ))

  ft_recursive <- ft_recursive |> ft_add_regex(c(wtocs = "wX\\d\\d"))

  expect_false(stringr::str_detect(
    "wT12.txt",
    ft_recursive$file_templates$data$compiled$default
  ))
  expect_true(stringr::str_detect(
    "wX12.txt",
    ft_recursive$file_templates$data$compiled$default
  ))
})

test_that("Recursive regex pool entries report unknown references and cycles", {
  ft_recursive <- ft_init(
    root = root1,
    layers = c("data")
  )

  expect_error(
    ft_recursive |> ft_add_regex(c(tocs = "{wtocs}|{stocs}")),
    "unknown regex name"
  )

  expect_error(
    ft_recursive |> ft_add_regex(c(a = "{b}", b = "{a}")),
    "Cyclic regex reference"
  )
})

test_that("File templates can be conditional on parent layer values", {
  root <- fs::path_temp("filetree-conditional")
  ft_conditional <- ft_init(
    root = root,
    layers = c("subject", "time", "data")
  )

  ft_conditional <- ft_conditional |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "{time}") |>
    ft_add_file_template(
      "data",
      c(txt = "{subject}_{task}.txt"),
      when = c(time = "day02")
    ) |>
    ft_add_file_template(
      "data",
      c(wav = "{subject}_{task}.wav"),
      when = c(time = "day03")
    )

  files <- fs::path(
    root,
    c("ab-01/day02/ab-01_red.txt", "ab-01/day03/ab-01_red.wav")
  )

  index <- ft_index(ft_conditional, files)

  expect_true(all(index$.ok))
  expect_equal(index$template, c("txt", "wav"))
})

test_that("Conditional file templates do not apply outside their conditions", {
  root <- fs::path_temp("filetree-conditional")
  ft_conditional <- ft_init(
    root = root,
    layers = c("subject", "time", "data")
  )

  ft_conditional <- ft_conditional |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "{time}") |>
    ft_add_file_template(
      "data",
      c(wav = "{subject}_{task}.wav"),
      when = c(time = "day03")
    )

  files <- fs::path(
    root,
    c("ab-01/day03/ab-01_red.wav", "ab-01/day02/ab-01_red.wav")
  )

  index <- ft_index(ft_conditional, files, strict = TRUE)

  expect_true(index$.ok[[1]])
  expect_false(index$.ok[[2]])
  expect_true(any(grepl(
    "does not match an applicable file template at layer `data`",
    index$.problems[[2]],
    fixed = TRUE
  )))
})

test_that("Placeholder names with underscores are extracted from templates", {
  root <- fs::path_temp("filetree-underscore-placeholder")
  ft_underscore <- ft_init(
    root = root,
    layers = c("subject", "time", "data")
  )

  ft_underscore <- ft_underscore |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task_main = "red|green",
      task_three = "yellow"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{time}_{task_main}.txt") |>
    ft_add_file_template(
      "data",
      "{subject}_{time}_{task_three}.txt",
      when = c(time = "03")
    )

  files <- fs::path(
    root,
    c("ab-01/day02/ab-01_02_red.txt", "ab-01/day03/ab-01_03_yellow.txt")
  )

  index <- ft_index(ft_underscore, files)

  expect_true(all(index$.ok))
  expect_equal(index$task_main, c("red", NA))
  expect_equal(index$task_three, c(NA, "yellow"))
})

test_that("Conditional file templates can match any of several parent values", {
  root <- fs::path_temp("filetree-conditional-many")
  ft_conditional <- ft_init(
    root = root,
    layers = c("subject", "time", "data")
  )

  ft_conditional <- ft_conditional |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task_main = "red|green",
      task_three = "yellow"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template(
      "data",
      c(main = "{subject}_{time}_{task_main}.txt"),
      when = list(time = c("01", "02"))
    ) |>
    ft_add_file_template(
      "data",
      c(day03 = "{subject}_{time}_{task_three}.txt"),
      when = c(time = "03")
    )

  files <- fs::path(
    root,
    c(
      "ab-01/day01/ab-01_01_red.txt",
      "ab-01/day02/ab-01_02_green.txt",
      "ab-01/day03/ab-01_03_yellow.txt",
      "ab-01/day03/ab-01_03_red.txt"
    )
  )

  index <- ft_index(ft_conditional, files, strict = TRUE)

  expect_equal(index$.ok, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(index$template, c("main", "main", "day03", NA))
  expect_true(any(grepl(
    "does not match a file template at layer `data`",
    index$.problems[[4]],
    fixed = TRUE
  )))
})

test_that("File templates can use template-local regex definitions", {
  root <- fs::path_temp("filetree-local-regex")
  ft_local <- ft_init(
    root = root,
    layers = c("subject", "time", "data")
  )

  ft_local <- ft_local |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template(
      "data",
      c(main = "{subject}_{time}_{task}.txt"),
      when = list(time = c("01", "02"))
    ) |>
    ft_add_file_template(
      "data",
      c(day03 = "{subject}_{time}_{task}.txt"),
      when = c(time = "03"),
      with = c(task = "yellow")
    )

  files <- fs::path(
    root,
    c(
      "ab-01/day01/ab-01_01_red.txt",
      "ab-01/day02/ab-01_02_green.txt",
      "ab-01/day03/ab-01_03_yellow.txt",
      "ab-01/day03/ab-01_03_red.txt"
    )
  )

  index <- ft_index(ft_local, files, strict = TRUE)

  expect_equal(index$.ok, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(index$task, c("red", "green", "yellow", NA))
  expect_false("task_main" %in% names(index))
  expect_false("task_three" %in% names(index))
})

test_that("directory templates can be conditional on parent layer values", {
  root <- fs::path_temp("filetree-conditional-dir")
  ft_conditional <- ft_init(root, c("site", "subject", "data")) |>
    ft_add_regex(c(
      site = "lab-a|lab-b",
      subject = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("site", "{site}") |>
    ft_add_dir_template(
      "subject",
      c(lab_a = "a-{subject}"),
      when = c(site = "lab-a")
    ) |>
    ft_add_dir_template(
      "subject",
      c(lab_b = "b-{subject}"),
      when = c(site = "lab-b")
    ) |>
    ft_add_file_template("data", "{task}.txt")

  index <- ft_index(
    ft_conditional,
    fs::path(
      root,
      c(
        "lab-a/a-01/red.txt",
        "lab-b/b-02/green.txt",
        "lab-a/b-03/red.txt"
      )
    )
  )

  expect_equal(index$.ok, c(TRUE, TRUE, FALSE))
  expect_equal(index$subject, c("01", "02", NA))
  expect_equal(
    index$.problems[[3]][[1]],
    "directory name 'b-03' does not match a dir template at layer `subject`"
  )
})

test_that("directory templates can use template-local regex definitions", {
  root <- fs::path_temp("filetree-local-dir-regex")
  ft_local <- ft_init(root, c("site", "subject", "data")) |>
    ft_add_regex(c(
      site = "lab-a|lab-b",
      subject = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("site", "{site}") |>
    ft_add_dir_template(
      "subject",
      c(lab_a = "{subject}"),
      when = c(site = "lab-a")
    ) |>
    ft_add_dir_template(
      "subject",
      c(lab_b = "{subject}"),
      when = c(site = "lab-b"),
      with = c(subject = "[A-Z]{2}")
    ) |>
    ft_add_file_template("data", "{task}.txt")

  index <- ft_index(
    ft_local,
    fs::path(
      root,
      c(
        "lab-a/01/red.txt",
        "lab-b/AB/green.txt",
        "lab-b/03/red.txt"
      )
    )
  )

  expect_equal(index$.ok, c(TRUE, TRUE, FALSE))
  expect_equal(index$subject, c("01", "AB", NA))
  expect_true(any(grepl(
    "does not match a dir template at layer `subject`",
    index$.problems[[3]],
    fixed = TRUE
  )))
})

test_that("Directory template-local regex definitions survive global regex updates", {
  root <- fs::path_temp("filetree-local-dir-regex-recompile")
  ft_local <- ft_init(root, c("site", "subject", "data")) |>
    ft_add_regex(c(
      site = "lab-a|lab-b",
      subject = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("site", "{site}") |>
    ft_add_dir_template(
      "subject",
      c(lab_b = "{subject}"),
      when = c(site = "lab-b"),
      with = c(subject = "[A-Z]{2}")
    ) |>
    ft_add_file_template("data", "{task}.txt")

  ft_local <- ft_local |> ft_add_regex(c(subject = "\\d{3}"))

  index <- ft_index(
    ft_local,
    fs::path(root, c("lab-b/AB/red.txt", "lab-b/123/green.txt"))
  )

  expect_equal(index$.ok, c(TRUE, FALSE))
  expect_equal(index$subject, c("AB", NA))
})

test_that("template-local regex definitions survive global regex updates", {
  root <- fs::path_temp("filetree-local-regex-recompile")
  ft_local <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template(
      "data",
      c(day03 = "{subject}_{time}_{task}.txt"),
      when = c(time = "03"),
      with = c(task = "yellow")
    )

  ft_local <- ft_local |> ft_add_regex(c(task = "red|green|blue"))

  index <- ft_index(
    ft_local,
    fs::path(
      root,
      c("ab-01/day03/ab-01_03_yellow.txt", "ab-01/day03/ab-01_03_blue.txt")
    ),
    strict = TRUE
  )

  expect_equal(index$.ok, c(TRUE, FALSE))
  expect_equal(index$task, c("yellow", NA))
})

test_that("File template problems use user-facing layer names", {
  root <- fs::path_temp("filetree-file-template-messages")
  ft_messages <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{time}_{task}.txt")

  index <- ft_index(
    ft_messages,
    fs::path(root, "ab-01/day02/ab-01_02_yellow.txt")
  )

  expect_equal(
    index$.problems[[1]],
    "filename 'ab-01_02_yellow.txt' does not match a file template at layer `data`"
  )
})

test_that("File capture conflicts describe filename and parent directory values", {
  root <- fs::path_temp("filetree-conflict-messages")
  ft_messages <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{time}_{task}.txt")

  index <- ft_index(
    ft_messages,
    fs::path(root, "ab-01/day02/ab-01_01_red.txt")
  )

  expect_equal(
    index$.problems[[1]],
    "filename has {.var time} {.val 01}, but a parent directory has {.var time} {.val 02}"
  )
})

test_that("Problem glimpses render problems with cli bullets", {
  index <- tibble::tibble(
    .rel = "a.txt",
    .ok = FALSE,
    .problems = list("filename has {.var time} {.val 01}")
  )

  out <- utils::capture.output(
    msg <- utils::capture.output(
      result <- ft_glimpse_problems(index, n = 1),
      type = "message"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_true(any(grepl("* a.txt: filename has", msg, fixed = TRUE)))
  expect_false(any(grepl("- a.txt: filename has", msg, fixed = TRUE)))
  expect_true(any(grepl("1/1 files with 1 problems.", out, fixed = TRUE)))
})

test_that("Directory template problems use cli formatting and layer context", {
  root <- fs::path_temp("filetree-dir-template-messages")
  ft_messages <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{task}.txt")

  index <- ft_index(
    ft_messages,
    fs::path(root, "ab-01/day3/ab-01_red.txt")
  )

  expect_equal(
    index$.problems[[1]][[1]],
    "directory name 'day3' does not match a dir template at layer `time`"
  )
})

test_that("File templates at parent layers validate sidecar files", {
  root <- fs::path_temp("filetree-sidecar-files")
  ft_messages <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("subject", "{subject}-manifest.txt") |>
    ft_add_file_template("data", "{subject}_{time}_{task}.txt")

  index <- ft_index(
    ft_messages,
    fs::path(
      root,
      c(
        "ab-01/ab-01-manifest.txt",
        "ab-02/aa-02-manifest.txt",
        "ab-03/not-a-manifest.csv"
      )
    )
  )

  expect_equal(index$at_layer, c("subject", "subject", "subject"))
  expect_true(index$.ok[[1]])
  expect_false(index$.ok[[2]])
  expect_false(index$.ok[[3]])
  expect_equal(
    index$.problems[[2]],
    "filename has {.var subject} {.val aa-02}, but a parent directory has {.var subject} {.val ab-02}"
  )
  expect_equal(
    index$.problems[[3]],
    "filename 'not-a-manifest.csv' does not match a file template at layer `subject`"
  )
})

test_that("Filenames are stored separately from raw directory layer columns", {
  ft_extra_layer <- ft_init(root3, c("subject", "time", "data", "extra")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template(
      "data",
      "{subject}_{time}_{task}.txt",
      when = list(time = c("01", "02"))
    )

  index <- ft_index(
    ft_extra_layer,
    fs::path(root3, "ab-02/day01/ab-02_01_green.txt")
  )

  expect_equal(index$at_layer, "data")
  expect_equal(index$layer__subject, "ab-02")
  expect_equal(index$layer__time, "day01")
  expect_true(is.na(index$layer__data))
  expect_true(is.na(index$layer__extra))
  expect_equal(index$.filename, "ab-02_01_green.txt")
  expect_equal(index$subject, "ab-02")
  expect_equal(index$time, "01")
  expect_equal(index$task, "green")
  expect_true(index$.ok)
})

test_that("ft_set_root updates the root path", {
  ft_root <- ft_init(root1, c("subject", "time", "data"))

  ft_updated <- ft_root |> ft_set_root(root2)

  expect_equal(ft_updated$root, fs::path_abs(root2))
  expect_equal(ft_updated$layers, ft_root$layers)
  expect_equal(ft_updated$dir_templates, ft_root$dir_templates)
  expect_error(
    ft_set_root(ft_root, character()),
    "`root` must be a single non-empty path.",
    fixed = TRUE
  )
})

test_that("Ignore file templates drop matching files from indexes", {
  root <- fs::path_temp("filetree-ignore-file")
  files <- fs::path(
    root,
    c(
      "ab-01/day01/ab-01_red.txt",
      "ab-01/day01/ab-01_notes.txt"
    )
  )
  fs::dir_create(fs::path_dir(files))
  fs::file_create(files)

  ft_ignore <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{task}.txt") |>
    ft_ignore_file_template("data", "{subject}_notes.txt")

  index <- ft_index(ft_ignore)

  expect_equal(index$.rel, "ab-01/day01/ab-01_red.txt")
  expect_true(index$.ok[[1]])
  expect_false(".ignored" %in% names(index))
})

test_that("Ignore directory templates prune matching subtrees", {
  root <- fs::path_temp("filetree-ignore-dir")
  files <- fs::path(
    root,
    c(
      "ab-01/day01/ab-01_red.txt",
      "ab-01/tmp/ab-01_bad.txt",
      "ab-02/day01/ab-02_green.txt"
    )
  )
  fs::dir_create(fs::path_dir(files))
  fs::file_create(files)

  ft_ignore <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{task}.txt") |>
    ft_ignore_dir_template("time", "tmp")

  index <- ft_index(ft_ignore)

  expect_equal(
    sort(index$.rel),
    c("ab-01/day01/ab-01_red.txt", "ab-02/day01/ab-02_green.txt")
  )
  expect_true(all(index$.ok))
})

test_that("ft_list can include or exclude ignored files", {
  root <- fs::path_temp("filetree-ignore-list")
  files <- fs::path(
    root,
    c("ab-01/day01/ab-01_red.txt", "ab-01/tmp/ab-01_bad.txt")
  )
  fs::dir_create(fs::path_dir(files))
  fs::file_create(files)

  ft_ignore <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_ignore_dir_template("time", "tmp")

  listed <- fs::path_file(ft_list(ft_ignore))
  listed_all <- fs::path_file(ft_list(ft_ignore, include_ignored = TRUE))

  expect_equal(listed, "ab-01_red.txt")
  expect_equal(sort(listed_all), c("ab-01_bad.txt", "ab-01_red.txt"))
})

test_that("Ignored files can be included as inert audit rows", {
  root <- fs::path_temp("filetree-ignore-audit")
  files <- fs::path(
    root,
    c(
      "ab-01/day01/ab-01_red.txt",
      "ab-01/tmp/not-a-valid-file-name.txt"
    )
  )
  fs::dir_create(fs::path_dir(files))
  fs::file_create(files)

  ft_ignore <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{task}.txt") |>
    ft_ignore_dir_template("time", c(scratch = "tmp"))

  index <- ft_index(ft_ignore, include_ignored = TRUE, strict = TRUE)
  ignored <- index[index$.ignored, , drop = FALSE]

  expect_equal(nrow(index), 2)
  expect_equal(nrow(ignored), 1)
  expect_equal(ignored$.rel, "ab-01/tmp/not-a-valid-file-name.txt")
  expect_equal(ignored$at_layer, "data")
  expect_equal(ignored$layer__subject, "ab-01")
  expect_equal(ignored$layer__time, "tmp")
  expect_equal(ignored$.ignore_type, "dir")
  expect_equal(ignored$.ignore_template, "scratch")
  expect_true(ignored$.ok)
  expect_length(ignored$.problems[[1]], 0)
  expect_true(is.na(ignored$subject))
  expect_true(is.na(ignored$time))
  expect_true(is.na(ignored$task))
})

test_that("Ignore templates support when and with arguments", {
  root <- fs::path_temp("filetree-ignore-when-with")
  files <- fs::path(
    root,
    c(
      "lab-a/tmp/red.txt",
      "lab-b/tmp/red.txt",
      "lab-b/dropme/red.txt",
      "lab-b/keep/red.txt"
    )
  )
  fs::dir_create(fs::path_dir(files))
  fs::file_create(files)

  ft_ignore <- ft_init(root, c("site", "bucket", "data")) |>
    ft_add_regex(c(
      site = "lab-a|lab-b",
      bucket = "tmp|dropme|keep",
      task = "red|green"
    )) |>
    ft_add_dir_template("site", "{site}") |>
    ft_add_dir_template("bucket", "{bucket}") |>
    ft_add_file_template("data", "{task}.txt") |>
    ft_ignore_dir_template(
      "bucket",
      c(lab_a = "{bucket}"),
      when = c(site = "lab-a")
    ) |>
    ft_ignore_dir_template(
      "bucket",
      c(lab_b = "{bucket}"),
      when = c(site = "lab-b"),
      with = c(bucket = "dropme")
    )

  index <- ft_index(ft_ignore)

  expect_equal(
    sort(index$.rel),
    c("lab-b/keep/red.txt", "lab-b/tmp/red.txt")
  )
})

test_that("Problem glimpses print a compact summary from an index", {
  index <- tibble::tibble(
    .rel = c("dir-a/a.txt", "dir-b/b.txt", "dir-a/c.txt"),
    at_layer = c("data", "data", "data"),
    .ok = c(FALSE, FALSE, TRUE),
    .problems = list(
      c("first problem", "second problem"),
      "third problem",
      character()
    )
  )

  out <- utils::capture.output(
    msg <- utils::capture.output(
      result <- ft_glimpse_problems(index, n = 1, n_lines = 1),
      type = "message"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$.rel, c("dir-a/a.txt", "dir-b/b.txt"))
  expect_true(any(grepl("2/3 files with 3 problems.", out, fixed = TRUE)))
  expect_true(any(grepl("Showing 1 of 2 problem batches.", out, fixed = TRUE)))
  expect_true(any(grepl("dir-a (`data` layer)", out, fixed = TRUE)))
  expect_true(any(grepl("* a.txt: first problem", msg, fixed = TRUE)))
  expect_true(any(grepl("[... 1 more problems]", out, fixed = TRUE)))
  expect_false(any(grepl("dir-b", out, fixed = TRUE)))
})

test_that("Problem glimpses show small remainders instead of truncating", {
  index <- tibble::tibble(
    .rel = paste0("dir-", 1:6, "/bad.txt"),
    at_layer = rep("data", 6),
    .ok = rep(FALSE, 6),
    .problems = as.list(paste("problem", 1:6))
  )

  out <- utils::capture.output(
    msg <- utils::capture.output(
      ft_glimpse_problems(index, n = 5, n_lines = 10),
      type = "message"
    )
  )

  expect_false(any(grepl("Showing 5 of 6 problem batches.", out, fixed = TRUE)))
  expect_true(any(grepl("dir-6 (`data` layer)", out, fixed = TRUE)))
  expect_true(any(grepl("* bad.txt: problem 6", msg, fixed = TRUE)))

  one_batch <- tibble::tibble(
    .rel = "dir-a/bad.txt",
    at_layer = "data",
    .ok = FALSE,
    .problems = list(paste("problem", 1:6))
  )

  out <- utils::capture.output(
    msg <- utils::capture.output(
      ft_glimpse_problems(one_batch, n = 1, n_lines = 5),
      type = "message"
    )
  )

  expect_false(any(grepl("[... 1 more problems]", out, fixed = TRUE)))
  expect_true(any(grepl("* bad.txt: problem 6", msg, fixed = TRUE)))
})

test_that("Problem glimpses do not repeat filenames inside filename messages", {
  index <- tibble::tibble(
    .rel = "ab-02/day02/ab-02_02_yellow.txt",
    at_layer = "data",
    .ok = FALSE,
    .problems = list(
      "filename 'ab-02_02_yellow.txt' does not match a file template at layer `data`"
    )
  )

  out <- utils::capture.output(
    msg <- utils::capture.output(
      ft_glimpse_problems(index, n = 1, n_lines = 1),
      type = "message"
    )
  )

  expect_true(any(grepl("ab-02/day02 (`data` layer)", out, fixed = TRUE)))
  expect_true(any(grepl(
    "* ab-02_02_yellow.txt: filename does not match a file template at layer `data`",
    msg,
    fixed = TRUE
  )))
  expect_false(any(grepl("filename 'ab-02_02_yellow.txt'", msg, fixed = TRUE)))
})

test_that("Problem glimpses can index a filetree", {
  ft_problem <- ft_init(root3, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{time}_{task}.txt")

  out <- utils::capture.output(
    msg <- utils::capture.output(
      result <- ft_glimpse_problems(ft_problem, n = 2),
      type = "message"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(any(grepl("files with", out, fixed = TRUE)))
  expect_true(any(grepl("*", msg, fixed = TRUE)))
})

test_that("Schema trees format layers and conditional file templates", {
  ft_schema <- ft_init("demo-root", c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template(
      "data",
      "{subject}_{time}_{task}.txt",
      when = list(time = c("01", "02"))
    ) |>
    ft_add_file_template(
      "data",
      c(day03 = "{subject}_{time}_{task}.txt"),
      when = c(time = "03"),
      with = c(task = "yellow")
    )

  lines <- ft_format_schema_tree(ft_schema)

  expect_equal(lines[[1]], as.character(ft_schema$root))
  expect_true(any(grepl("subject: {subject}", lines, fixed = TRUE)))
  expect_true(any(grepl("time: day{time}", lines, fixed = TRUE)))
  expect_true(any(grepl(
    "`data` file: default = {subject}_{time}_{task}.txt [when time in 01, 02]",
    lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "`data` file: day03 = {subject}_{time}_{task}.txt [when time == 03; with task = yellow]",
    lines,
    fixed = TRUE
  )))
})

test_that("Schema trees format conditional directory templates", {
  ft_schema <- ft_init("demo-root", c("site", "subject", "data")) |>
    ft_add_regex(c(
      site = "lab-a|lab-b",
      subject = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("site", "{site}") |>
    ft_add_dir_template(
      "subject",
      c(lab_a = "a-{subject}"),
      when = c(site = "lab-a")
    ) |>
    ft_add_dir_template(
      "subject",
      c(lab_b = "{subject}"),
      when = c(site = "lab-b"),
      with = c(subject = "[A-Z]{2}")
    ) |>
    ft_add_file_template("data", "{task}.txt")

  lines <- ft_format_schema_tree(ft_schema)

  expect_true(any(grepl("site: {site}", lines, fixed = TRUE)))
  expect_true(any(grepl(
    "subject: lab_a = a-{subject} [when site == lab-a] | lab_b = {subject} [when site == lab-b; with subject = [A-Z]{2}]",
    lines,
    fixed = TRUE
  )))
})

test_that("Schema trees show file templates registered on parent layers", {
  ft_schema <- ft_init("demo-root", c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("subject", "{subject}-manifest.txt") |>
    ft_add_file_template("time", "{subject}_{time}-manifest.txt") |>
    ft_add_file_template("data", "{subject}_{time}_{task}.txt")

  lines <- ft_format_schema_tree(ft_schema)

  subject_manifest <- grep(
    "`subject` file: {subject}-manifest.txt",
    lines,
    fixed = TRUE
  )
  subject_layer <- grep("subject: {subject}", lines, fixed = TRUE)
  time_manifest <- grep(
    "`time` file: {subject}_{time}-manifest.txt",
    lines,
    fixed = TRUE
  )
  time_layer <- grep("time: day{time}", lines, fixed = TRUE)
  data_file <- grep(
    "`data` file: {subject}_{time}_{task}.txt",
    lines,
    fixed = TRUE
  )

  expect_length(subject_manifest, 1)
  expect_length(time_manifest, 1)
  expect_length(data_file, 1)
  expect_lt(subject_manifest, subject_layer)
  expect_lt(subject_layer, time_manifest)
  expect_lt(time_manifest, time_layer)
  expect_lt(time_layer, data_file)
  expect_match(lines[[subject_manifest]], "^\u251c\u2500\u2500")
  expect_match(lines[[time_manifest]], "^    \u251c\u2500\u2500")
  expect_match(lines[[data_file]], "^        \u2514\u2500\u2500")
})

test_that("Schema trees show ignored directory and file templates", {
  ft_schema <- ft_init("demo-root", c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_dir_template("time", "day{time}") |>
    ft_add_file_template("data", "{subject}_{time}_{task}.txt") |>
    ft_ignore_dir_template("time", c(scratch = "tmp")) |>
    ft_ignore_file_template("data", c(notes = "{subject}_notes.txt"))

  lines <- ft_format_schema_tree(ft_schema)

  expect_true(any(grepl(
    "ignored `time` dir: scratch = tmp",
    lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "ignored `data` file: notes = {subject}_notes.txt",
    lines,
    fixed = TRUE
  )))
})

test_that("Formatted filetrees summarize ignored templates", {
  ft_schema <- ft_init("demo-root", c("subject", "data")) |>
    ft_add_regex(c(subject = "\\w{2}-\\d{2}", task = "red|green")) |>
    ft_ignore_dir_template("subject", c(scratch = "tmp")) |>
    ft_ignore_file_template("data", c(notes = "{subject}_notes.txt"))

  lines <- format(ft_schema)

  expect_true(grepl("ignore_dir_templates:", lines, fixed = TRUE))
  expect_true(grepl("ignore_file_templates:", lines, fixed = TRUE))
  expect_true(grepl("subject: scratch=\"tmp\"", lines, fixed = TRUE))
  expect_true(grepl(
    "at_layer=data: notes=\"{subject}_notes.txt\"",
    lines,
    fixed = TRUE
  ))
})

test_that("Schema trees hide default template names only for singleton layers", {
  ft_single <- ft_init("demo-root", c("subject", "data")) |>
    ft_add_regex(c(subject = "\\w{2}-\\d{2}", task = "red|green")) |>
    ft_add_dir_template("subject", "{subject}") |>
    ft_add_file_template("data", "{subject}_{task}.txt")

  single_lines <- ft_format_schema_tree(ft_single)

  expect_true(any(grepl(
    "`data` file: {subject}_{task}.txt",
    single_lines,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "default = {subject}_{task}.txt",
    single_lines,
    fixed = TRUE
  )))

  ft_multi <- ft_single |>
    ft_add_file_template("data", c(yellow = "{subject}_yellow.txt"))

  multi_lines <- ft_format_schema_tree(ft_multi)

  expect_true(any(grepl(
    "`data` file: default = {subject}_{task}.txt",
    multi_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "`data` file: yellow = {subject}_yellow.txt",
    multi_lines,
    fixed = TRUE
  )))
})

test_that("Schema trees print and return the filetree invisibly", {
  ft_schema <- ft_init("demo-root", c("subject", "data")) |>
    ft_add_regex(c(subject = "\\w{2}-\\d{2}")) |>
    ft_add_dir_template("subject", "{subject}")

  out <- utils::capture.output(result <- ft_schema_tree(ft_schema))

  expect_identical(result, ft_schema)
  expect_true(any(grepl("subject: {subject}", out, fixed = TRUE)))
})
