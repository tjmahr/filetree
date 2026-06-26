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
  ft_add_dir_pattern(
    layer = "time",
    patterns = "{time}"
  ) |>
  ft_add_dir_pattern(
    layer = "subject",
    patterns = "{subject}"
  ) |>
  ft_add_file_pattern(
    layer = "data",
    patterns = "{subject}_{task}.txt"
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
    ft_add_dir_pattern(
      layer = "time",
      patterns = "{time}"
    ) |>
    ft_add_dir_pattern(
      layer = "subject",
      patterns = "{subject}"
    ) |>
    ft_add_file_pattern(
      layer = "data",
      patterns = "{subject}_{task}.txt"
    )
  index <- ft_okay |> ft_index()

  # subject to change
  index |>
    hasName(c("subject", "time", "task", "pattern", ".ok", ".problems")) |>
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

test_that("No problems in well-formed tree", {
  index <- ft |> ft_index()

  # subject to change
  index |>
    hasName(c("subject", "time", "task", "pattern", ".ok", ".problems")) |>
    all() |>
    expect_true()

  index$.problems |>
    lengths() |>
    unique() |>
    expect_equal(0)
})

test_that("Pattern registration overwrites existing names and recompiles after regex updates", {
  ft_overwrite <- ft_init(
    root = root1,
    layers = c("subject", "time", "data")
  )

  ft_overwrite <- ft_overwrite |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}"
    )) |>
    ft_add_dir_pattern(
      layer = "subject",
      patterns = c(main = "{subject}")
    )

  ft_overwrite <- ft_overwrite |>
    ft_add_dir_pattern(
      layer = "subject",
      patterns = c(main = "{time}")
    )

  expect_equal(ft_overwrite$dir_patterns$subject$raw[["main"]], "{time}")
  compiled <- ft_overwrite$dir_patterns$subject$compiled[["main"]]
  expect_equal(as.character(compiled), "^(?<ftcap1>(?:day\\d{2}))$")
  expect_equal(attr(compiled, "capture_names"), c(ftcap1 = "time"))

  ft_overwrite <- ft_overwrite |>
    ft_add_regex(c(time = "day\\d{3}"))

  compiled <- ft_overwrite$dir_patterns$subject$compiled[["main"]]
  expect_equal(as.character(compiled), "^(?<ftcap1>(?:day\\d{3}))$")
  expect_equal(attr(compiled, "capture_names"), c(ftcap1 = "time"))
})

test_that("Regex pool anchors match layer boundaries when composed", {
  ft_anchor <- ft_init(
    root = root1,
    layers = c("subject", "data")
  )

  ft_anchor <- ft_anchor |>
    ft_add_regex(c(subject = "^[A-Z]{3}\\d{2}$")) |>
    ft_add_dir_pattern(
      layer = "subject",
      patterns = "{subject}"
    )

  compiled <- ft_anchor$dir_patterns$subject$compiled[["default"]]

  expect_true(stringr::str_detect("ABC12", compiled))
  expect_false(stringr::str_detect("AAA123", compiled))
  expect_false(stringr::str_detect("AAAA12", compiled))
})

test_that("Missing file patterns are okay by default and problems in strict mode", {
  ft_partial <- ft_init(
    root = root1,
    layers = c("subject", "time", "data")
  )
  ft_partial <- ft_partial |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "day\\d{2}"
    )) |>
    ft_add_dir_pattern(
      layer = "subject",
      patterns = "{subject}"
    ) |>
    ft_add_dir_pattern(
      layer = "time",
      patterns = "{time}"
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
      function(x) any(grepl("no file patterns registered for `data` files", x, fixed = TRUE)),
      logical(1)
    )
  ))
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
    ft_add_file_pattern(
      layer = "data",
      patterns = "{tocs}_{xxv00}"
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
    ft_add_file_pattern(
      layer = "data",
      patterns = "{tocs}[.]txt"
    )

  expect_true(stringr::str_detect("wT12.txt", ft_recursive$file_patterns$data$compiled$default))

  ft_recursive <- ft_recursive |> ft_add_regex(c(wtocs = "wX\\d\\d"))

  expect_false(stringr::str_detect("wT12.txt", ft_recursive$file_patterns$data$compiled$default))
  expect_true(stringr::str_detect("wX12.txt", ft_recursive$file_patterns$data$compiled$default))
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

test_that("File patterns can be conditional on parent layer values", {
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
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "{time}") |>
    ft_add_file_pattern("data", c(txt = "{subject}_{task}.txt"), when = c(time = "day02")) |>
    ft_add_file_pattern("data", c(wav = "{subject}_{task}.wav"), when = c(time = "day03"))

  files <- fs::path(
    root,
    c("ab-01/day02/ab-01_red.txt", "ab-01/day03/ab-01_red.wav")
  )

  index <- ft_index(ft_conditional, files)

  expect_true(all(index$.ok))
  expect_equal(index$pattern, c("txt", "wav"))
})

test_that("Conditional file patterns do not apply outside their conditions", {
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
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "{time}") |>
    ft_add_file_pattern("data", c(wav = "{subject}_{task}.wav"), when = c(time = "day03"))

  files <- fs::path(
    root,
    c("ab-01/day03/ab-01_red.wav", "ab-01/day02/ab-01_red.wav")
  )

  index <- ft_index(ft_conditional, files, strict = TRUE)

  expect_true(index$.ok[[1]])
  expect_false(index$.ok[[2]])
  expect_true(any(grepl("does not match an applicable file pattern at layer `data`", index$.problems[[2]], fixed = TRUE)))
})

test_that("Placeholder names with underscores are extracted from patterns", {
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
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern("data", "{subject}_{time}_{task_main}.txt") |>
    ft_add_file_pattern("data", "{subject}_{time}_{task_three}.txt", when = c(time = "03"))

  files <- fs::path(
    root,
    c("ab-01/day02/ab-01_02_red.txt", "ab-01/day03/ab-01_03_yellow.txt")
  )

  index <- ft_index(ft_underscore, files)

  expect_true(all(index$.ok))
  expect_equal(index$task_main, c("red", NA))
  expect_equal(index$task_three, c(NA, "yellow"))
})

test_that("Conditional file patterns can match any of several parent values", {
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
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern(
      "data",
      c(main = "{subject}_{time}_{task_main}.txt"),
      when = list(time = c("01", "02"))
    ) |>
    ft_add_file_pattern(
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
  expect_equal(index$pattern, c("main", "main", "day03", NA))
  expect_true(any(grepl("does not match a file pattern at layer `data`", index$.problems[[4]], fixed = TRUE)))
})

test_that("File patterns can use pattern-local regex definitions", {
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
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern(
      "data",
      c(main = "{subject}_{time}_{task}.txt"),
      when = list(time = c("01", "02"))
    ) |>
    ft_add_file_pattern(
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

test_that("Directory patterns can be conditional on parent layer values", {
  root <- fs::path_temp("filetree-conditional-dir")
  ft_conditional <- ft_init(root, c("site", "subject", "data")) |>
    ft_add_regex(c(
      site = "lab-a|lab-b",
      subject = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("site", "{site}") |>
    ft_add_dir_pattern(
      "subject",
      c(lab_a = "a-{subject}"),
      when = c(site = "lab-a")
    ) |>
    ft_add_dir_pattern(
      "subject",
      c(lab_b = "b-{subject}"),
      when = c(site = "lab-b")
    ) |>
    ft_add_file_pattern("data", "{task}.txt")

  index <- ft_index(
    ft_conditional,
    fs::path(root, c(
      "lab-a/a-01/red.txt",
      "lab-b/b-02/green.txt",
      "lab-a/b-03/red.txt"
    ))
  )

  expect_equal(index$.ok, c(TRUE, TRUE, FALSE))
  expect_equal(index$subject, c("01", "02", NA))
  expect_equal(
    index$.problems[[3]][[1]],
    "directory name 'b-03' does not match a dir pattern at layer `subject`"
  )
})

test_that("Directory patterns can use pattern-local regex definitions", {
  root <- fs::path_temp("filetree-local-dir-regex")
  ft_local <- ft_init(root, c("site", "subject", "data")) |>
    ft_add_regex(c(
      site = "lab-a|lab-b",
      subject = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("site", "{site}") |>
    ft_add_dir_pattern(
      "subject",
      c(lab_a = "{subject}"),
      when = c(site = "lab-a")
    ) |>
    ft_add_dir_pattern(
      "subject",
      c(lab_b = "{subject}"),
      when = c(site = "lab-b"),
      with = c(subject = "[A-Z]{2}")
    ) |>
    ft_add_file_pattern("data", "{task}.txt")

  index <- ft_index(
    ft_local,
    fs::path(root, c(
      "lab-a/01/red.txt",
      "lab-b/AB/green.txt",
      "lab-b/03/red.txt"
    ))
  )

  expect_equal(index$.ok, c(TRUE, TRUE, FALSE))
  expect_equal(index$subject, c("01", "AB", NA))
  expect_true(any(grepl("does not match a dir pattern at layer `subject`", index$.problems[[3]], fixed = TRUE)))
})

test_that("Directory pattern-local regex definitions survive global regex updates", {
  root <- fs::path_temp("filetree-local-dir-regex-recompile")
  ft_local <- ft_init(root, c("site", "subject", "data")) |>
    ft_add_regex(c(
      site = "lab-a|lab-b",
      subject = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("site", "{site}") |>
    ft_add_dir_pattern(
      "subject",
      c(lab_b = "{subject}"),
      when = c(site = "lab-b"),
      with = c(subject = "[A-Z]{2}")
    ) |>
    ft_add_file_pattern("data", "{task}.txt")

  ft_local <- ft_local |> ft_add_regex(c(subject = "\\d{3}"))

  index <- ft_index(
    ft_local,
    fs::path(root, c("lab-b/AB/red.txt", "lab-b/123/green.txt"))
  )

  expect_equal(index$.ok, c(TRUE, FALSE))
  expect_equal(index$subject, c("AB", NA))
})

test_that("Pattern-local regex definitions survive global regex updates", {
  root <- fs::path_temp("filetree-local-regex-recompile")
  ft_local <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern(
      "data",
      c(day03 = "{subject}_{time}_{task}.txt"),
      when = c(time = "03"),
      with = c(task = "yellow")
    )

  ft_local <- ft_local |> ft_add_regex(c(task = "red|green|blue"))

  index <- ft_index(
    ft_local,
    fs::path(root, c("ab-01/day03/ab-01_03_yellow.txt", "ab-01/day03/ab-01_03_blue.txt")),
    strict = TRUE
  )

  expect_equal(index$.ok, c(TRUE, FALSE))
  expect_equal(index$task, c("yellow", NA))
})

test_that("File pattern problems use user-facing layer names", {
  root <- fs::path_temp("filetree-file-pattern-messages")
  ft_messages <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern("data", "{subject}_{time}_{task}.txt")

  index <- ft_index(
    ft_messages,
    fs::path(root, "ab-01/day02/ab-01_02_yellow.txt")
  )

  expect_equal(
    index$.problems[[1]],
    "filename 'ab-01_02_yellow.txt' does not match a file pattern at layer `data`"
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
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern("data", "{subject}_{time}_{task}.txt")

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

test_that("Directory pattern problems use cli formatting and layer context", {
  root <- fs::path_temp("filetree-dir-pattern-messages")
  ft_messages <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern("data", "{subject}_{task}.txt")

  index <- ft_index(
    ft_messages,
    fs::path(root, "ab-01/day3/ab-01_red.txt")
  )

  expect_equal(
    index$.problems[[1]][[1]],
    "directory name 'day3' does not match a dir pattern at layer `time`"
  )
})

test_that("File patterns at parent layers validate sidecar files", {
  root <- fs::path_temp("filetree-sidecar-files")
  ft_messages <- ft_init(root, c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern("subject", "{subject}-manifest.txt") |>
    ft_add_file_pattern("data", "{subject}_{time}_{task}.txt")

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
    "filename 'not-a-manifest.csv' does not match a file pattern at layer `subject`"
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
      "filename 'ab-02_02_yellow.txt' does not match a file pattern at layer `data`"
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
    "* ab-02_02_yellow.txt: filename does not match a file pattern at layer `data`",
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
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern("data", "{subject}_{time}_{task}.txt")

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

test_that("Schema trees format layers and conditional file patterns", {
  ft_schema <- ft_init("demo-root", c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern(
      "data",
      "{subject}_{time}_{task}.txt",
      when = list(time = c("01", "02"))
    ) |>
    ft_add_file_pattern(
      "data",
      c(day03 = "{subject}_{time}_{task}.txt"),
      when = c(time = "03"),
      with = c(task = "yellow")
    )

  lines <- ft_format_schema_tree(ft_schema)

  expect_equal(lines[[1]], as.character(ft_schema$root))
  expect_true(any(grepl("subject: {subject}", lines, fixed = TRUE)))
  expect_true(any(grepl("time: day{time}", lines, fixed = TRUE)))
  expect_true(any(grepl("`data` file: default = {subject}_{time}_{task}.txt [when time in 01, 02]", lines, fixed = TRUE)))
  expect_true(any(grepl("`data` file: day03 = {subject}_{time}_{task}.txt [when time == 03; with task = yellow]", lines, fixed = TRUE)))
})

test_that("Schema trees format conditional directory patterns", {
  ft_schema <- ft_init("demo-root", c("site", "subject", "data")) |>
    ft_add_regex(c(
      site = "lab-a|lab-b",
      subject = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("site", "{site}") |>
    ft_add_dir_pattern(
      "subject",
      c(lab_a = "a-{subject}"),
      when = c(site = "lab-a")
    ) |>
    ft_add_dir_pattern(
      "subject",
      c(lab_b = "{subject}"),
      when = c(site = "lab-b"),
      with = c(subject = "[A-Z]{2}")
    ) |>
    ft_add_file_pattern("data", "{task}.txt")

  lines <- ft_format_schema_tree(ft_schema)

  expect_true(any(grepl("site: {site}", lines, fixed = TRUE)))
  expect_true(any(grepl(
    "subject: lab_a = a-{subject} [when site == lab-a] | lab_b = {subject} [when site == lab-b; with subject = [A-Z]{2}]",
    lines,
    fixed = TRUE
  )))
})

test_that("Schema trees show file patterns registered on parent layers", {
  ft_schema <- ft_init("demo-root", c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern("subject", "{subject}-manifest.txt") |>
    ft_add_file_pattern("time", "{subject}_{time}-manifest.txt") |>
    ft_add_file_pattern("data", "{subject}_{time}_{task}.txt")

  lines <- ft_format_schema_tree(ft_schema)

  subject_manifest <- grep("`subject` file: {subject}-manifest.txt", lines, fixed = TRUE)
  subject_layer <- grep("subject: {subject}", lines, fixed = TRUE)
  time_manifest <- grep("`time` file: {subject}_{time}-manifest.txt", lines, fixed = TRUE)
  time_layer <- grep("time: day{time}", lines, fixed = TRUE)
  data_file <- grep("`data` file: {subject}_{time}_{task}.txt", lines, fixed = TRUE)

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

test_that("Schema trees hide default pattern names only for singleton layers", {
  ft_single <- ft_init("demo-root", c("subject", "data")) |>
    ft_add_regex(c(subject = "\\w{2}-\\d{2}", task = "red|green")) |>
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_file_pattern("data", "{subject}_{task}.txt")

  single_lines <- ft_format_schema_tree(ft_single)

  expect_true(any(grepl("`data` file: {subject}_{task}.txt", single_lines, fixed = TRUE)))
  expect_false(any(grepl("default = {subject}_{task}.txt", single_lines, fixed = TRUE)))

  ft_multi <- ft_single |>
    ft_add_file_pattern("data", c(yellow = "{subject}_yellow.txt"))

  multi_lines <- ft_format_schema_tree(ft_multi)

  expect_true(any(grepl("`data` file: default = {subject}_{task}.txt", multi_lines, fixed = TRUE)))
  expect_true(any(grepl("`data` file: yellow = {subject}_yellow.txt", multi_lines, fixed = TRUE)))
})

test_that("Schema trees print and return the filetree invisibly", {
  ft_schema <- ft_init("demo-root", c("subject", "data")) |>
    ft_add_regex(c(subject = "\\w{2}-\\d{2}")) |>
    ft_add_dir_pattern("subject", "{subject}")

  out <- utils::capture.output(result <- ft_schema_tree(ft_schema))

  expect_identical(result, ft_schema)
  expect_true(any(grepl("subject: {subject}", out, fixed = TRUE)))
})

test_that("Schema trees format layers and conditional file patterns", {
  ft_schema <- ft_init("demo-root", c("subject", "time", "data")) |>
    ft_add_regex(c(
      subject = "\\w{2}-\\d{2}",
      time = "\\d{2}",
      task = "red|green"
    )) |>
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_dir_pattern("time", "day{time}") |>
    ft_add_file_pattern(
      "data",
      "{subject}_{time}_{task}.txt",
      when = list(time = c("01", "02"))
    ) |>
    ft_add_file_pattern(
      "data",
      c(day03 = "{subject}_{time}_{task}.txt"),
      when = c(time = "03"),
      with = c(task = "yellow")
    )

  lines <- ft_format_schema_tree(ft_schema)

  expect_equal(lines[[1]], as.character(ft_schema$root))
  expect_true(any(grepl("subject: {subject}", lines, fixed = TRUE)))
  expect_true(any(grepl("time: day{time}", lines, fixed = TRUE)))
  expect_true(any(grepl("`data` file: default = {subject}_{time}_{task}.txt [when time in 01, 02]", lines, fixed = TRUE)))
  expect_true(any(grepl("`data` file: day03 = {subject}_{time}_{task}.txt [when time == 03; with task = yellow]", lines, fixed = TRUE)))
})

test_that("Schema trees hide default pattern names only for singleton layers", {
  ft_single <- ft_init("demo-root", c("subject", "data")) |>
    ft_add_regex(c(subject = "\\w{2}-\\d{2}", task = "red|green")) |>
    ft_add_dir_pattern("subject", "{subject}") |>
    ft_add_file_pattern("data", "{subject}_{task}.txt")

  single_lines <- ft_format_schema_tree(ft_single)

  expect_true(any(grepl("`data` file: {subject}_{task}.txt", single_lines, fixed = TRUE)))
  expect_false(any(grepl("default = {subject}_{task}.txt", single_lines, fixed = TRUE)))

  ft_multi <- ft_single |>
    ft_add_file_pattern("data", c(yellow = "{subject}_yellow.txt"))

  multi_lines <- ft_format_schema_tree(ft_multi)

  expect_true(any(grepl("`data` file: default = {subject}_{task}.txt", multi_lines, fixed = TRUE)))
  expect_true(any(grepl("`data` file: yellow = {subject}_yellow.txt", multi_lines, fixed = TRUE)))
})

test_that("Schema trees print and return the filetree invisibly", {
  ft_schema <- ft_init("demo-root", c("subject", "data")) |>
    ft_add_regex(c(subject = "\\w{2}-\\d{2}")) |>
    ft_add_dir_pattern("subject", "{subject}")

  out <- utils::capture.output(result <- ft_schema_tree(ft_schema))

  expect_identical(result, ft_schema)
  expect_true(any(grepl("subject: {subject}", out, fixed = TRUE)))
})
