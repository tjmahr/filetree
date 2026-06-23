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
  expect_equal(
    ft_overwrite$dir_patterns$subject$compiled[["main"]],
    "^(?<time>(?:day\\d{2}))$"
  )

  ft_overwrite <- ft_overwrite |>
    ft_add_regex(c(time = "day\\d{3}"))

  expect_equal(
    ft_overwrite$dir_patterns$subject$compiled[["main"]],
    "^(?<time>(?:day\\d{3}))$"
  )
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
      function(x) any(grepl("no file patterns registered at_layer='data'", x, fixed = TRUE)),
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
