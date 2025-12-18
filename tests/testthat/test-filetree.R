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
