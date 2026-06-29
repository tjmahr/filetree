
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filetree

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjmahr/filetree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjmahr/filetree/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A (currently) largely **vibecoded** package for declarative filetrees.
This package is mostly a proof-of-concept or an API experiment.
Contributions:

- 🤖: R code, roxygen2 descriptions
- 🤓: README

If I review and refactor things, they will move from robot to nerd.

## Installation

You can install the development version of filetree from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tjmahr/filetree")
```

## Example

Here is some data organized into nice folders.

``` r
"./inst/demo-1" |> 
  fs::dir_tree(recurse = TRUE)
#> ./inst/demo-1
#> ├── ab-01
#> │   ├── day01
#> │   │   ├── ab-01_green.txt
#> │   │   └── ab-01_red.txt
#> │   ├── day02
#> │   │   ├── ab-01_green.txt
#> │   │   └── ab-01_red.txt
#> │   └── day03
#> │       └── ab-01_green.txt
#> └── ac-02
#>     ├── day01
#>     │   ├── ac-02_green.txt
#>     │   └── ac-02_red.txt
#>     ├── day02
#>     │   ├── ac-02_green.txt
#>     │   └── ac-02_red.txt
#>     └── day03
#>         ├── ac-02_green.txt
#>         └── ac-02_red.txt
```

We set up the filetree with `ft_init()`. Each *layer* is a level of
hierarchy. At the root, when we list the folders, we see subjects. So
the first layer is `"subject"`. Inside of a subject folder, when we list
the folders, we see time points. The layer is therefore `"time"`.
Finally, in the last level, we see out main data files, so we call this
layer `"data"`.

``` r
library(filetree)

ft <- ft_init(
  root = "./inst/demo-1", 
  layers = c("subject", "time", "data")
)
ft
#> <filetree> root: C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#>   layers: subject / time / data
#>   file_layer: data
#>   regex_pool: <empty>
#>   dir_patterns: <none>
#>   file_patterns: <none>
```

Now, we define the patterns. First, we register and store regexes. These
can be reused in patterns so we can describe things very succinctly. For
example, after defining the regexes `subject` and `task`, we can say
that files have the pattern `"{subject}_{task}.txt"`.

``` r
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
ft
#> <filetree> root: C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#>   layers: subject / time / data
#>   file_layer: data
#>   regex_pool: 3 (subject, time, task)
#>   dir_patterns:
#>     - subject: default="{subject}"
#>     - time: default="{time}"
#>   file_patterns:
#>     - at_layer=data: default="{subject}_{task}.txt"
```

We can also view the filetree schema as a tree:

``` r
ft |> ft_schema_tree()
#> C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#> └── subject: {subject}
#>     └── time: {time}
#>         └── `data` file: {subject}_{task}.txt
```

Now we can validate and parse the file names.

``` r
# Look at the files
ft |> ft_list() |> fs::path_rel()
#> inst/demo-1/ab-01/day01/ab-01_green.txt inst/demo-1/ab-01/day01/ab-01_red.txt   
#> inst/demo-1/ab-01/day02/ab-01_green.txt inst/demo-1/ab-01/day02/ab-01_red.txt   
#> inst/demo-1/ab-01/day03/ab-01_green.txt inst/demo-1/ac-02/day01/ac-02_green.txt 
#> inst/demo-1/ac-02/day01/ac-02_red.txt   inst/demo-1/ac-02/day02/ac-02_green.txt 
#> inst/demo-1/ac-02/day02/ac-02_red.txt   inst/demo-1/ac-02/day03/ac-02_green.txt 
#> inst/demo-1/ac-02/day03/ac-02_red.txt

ft |> ft_index()
#> # A tibble: 11 × 12
#>    .path           .rel  at_layer layer__subject layer__time layer__data subject
#>    <fs::path>      <chr> <chr>    <chr>          <chr>       <chr>       <chr>  
#>  1 …b-01_green.txt ab-0… data     ab-01          day01       ab-01_gree… ab-01  
#>  2 …/ab-01_red.txt ab-0… data     ab-01          day01       ab-01_red.… ab-01  
#>  3 …b-01_green.txt ab-0… data     ab-01          day02       ab-01_gree… ab-01  
#>  4 …/ab-01_red.txt ab-0… data     ab-01          day02       ab-01_red.… ab-01  
#>  5 …b-01_green.txt ab-0… data     ab-01          day03       ab-01_gree… ab-01  
#>  6 …c-02_green.txt ac-0… data     ac-02          day01       ac-02_gree… ac-02  
#>  7 …/ac-02_red.txt ac-0… data     ac-02          day01       ac-02_red.… ac-02  
#>  8 …c-02_green.txt ac-0… data     ac-02          day02       ac-02_gree… ac-02  
#>  9 …/ac-02_red.txt ac-0… data     ac-02          day02       ac-02_red.… ac-02  
#> 10 …c-02_green.txt ac-0… data     ac-02          day03       ac-02_gree… ac-02  
#> 11 …/ac-02_red.txt ac-0… data     ac-02          day03       ac-02_red.… ac-02  
#> # ℹ 5 more variables: time <chr>, task <chr>, pattern <chr>, .ok <lgl>,
#> #   .problems <list>
```

For comparison, here is a file tree with some problems.

    # fs::dir_tree("./inst/demo-2")
    ├── ab-01
    │   ├── day01
    │   │   ├── ab-01_blue.txt       <---- 'blue' is not a valid task
    │   │   └── ab-01_red.txt
    │   ├── day02
    │   │   ├── ab-01_green.txt
    │   │   └── ac-01_red.txt        <---- wrong subject ('ac-01')
    │   └── day03
    │       └── ab-01_green.txt
    └── ac-02
        ├── day01
        │   ├── ac-02_green.txt
        │   └── ac-02_red.txt
        ├── day02
        │   ├── ac-02_green.txt
        │   └── ac-02_red.txt
        └── day3                     <---- expected 2-digit format
            ├── ac-02_green.txt    
            └── ac-02_red.txt

That is,

- a misformatted day folder
- a file with the wrong subject
- a file with a nonexisting task (“blue”)

We reuse the same patterns as earlier and find the bad file names:

``` r
ft <- "./inst/demo-2" |> 
  ft_init(c("subject", "time", "data")) |>
  ft_add_regex(c(
    subject = "\\w{2}-\\d{2}",
    time = "day\\d{2}",
    task = "red|green"
  )) |> 
  ft_add_dir_pattern("time", "{time}") |> 
  ft_add_dir_pattern("subject", "{subject}") |> 
  ft_add_file_pattern("data", "{subject}_{task}.txt")

ft |> 
  ft_index() |> 
  ft_glimpse_problems()
#> 4/11 files with 4 problems.
#> 
#> ab-01/day01 (`data` layer)
#> • ab-01_blue.txt: filename does not match a file pattern at layer `data`
#> 
#> ab-01/day02 (`data` layer)
#> • ac-01_red.txt: filename has `subject` "ac-01", but a parent directory has
#>   `subject` "ab-01"
#> 
#> ac-02/day3 (`data` layer)
#> • ac-02_green.txt: directory name 'day3' does not match a dir pattern at layer
#>   `time`
#> • ac-02_red.txt: directory name 'day3' does not match a dir pattern at layer
#>   `time`
```

### Pattern consistency and conditional patterns

The following example demonstrates

- that fields need to be consistent along a path
- regexes and file patterns can be defined for only specific layer
  values

In the `time` layer, folders are named `day{time}`. In the data layer,
files are named `"{subject}_{time}_{task}.txt"`. In this demo, there is
a file where the time values don’t match. There is also a task
`"yellow"` that only appears on `day03`. There is also a file that
appears at a middle layer in the hierarchy that we need to check.

    # fs::dir_tree("./inst/demo-3")
    ./inst/demo-3
    ├── ab-01
    │   ├── ab-01-manifest.txt
    │   ├── day01
    │   │   ├── ab-01_01_green.txt
    │   │   └── ab-01_01_red.txt
    │   ├── day02
    │   │   ├── ab-01_01_green.txt   <---- wrong time value
    │   │   └── ab-01_01_red.txt     <---- wrong time value
    │   └── day03
    │       └── ab-01_03_yellow.txt
    └── ab-02
        ├── aa-02-manifest.txt       <---- wrong subject value
        ├── day01
        │   ├── ab-02_01_green.txt
        │   └── ab-02_01_red.txt
        ├── day02
        │   ├── ab-02_02_green.txt
        │   ├── ab-02_02_red.txt
        │   └── ab-02_02_yellow.txt  <---- can't go here
        └── day03
            └── ab-02_03_green.txt   <---- can't go here

We can make patterns apply conditionally by using `when = list(...)` to
say which layers a pattern applies to. We can also conditionally
override regex patterns using `with = c(...)`.

``` r
ft <- "./inst/demo-3" |> 
  ft_init(c("subject", "time", "data")) |>
  ft_add_regex(c(
    subject = "\\w{2}-\\d{2}",
    time = "\\d{2}",
    task = "red|green"
  )) |> 
  ft_add_dir_pattern("time", "day{time}") |> 
  ft_add_dir_pattern("subject", "{subject}") |> 
  ft_add_file_pattern(
    "data", 
    "{subject}_{time}_{task}.txt",
    # limit when this pattern is used
    when = list(time = c("01", "02")),
  ) |> 
  ft_add_file_pattern(
    "time", 
    "{subject}-manifest.txt"
  ) |> 
  ft_add_file_pattern(
    "data", 
    c(day03 = "{subject}_{time}_{task}.txt"),
    # limit when this pattern is used
    when = c(time = "03"),
    # temporarily overwrite the pattern too
    with = c(task = "yellow")
  )
```

In the tree view, we can see multiple file patterns in the `data` layer:

``` r
ft_schema_tree(ft)
#> C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-3
#> └── subject: {subject}
#>     ├── `time` file: {subject}-manifest.txt
#>     └── time: day{time}
#>         ├── `data` file: default = {subject}_{time}_{task}.txt [when time in 01, 02]
#>         └── `data` file: day03 = {subject}_{time}_{task}.txt [when time == 03; with task = yellow]
```

We should find our five problems:

``` r
ft |> 
  ft_index() |> 
  ft_glimpse_problems()
#> 5/13 files with 5 problems.
#> 
#> ab-01/day02 (`data` layer)
#> • ab-01_01_green.txt: filename has `time` "01", but a parent directory has
#>   `time` "02"
#> • ab-01_01_red.txt: filename has `time` "01", but a parent directory has `time`
#>   "02"
#> 
#> ab-02 (`time` layer)
#> • aa-02-manifest.txt: filename has `subject` "aa-02", but a parent directory
#>   has `subject` "ab-02"
#> 
#> ab-02/day02 (`data` layer)
#> • ab-02_02_yellow.txt: filename does not match a file pattern at layer `data`
#> 
#> ab-02/day03 (`data` layer)
#> • ab-02_03_green.txt: filename does not match a file pattern at layer `data`
```

When the ft is complex, we can print out a tree-like version:

``` r
ft_schema_tree(ft)
#> C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-3
#> └── subject: {subject}
#>     ├── `time` file: {subject}-manifest.txt
#>     └── time: day{time}
#>         ├── `data` file: default = {subject}_{time}_{task}.txt [when time in 01, 02]
#>         └── `data` file: day03 = {subject}_{time}_{task}.txt [when time == 03; with task = yellow]
```

Because the parsed out layers and fields need to kept separate from each
other, we get a lot of columns now:

``` r
ft |> 
  ft_index() |> 
  dplyr::glimpse()
#> Rows: 13
#> Columns: 12
#> $ .path          <fs::path> "C:/Users/Tristan/Documents/GitRepos/filetree/inst…
#> $ .rel           <chr> "ab-01/ab-01-manifest.txt", "ab-01/day01/ab-01_01_green…
#> $ at_layer       <chr> "time", "data", "data", "data", "data", "data", "time",…
#> $ layer__subject <chr> "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "…
#> $ layer__time    <chr> NA, "day01", "day01", "day02", "day02", "day03", NA, "d…
#> $ layer__data    <chr> "ab-01-manifest.txt", "ab-01_01_green.txt", "ab-01_01_r…
#> $ subject        <chr> "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "…
#> $ time           <chr> NA, "01", "01", "02", "02", "03", NA, "01", "01", "02",…
#> $ task           <chr> NA, "green", "red", "green", "red", "yellow", NA, "gree…
#> $ pattern        <chr> "default", "default", "default", "default", "default", …
#> $ .ok            <lgl> TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE…
#> $ .problems      <list> <NULL>, <NULL>, <NULL>, "filename has {.var time} {.va…
```

## Current impressions

I have been using this tool validate our speech corpus data on our
network drives. The `ft_index()` operation is very expensive in this
case because of the network access and the thousands upon thousands of
files at play. I’ve found it better to instead do something like:

``` r
files_to_check <- fs::dir_ls(ft$root, recurse = TRUE, type = "file")
results <- ft_index(ft, files_to_check)

# Even faster to prefilter the tree if we are just interested in a particular
# set of files
files_to_check <- ft$root |> 
  fs::dir_ls(recurse = TRUE, type = "file", regexp = ".txt$")

results <- ft_index(ft, files_to_check)
```

------------------------------------------------------------------------

It would be nice to

- [ ] check inventory/completeness. (Did you notice a missing “red” file
  in the first tree?)

- [x] constrain parent folder. (Maybe a “yellow” is given on and only on
  day 3.) Or is that more of a dplyr-layer move for validation?

- [ ] add validation that we can reconstruct `.rel` from the
  concatenation of each layer?

- [x] having a really good and fast way to see the problem files
