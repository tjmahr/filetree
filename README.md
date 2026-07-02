
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filetree

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjmahr/filetree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjmahr/filetree/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A largely AI-coded package for declarative filetrees with validation and
parsing.

## Installation

Install the development version of filetree from
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
hierarchy. At the root, when we list the folders, we see subjects. So,
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
#>   dir_templates: <none>
#>   file_templates: <none>
#>   ignore_dir_templates: <none>
#>   ignore_file_templates: <none>
```

We define the schema with two related pieces. Field *regexes* define
values to extract, and *templates* arrange those fields with fixed text.
Regexes define fields; templates arrange fields.

For example, after defining the field regexes `subject` and `task`, we
can say that files have the template `"{subject}_{task}.txt"`.
**Templates match complete directory names or file names**, and fixed
text is literal. That means `.txt` is a real extension here, not a
regular expression wildcard. Likewise, `"day{time}"` matches `"day01"`
when `time = "\\d{2}"`, but not `"day01b"`.

``` r
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
ft
#> <filetree> root: C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#>   layers: subject / time / data
#>   file_layer: data
#>   regex_pool: 3 (subject, time, task)
#>   dir_templates:
#>     - subject: default="{subject}"
#>     - time: default="{time}"
#>   file_templates:
#>     - at_layer=data: default="{subject}_{task}.txt"
#>   ignore_dir_templates: <none>
#>   ignore_file_templates: <none>
```

Layer arguments also accept positive integer positions. Layer `1` is the
first configured layer, while layer `0` is the implicit root and cannot
be used for template registration.

``` r
ft |>
  ft_add_dir_template(layer = 2, template = "{time}")
#> Warning: Template "default" is already registered at this layer.
#> ℹ The existing template will be replaced. Use unique template names to keep
#>   multiple templates.
#> <filetree> root: C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#>   layers: subject / time / data
#>   file_layer: data
#>   regex_pool: 3 (subject, time, task)
#>   dir_templates:
#>     - subject: default="{subject}"
#>     - time: default="{time}"
#>   file_templates:
#>     - at_layer=data: default="{subject}_{task}.txt"
#>   ignore_dir_templates: <none>
#>   ignore_file_templates: <none>
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
#> # A tibble: 11 × 13
#>    .path         .rel  at_layer .filename layer__subject layer__time layer__data
#>    <fs::path>    <chr> <chr>    <chr>     <chr>          <chr>       <chr>      
#>  1 …01_green.txt ab-0… data     ab-01_gr… ab-01          day01       <NA>       
#>  2 …b-01_red.txt ab-0… data     ab-01_re… ab-01          day01       <NA>       
#>  3 …01_green.txt ab-0… data     ab-01_gr… ab-01          day02       <NA>       
#>  4 …b-01_red.txt ab-0… data     ab-01_re… ab-01          day02       <NA>       
#>  5 …01_green.txt ab-0… data     ab-01_gr… ab-01          day03       <NA>       
#>  6 …02_green.txt ac-0… data     ac-02_gr… ac-02          day01       <NA>       
#>  7 …c-02_red.txt ac-0… data     ac-02_re… ac-02          day01       <NA>       
#>  8 …02_green.txt ac-0… data     ac-02_gr… ac-02          day02       <NA>       
#>  9 …c-02_red.txt ac-0… data     ac-02_re… ac-02          day02       <NA>       
#> 10 …02_green.txt ac-0… data     ac-02_gr… ac-02          day03       <NA>       
#> 11 …c-02_red.txt ac-0… data     ac-02_re… ac-02          day03       <NA>       
#> # ℹ 6 more variables: subject <chr>, time <chr>, task <chr>, template <chr>,
#> #   .ok <lgl>, .problems <list>
```

### Ignoring files and directory subtrees

Some files are intentionally outside the schema. Ignored files are
pruned from `ft_list()` and `ft_index()` by default. Ignored directory
templates prune every file below a matching directory.

``` r
files_to_check <- fs::path(
  ft$root,
  c(
    "ab-01/day01/ab-01_red.txt",
    "ab-01/day01/ab-01_notes.txt",
    "ab-01/tmp/not-a-data-file.txt"
  )
)

ft_ignored <- ft |>
  ft_ignore_file_template("data", "{subject}_notes.txt") |>
  ft_ignore_dir_template("time", "tmp")

ft_index(ft_ignored, files_to_check)
#> # A tibble: 1 × 13
#>   .path          .rel  at_layer .filename layer__subject layer__time layer__data
#>   <fs::path>     <chr> <chr>    <chr>     <chr>          <chr>       <chr>      
#> 1 …ab-01_red.txt ab-0… data     ab-01_re… ab-01          day01       <NA>       
#> # ℹ 6 more variables: subject <chr>, time <chr>, task <chr>, template <chr>,
#> #   .ok <lgl>, .problems <list>

ft_index(ft_ignored, files_to_check, include_ignored = TRUE)
#> # A tibble: 3 × 16
#>   .path          .rel  at_layer .filename layer__subject layer__time layer__data
#>   <fs::path>     <chr> <chr>    <chr>     <chr>          <chr>       <chr>      
#> 1 …ab-01_red.txt ab-0… data     ab-01_re… ab-01          day01       <NA>       
#> 2 …-01_notes.txt ab-0… data     ab-01_no… ab-01          day01       <NA>       
#> 3 …data-file.txt ab-0… data     not-a-da… ab-01          tmp         <NA>       
#> # ℹ 9 more variables: subject <chr>, time <chr>, task <chr>, template <chr>,
#> #   .ignored <lgl>, .ignore_template <chr>, .ignore_type <chr>, .ok <lgl>,
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

We reuse the same templates as earlier and find the bad file names:

``` r
ft <- "./inst/demo-2" |> 
  ft_init(c("subject", "time", "data")) |>
  ft_add_regex(c(
    subject = "\\w{2}-\\d{2}",
    time = "day\\d{2}",
    task = "red|green"
  )) |> 
  ft_add_dir_template("time", "{time}") |> 
  ft_add_dir_template("subject", "{subject}") |> 
  ft_add_file_template("data", "{subject}_{task}.txt")

ft |> 
  ft_index() |> 
  ft_glimpse_problems()
#> 4/11 files with 4 problems.
#> 
#> ab-01/day01 (`data` layer)
#> • ab-01_blue.txt: filename does not match a file template at layer `data`
#> 
#> ab-01/day02 (`data` layer)
#> • ac-01_red.txt: filename has `subject` "ac-01", but a parent directory has
#>   `subject` "ab-01"
#> 
#> ac-02/day3 (`data` layer)
#> • ac-02_green.txt: directory name 'day3' does not match a dir template at layer
#>   `time`
#> • ac-02_red.txt: directory name 'day3' does not match a dir template at layer
#>   `time`
```

### Template consistency and conditional templates

The following example demonstrates

- that fields need to be consistent along a path
- field regexes and file templates can be defined for only specific
  layer values

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

We can make templates apply conditionally by using `when = list(...)` to
say which layer values a template applies to. We can also conditionally
override field regexes using `with = c(...)`.

``` r
ft <- "./inst/demo-3" |> 
  ft_init(c("subject", "time", "data")) |>
  ft_add_regex(c(
    subject = "\\w{2}-\\d{2}",
    time = "\\d{2}",
    task = "red|green"
  )) |> 
  ft_add_dir_template("time", "day{time}") |> 
  ft_add_dir_template("subject", "{subject}") |> 
  ft_add_file_template(
    "data", 
    "{subject}_{time}_{task}.txt",
    # limit when this template is used
    when = list(time = c("01", "02")),
  ) |> 
  ft_add_file_template(
    "time", 
    "{subject}-manifest.txt"
  ) |> 
  ft_add_file_template(
    "data", 
    c(day03 = "{subject}_{time}_{task}.txt"),
    # limit when this template is used
    when = c(time = "03"),
    # temporarily override a field regex too
    with = c(task = "yellow")
  )
```

In the tree view, we can see multiple file templates in the `data`
layer:

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
#> • ab-02_02_yellow.txt: filename does not match a file template at layer `data`
#> 
#> ab-02/day03 (`data` layer)
#> • ab-02_03_green.txt: filename does not match a file template at layer `data`
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
#> Columns: 13
#> $ .path          <fs::path> "C:/Users/Tristan/Documents/GitRepos/filetree/inst…
#> $ .rel           <chr> "ab-01/ab-01-manifest.txt", "ab-01/day01/ab-01_01_green…
#> $ at_layer       <chr> "time", "data", "data", "data", "data", "data", "time",…
#> $ .filename      <chr> "ab-01-manifest.txt", "ab-01_01_green.txt", "ab-01_01_r…
#> $ layer__subject <chr> "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "…
#> $ layer__time    <chr> NA, "day01", "day01", "day02", "day02", "day03", NA, "d…
#> $ layer__data    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $ subject        <chr> "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "…
#> $ time           <chr> NA, "01", "01", "02", "02", "03", NA, "01", "01", "02",…
#> $ task           <chr> NA, "green", "red", "green", "red", "yellow", NA, "gree…
#> $ template       <chr> "default", "default", "default", "default", "default", …
#> $ .ok            <lgl> TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE…
#> $ .problems      <list> <NULL>, <NULL>, <NULL>, "filename has {.var time} {.val…
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
