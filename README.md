
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filetree

<!-- badges: start -->

<!-- badges: end -->

A (currently) largely vibecoded package for declarative filetrees. This
package is mostly a proof-of-concept or API experiment.

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
#> # A tibble: 11 × 10
#>    .path         .rel       at_layer subject time  data  pattern .ok   .problems
#>    <fs::path>    <fs::path> <chr>    <chr>   <chr> <chr> <chr>   <lgl> <list>   
#>  1 …01_green.txt …green.txt data     ab-01   day01 ab-0… default TRUE  <chr [0]>
#>  2 …b-01_red.txt …1_red.txt data     ab-01   day01 ab-0… default TRUE  <chr [0]>
#>  3 …01_green.txt …green.txt data     ab-01   day02 ab-0… default TRUE  <chr [0]>
#>  4 …b-01_red.txt …1_red.txt data     ab-01   day02 ab-0… default TRUE  <chr [0]>
#>  5 …01_green.txt …green.txt data     ab-01   day03 ab-0… default TRUE  <chr [0]>
#>  6 …02_green.txt …green.txt data     ac-02   day01 ac-0… default TRUE  <chr [0]>
#>  7 …c-02_red.txt …2_red.txt data     ac-02   day01 ac-0… default TRUE  <chr [0]>
#>  8 …02_green.txt …green.txt data     ac-02   day02 ac-0… default TRUE  <chr [0]>
#>  9 …c-02_red.txt …2_red.txt data     ac-02   day02 ac-0… default TRUE  <chr [0]>
#> 10 …02_green.txt …green.txt data     ac-02   day03 ac-0… default TRUE  <chr [0]>
#> 11 …c-02_red.txt …2_red.txt data     ac-02   day03 ac-0… default TRUE  <chr [0]>
#> # ℹ 1 more variable: task <chr>
```

For comparison, here is a file tree with some problems. There is

- a misformatted day folder
- a file with the wrong subject
- a file with a nonexisting task (“blue”)

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
  dplyr::filter(!.ok) |> 
  split(~.rel) |> 
  lapply(dplyr::pull, .problems)
#> $`ab-01/day01/ab-01_blue.txt`
#> $`ab-01/day01/ab-01_blue.txt`[[1]]
#> [1] "file 'ab-01_blue.txt' matches no pattern at_layer='data'"
#> 
#> 
#> $`ab-01/day02/ac-01_red.txt`
#> $`ab-01/day02/ac-01_red.txt`[[1]]
#> [1] "file capture subject='ac-01' conflicts with subject='ab-01'"
#> 
#> 
#> $`ac-02/day3/ac-02_green.txt`
#> $`ac-02/day3/ac-02_green.txt`[[1]]
#> [1] "directory time='day3' matches no pattern"
#> 
#> 
#> $`ac-02/day3/ac-02_red.txt`
#> $`ac-02/day3/ac-02_red.txt`[[1]]
#> [1] "directory time='day3' matches no pattern"
```

It would be nice to

- [ ] check inventory/completeness. (Did you notice a missing “red” file
  in the first tree?)

- [ ] constrain parent folder. (Maybe a “yellow” is given on and only on
  day 3.) Or is that more of a dplyr-layer move for validation?
