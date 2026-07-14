
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filetree

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjmahr/filetree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjmahr/filetree/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package for declarative filetrees with filename validation and
parsing.

## Installation

Install the development version of filetree from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tjmahr/filetree")
```

## Example (filetree with no problems)

Let’s say that in our setting, we collect raw data files, process the
data and deposit the files into folders on a shared network drive. We
want to make sure that the directory names and filenames in this
directory are consistently named and correctly organized. With filetree,
we describe how these files should be organized and it checks for any
violations to this file organization scheme.

Here is some example longitudinal data organized by participant, time,
and task.

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

The subject identifiers have a pattern (two letters + hyphen + number),
and these identifiers appear in the filenames in the task folder.
Therefore, one thing filetree should check is that the subject
identifiers in the parent folder and in the filename agree.

We create a filetree with `ft_init()`. Each *layer* is a level of
hierarchy. At the root, when we list the folders, we see subjects. So,
the first layer is `"subject"`. Inside of a subject folder, when we list
the folders, we see time points. The layer is therefore `"time"`.
Finally, in the last level, we see our main task files, so we call this
layer `"task"`.

``` r
library(filetree)

ft <- ft_init(
  root = "./inst/demo-1", 
  layers = c("subject", "time", "task")
)
ft
#> <<filetree>> root: C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#> layers: subject / time / task
#> regex_pool: <empty>
#> dir_templates: <none>
#> file_templates: <none>
#> ignore_dir_templates: <none>
#> ignore_file_templates: <none>
```

We define the file organization rules (*schema*) with two ingredients:

1.  field *regexes* define substring values to extract,
2.  *templates* arrange the fields with fixed text.

As a slogan: Regexes define fields, and templates arrange fields.

Regexes are registered with `ft_add_regex()` by using a named character
vector.

``` r
ft <- ft |>
  ft_add_regex(c(
    subject = "\\w{2}-\\d{2}",
    time = "day\\d{2}",
    task = "red|green"
  )) 
ft
#> <<filetree>> root: C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#> layers: subject / time / task
#> regex_pool: 3 (subject, time, task)
#> dir_templates: <none>
#> file_templates: <none>
#> ignore_dir_templates: <none>
#> ignore_file_templates: <none>
```

Templates are added with `ft_add_dir_template()` or
`ft_add_file_template()`. We can refer to the fields from the regex pool
by using `{regex_name}`. Thus, files at the task layer have the template
`"{subject}_{task}.txt"` which then gets expanded (by filetree) into the
regex: `"^(\\w{2}-\\d{2})_(red|green)[.]txt$"`.

``` r
ft <- ft |>
  ft_add_dir_template(
    layer = "time", 
    template = "{time}"
  ) |> 
  ft_add_dir_template(
    layer = "subject", 
    template = "{subject}"
  ) |> 
  ft_add_file_template(
    layer = "task",  
    template = "{subject}_{task}.txt"
  )
ft
#> <<filetree>> root: C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#> layers: subject / time / task
#> regex_pool: 3 (subject, time, task)
#> dir_templates:
#> • subject: default = `{subject}`
#> • time: default = `{time}`
#> file_templates:
#> • task: default = `{subject}_{task}.txt`
#> ignore_dir_templates: <none>
#> ignore_file_templates: <none>
```

Templates match full directory names or filenames, so `"day{time}"`
matches `"day01"` when `time = "\\d{2}"`, but not `"day01b"`. Templates
are also literal or fixed text patterns: `.txt` is a real extension
here. In other words, regexes are not allowed in the templates except
through `{}` fields.

We can view the filetree schema as a tree:

``` r
ft |> ft_schema_tree()
#> C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#> └── subject: {subject}
#>     └── time: {time}
#>         └── `task` file: {subject}_{task}.txt
```

Or as a flat list:

``` r
ft |> ft_schema_flat()
#> . / subject
#>   dirs: default = `{subject}`
#> 
#> . / subject / time
#>   dirs: default = `{time}`
#> 
#> . / subject / time / task
#>   files: default = `{subject}_{task}.txt`
```

We can retrieve the list of filenames in the filetree with `ft_list()`:

``` r
# Look at the files
ft |> ft_list() |> fs::path_rel()
#> inst/demo-1/ab-01/day01/ab-01_green.txt inst/demo-1/ab-01/day01/ab-01_red.txt   
#> inst/demo-1/ab-01/day02/ab-01_green.txt inst/demo-1/ab-01/day02/ab-01_red.txt   
#> inst/demo-1/ab-01/day03/ab-01_green.txt inst/demo-1/ac-02/day01/ac-02_green.txt 
#> inst/demo-1/ac-02/day01/ac-02_red.txt   inst/demo-1/ac-02/day02/ac-02_green.txt 
#> inst/demo-1/ac-02/day02/ac-02_red.txt   inst/demo-1/ac-02/day03/ac-02_green.txt 
#> inst/demo-1/ac-02/day03/ac-02_red.txt
```

We can parse and validate the file names with `ft_index()`. This
function returns one row per file with columns about the file’s layer,
template, and validation status:

``` r
ft |> ft_index()
#> # A tibble: 11 × 13
#>    .path                  .rel  .at_layer .filename .layer__subject .layer__time
#>    <fs::path>             <chr> <chr>     <chr>     <chr>           <chr>       
#>  1 …day01/ab-01_green.txt ab-0… task      ab-01_gr… ab-01           day01       
#>  2 …1/day01/ab-01_red.txt ab-0… task      ab-01_re… ab-01           day01       
#>  3 …day02/ab-01_green.txt ab-0… task      ab-01_gr… ab-01           day02       
#>  4 …1/day02/ab-01_red.txt ab-0… task      ab-01_re… ab-01           day02       
#>  5 …day03/ab-01_green.txt ab-0… task      ab-01_gr… ab-01           day03       
#>  6 …day01/ac-02_green.txt ac-0… task      ac-02_gr… ac-02           day01       
#>  7 …2/day01/ac-02_red.txt ac-0… task      ac-02_re… ac-02           day01       
#>  8 …day02/ac-02_green.txt ac-0… task      ac-02_gr… ac-02           day02       
#>  9 …2/day02/ac-02_red.txt ac-0… task      ac-02_re… ac-02           day02       
#> 10 …day03/ac-02_green.txt ac-0… task      ac-02_gr… ac-02           day03       
#> 11 …2/day03/ac-02_red.txt ac-0… task      ac-02_re… ac-02           day03       
#> # ℹ 7 more variables: .layer__task <chr>, subject <chr>, time <chr>,
#> #   task <chr>, .file_template <chr>, .ok <lgl>, .problems <list>

ft |> ft_index() |> dplyr::glimpse()
#> Rows: 11
#> Columns: 13
#> $ .path           <fs::path> "C:/Users/Tristan/Documents/GitRepos/filetree/ins…
#> $ .rel            <chr> "ab-01/day01/ab-01_green.txt", "ab-01/day01/ab-01_red.…
#> $ .at_layer       <chr> "task", "task", "task", "task", "task", "task", "task"…
#> $ .filename       <chr> "ab-01_green.txt", "ab-01_red.txt", "ab-01_green.txt",…
#> $ .layer__subject <chr> "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "ac-02", …
#> $ .layer__time    <chr> "day01", "day01", "day02", "day02", "day03", "day01", …
#> $ .layer__task    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $ subject         <chr> "ab-01", "ab-01", "ab-01", "ab-01", "ab-01", "ac-02", …
#> $ time            <chr> "day01", "day01", "day02", "day02", "day03", "day01", …
#> $ task            <chr> "green", "red", "green", "red", "green", "green", "red…
#> $ .file_template  <chr> "default", "default", "default", "default", "default",…
#> $ .ok             <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
#> $ .problems       <list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>…
```

All `.`-initial columns were generated by filetree:

- `.path` - full file path.
- `.rel` - relative path
- `.at_layer` - name of the layer containing the file
- `.filename` - base filename
- `.layer__subject`, `.layer__time`, `.layer__task` - folder matched at
  the `subject`, `time`, `task` layers.
- `.file_template` - name of the file template from
  `ft_add_file_template()` that matched this file
- `.ok` - whether the filepath passed the schema checks (`TRUE`) or
  whether there was a problem (`FALSE`).
- `.problems` - list column containing diagnostic messages about each
  file.

The other columns here are the regex fields extracted from each
filepath:

- `subject`
- `time`
- `task`

## Example (filetree with problems)

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

We reuse the same templates as earlier.

``` r
ft2 <- "./inst/demo-2" |> 
  ft_init(c("subject", "time", "task")) |>
  ft_add_regex(c(
    subject = "\\w{2}-\\d{2}",
    time = "day\\d{2}",
    task = "red|green"
  )) |> 
  ft_add_dir_template("subject", "{subject}") |> 
  ft_add_dir_template("time", "{time}") |> 
  ft_add_file_template("task", "{subject}_{task}.txt")
```

Now, the `.ok` column from `ft_index()` indicates problems:

``` r
ft2 |> 
  ft_index() |> 
  subset(!.ok)
#> # A tibble: 4 × 13
#>   .path      .rel  .at_layer .filename .layer__subject .layer__time .layer__task
#>   <fs::path> <chr> <chr>     <chr>     <chr>           <chr>        <chr>       
#> 1 …_blue.txt ab-0… task      ab-01_bl… ab-01           day01        <NA>        
#> 2 …1_red.txt ab-0… task      ac-01_re… ab-01           day02        <NA>        
#> 3 …green.txt ac-0… task      ac-02_gr… ac-02           day3         <NA>        
#> 4 …2_red.txt ac-0… task      ac-02_re… ac-02           day3         <NA>        
#> # ℹ 6 more variables: subject <chr>, time <chr>, task <chr>,
#> #   .file_template <chr>, .ok <lgl>, .problems <list>
```

The `.problems` column contains formatted messages about each file, so
these are best viewed with `ft_glimpse_problems()`:

``` r
ft2 |> 
  ft_index() |> 
  ft_glimpse_problems()
#> 4/11 files with 4 problems.
#> 
#> ab-01/day01 (`task` layer)
#> • ab-01_blue.txt: filename does not match a file template at layer `task`
#> 
#> ab-01/day02 (`task` layer)
#> • ac-01_red.txt: filename has `subject` "ac-01", but a parent directory has
#>   `subject` "ab-01"
#> 
#> ac-02/day3 (`task` layer)
#> • ac-02_green.txt: directory name 'day3' does not match a dir template at layer
#>   `time`
#> • ac-02_red.txt: directory name 'day3' does not match a dir template at layer
#>   `time`
```

### Template consistency and conditional templates

We are going to add three new complications to the ongoing example.

1.  A new “manifest” file appears at the `time` layer. This is a
    “sidecar” file. It can appear in a layer alongside directories.
2.  `task`-layer files now include `time` values with the template
    `"{subject}_{time}_{task}.txt"`.
3.  There is a `"yellow"` task that only appears on `day03`.

Each of these new additions is broken somehow in the following example:

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
restrict a template to certain field values. We can also conditionally
override field regexes using `with = c(...)`.

``` r
ft3 <- "./inst/demo-3" |> 
  ft_init(c("subject", "time", "task")) |>
  ft_add_regex(c(
    subject = "\\w{2}-\\d{2}",
    time = "\\d{2}",
    task = "red|green"
  )) |> 
  ft_add_dir_template("time", "day{time}") |> 
  ft_add_dir_template("subject", "{subject}") |> 
  ft_add_file_template(
    "task", 
    "{subject}_{time}_{task}.txt",
    # limit when this template is used
    when = list(time = c("01", "02")),
  ) |> 
  ft_add_file_template(
    "time", 
    "{subject}-manifest.txt"
  ) |> 
  ft_add_file_template(
    "task", 
    c(day03 = "{subject}_{time}_{task}.txt"),
    # limit when this template is used
    when = c(time = "03"),
    # temporarily override a field regex too
    with = c(task = "yellow")
  )
ft3
#> <<filetree>> root: C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-3
#> layers: subject / time / task
#> regex_pool: 3 (subject, time, task)
#> dir_templates:
#> • subject: default = `{subject}`
#> • time: default = `day{time}`
#> file_templates:
#> • time: default = `{subject}-manifest.txt`
#> • task: default = `{subject}_{time}_{task}.txt`, day03 =
#> `{subject}_{time}_{task}.txt`
#> ignore_dir_templates: <none>
#> ignore_file_templates: <none>
```

When using an alternative or additional template, it can be helpful to
use a named character vector like
`c(day03 = "{subject}_{time}_{task}.txt")` to indicate something about
the nature of this special case. These template names are provided in
the schema views of the filetree:

``` r
ft_schema_tree(ft3)
#> C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-3
#> └── subject: {subject}
#>     ├── `time` file: {subject}-manifest.txt
#>     └── time: day{time}
#>         ├── `task` file: default = {subject}_{time}_{task}.txt [when time in 01, 02]
#>         └── `task` file: day03 = {subject}_{time}_{task}.txt [when time == 03; with task = yellow]

ft_schema_flat(ft3)
#> . / subject
#>   dirs: default = `{subject}`
#> 
#> . / subject / time
#>   dirs: default = `day{time}`
#>   files: default = `{subject}-manifest.txt`
#> 
#> . / subject / time / task
#>   files: default = `{subject}_{time}_{task}.txt` [when time in 01, 02]
#>          day03 = `{subject}_{time}_{task}.txt` [when time == 03; with task = yellow]
```

We should find our five problems:

``` r
ft3 |> 
  ft_index() |> 
  ft_glimpse_problems()
#> 5/13 files with 5 problems.
#> 
#> ab-01/day02 (`task` layer)
#> • ab-01_01_green.txt: filename has `time` "01", but a parent directory has
#>   `time` "02"
#> • ab-01_01_red.txt: filename has `time` "01", but a parent directory has `time`
#>   "02"
#> 
#> ab-02 (`time` layer)
#> • aa-02-manifest.txt: filename has `subject` "aa-02", but a parent directory
#>   has `subject` "ab-02"
#> 
#> ab-02/day02 (`task` layer)
#> • ab-02_02_yellow.txt: filename does not match a file template at layer `task`
#> 
#> ab-02/day03 (`task` layer)
#> • ab-02_03_green.txt: filename does not match a file template at layer `task`
```

## Additional package usage notes

### `ft_index()` can take a vector of file paths

By default, `ft_index()` searches for files:

``` r
nrow(ft_index(ft))
#> [1] 11
```

But if that file-listing is time consuming (as in a network location),
we can store the filename beforehand and pass it into `ft_index()`:

``` r
files_to_check <- ft_list(ft)
nrow(ft_index(ft, files_to_check))
#> [1] 11
```

### Integer layers

Layer arguments also accept positive integer positions. Layer `1` is the
first configured layer, while layer `0` is the implicit root and cannot
be used for template registration.

``` r
ft |>
  ft_add_dir_template(
    layer = 2, 
    template = c(extra = "__{time}")
  )
#> <<filetree>> root: C:/Users/Tristan/Documents/GitRepos/filetree/inst/demo-1
#> layers: subject / time / task
#> regex_pool: 3 (subject, time, task)
#> dir_templates:
#> • subject: default = `{subject}`
#> • time: default = `{time}`, extra = `__{time}`
#> file_templates:
#> • task: default = `{subject}_{task}.txt`
#> ignore_dir_templates: <none>
#> ignore_file_templates: <none>
```

I added this convention after having a case where there was a “happy”
data path organization like `. / {speaker}` / `{task}` and a misfit path
`. / _excluded / {speaker} / {task}` so it became easier to refer to the
misfit files’ layer depth using numbers.

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
  ft_ignore_file_template("task", "{subject}_notes.txt") |>
  ft_ignore_dir_template("time", "tmp")

ft_ignored |> 
  ft_index(files_to_check) |> 
  nrow()
#> [1] 1

ft_ignored |> 
  ft_index(files_to_check, include_ignored = TRUE) |> 
  nrow()
#> [1] 3
```

### Strict mode

There is an additional `strict` mode for `ft_index()`. By default, when
no templates are defined for a file in a layer, those files are
accepted.

``` r
ft4 <- "./inst/demo-1" |> 
  ft_init(
    layers = c("subject", "time", "task")
  )

files_to_check <- fs::path(
  ft4$root,
  c("ab-01/day01/ab-01_red.txt", "ab-01/sidecar.txt")
)

ft4 |> 
  ft_index(files_to_check) |> 
  ft_glimpse_problems()
#> 0/2 files with 0 problems.
```

When `strict` is set to `TRUE`, then every file (row) must have a
template associated with it:

``` r
ft4 |> 
  ft_index(files_to_check, strict = TRUE) |> 
  ft_glimpse_problems()
#> 2/2 files with 2 problems.
#> 
#> ab-01/day01 (`task` layer)
#> • ab-01_red.txt: no file templates registered for `task` files
#> 
#> ab-01 (`time` layer)
#> • sidecar.txt: no file templates registered for `time` files
```

My rationale with strict mode is that every file in the tree must be
accounted for somehow.

## LLM-assisted development

This package has been developed using ChatGPT/Codex with the
[obra/superpowers](https://github.com/obra/superpowers) and the
[posit-dev/skills](https://github.com/posit-dev/skills) skills
installed. Agents track their notes in `inst/`. This package is my first
time using LLM agents. The agents are not supposed to edit README.Rmd,
so this document provides the final word on package functionality after
R package tests and checks clear.

## Current impressions

I have been using this tool to validate our speech corpus data on our
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
