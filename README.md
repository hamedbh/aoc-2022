Advent of Code 2022
================

- <a href="#day-1" id="toc-day-1">Day 1</a>
- <a href="#day-2" id="toc-day-2">Day 2</a>
- <a href="#day-3" id="toc-day-3">Day 3</a>
- <a href="#day-4" id="toc-day-4">Day 4</a>
- <a href="#day-5" id="toc-day-5">Day 5</a>
- <a href="#day-6" id="toc-day-6">Day 6</a>
- <a href="#day-7" id="toc-day-7">Day 7</a>

Here’s my work on Advent of Code 2022. I’ve never finished one of these,
perhaps this will be the year …

``` r
knitr::opts_chunk$set(echo = TRUE)
library(purrr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringi)
walk(list.files(here::here("R"), full.names = TRUE), source)
```

# Day 1

## Part 1

``` r
d01_input <- readLines(here::here("input/day_01.txt")) |> 
  as.integer()

d01_calories <- split(d01_input, cumsum(is.na(d01_input))) |> 
  map_int(sum, na.rm = TRUE)

d01_calories |> 
  max()
```

    ## [1] 67450

## Part 2

``` r
d01_calories |> 
  sort(decreasing = TRUE) |> 
  head(3) |> 
  sum()
```

    ## [1] 199357

# Day 2

## Part 1

It’s nice to have a chance to use `switch()`.

``` r
d02_input <- read.table(
  here::here("input/day_02.txt"), 
  sep = " ", 
  col.names = c("opp", "me")
) |> 
  as_tibble()

d02_input |> 
  mutate(
    opp_int = map_int(opp, ~ switch(.x, A = 1L, B = 2L, C = 3L)), 
    me_int = map_int(me, ~ switch(.x, X = 1L, Y = 2L, Z = 3L))
  ) |> 
  mutate(diff = (me_int - opp_int) %% 3) |> 
  # 2 = loss
  # 0 = draw
  # 1 = win
  mutate(score = c(3, 6, 0)[diff + 1L]) |> 
  summarise(total_score = sum(me_int + score))
```

    ## # A tibble: 1 × 1
    ##   total_score
    ##         <dbl>
    ## 1       13924

## Part 2

``` r
d02_input |> 
  mutate(
    opp_int = map_int(opp, ~ switch(.x, A = 1L, B = 2L, C = 3L)), 
    diff = map_int(me, ~ switch(.x, X = 2L, Y = 0L, Z = 1L))
  ) |> 
    mutate(
      # Is this the most elegant modular arithmetic? Almost certainly not. Is
      # there a simpler way to do this arithmetic that avoids quite so many
      # operations? Almost certainly yes.
      me_int = ((((opp_int + 1L) %% 3) + ((diff + 1L) %% 3)) %% 3) + 1L, 
      score = c(3, 6, 0)[diff + 1L]
    ) |> 
  summarise(total_score = sum(me_int + score))
```

    ## # A tibble: 1 × 1
    ##   total_score
    ##         <dbl>
    ## 1       13448

# Day 3

## Part 1

``` r
d03_input <- readLines(here::here("input/day_03.txt")) |> 
  strsplit("")

split_backpack <- function(b) {
  half <- length(b) / 2L
  list(head(b, half), tail(b, half))
}

scores <- c(letters, LETTERS)
d03_input |> 
  map(split_backpack) |> 
  map_chr(~ intersect(.x[[1]], .x[[2]])) |> 
  match(scores) |> 
  sum()
```

    ## [1] 7568

## Part 2

``` r
split(d03_input, ((seq_along(d03_input) + 2L) %/% 3)) |> 
  map_chr(
    ~ intersect(.x[[1]], .x[[2]]) |> 
      intersect(.x[[3]])
  ) |> 
  match(scores) |> 
  sum()
```

    ## [1] 2780

# Day 4

## Part 1

``` r
d04_input <- readLines(here::here("input/day_04.txt")) |> 
  strsplit("[,-]") |> 
  map(
    function(x) {
      list(
        left = seq(x[[1]], x[[2]]), 
        right = seq(x[[3]], x[[4]])
      )
    }
  )
d04_input |> 
  map_lgl(~ all(.x[[1]] %in% .x[[2]]) | all(.x[[2]] %in% .x[[1]])) |> 
  sum()
```

    ## [1] 602

## Part 2

``` r
d04_input |> 
  map_lgl(~ any(.x[[1]] %in% .x[[2]]) | any(.x[[2]] %in% .x[[1]])) |> 
  sum()
```

    ## [1] 891

# Day 5

This one was annoying, mostly because parsing the inputs was a pain. I
also failed to read the instructions for part 1 correctly, which meant I
was solving part 2 without realising it.

## Part 1

``` r
d05_input <- readLines(here::here("input/day_05.txt"))
d05_crates <- d05_input[seq_len(which(d05_input == "") - 2L)]
d05_steps <- d05_input[seq(which(d05_input == "") + 1L, length(d05_input))] |> 
  map(
    ~ stri_extract_all_regex(.x, "\\d+", simplify = TRUE)[1, ] |> 
      as.integer()
  )

d05_crate_matrix <- map(
  d05_crates, 
  function(crate) {
    seq(from = 2, by = 4, length.out = 9) |> 
      map_chr(~ stri_sub(crate, .x, .x))
  }
) |> 
  reduce(rbind) |> 
  t()

d05_stacks <- map(
  seq_len(9),
  ~ grep("[A-Z]", d05_crate_matrix[.x, ], value = TRUE) |> 
    unname()
)

# If I'm going to have to write a loop, let's at least put it inside a function.
d05_restack <- function(stacks, steps, part1 = TRUE) {
  for (step in steps) {
    n <- step[[1]]
    from <- step[[2]]
    to <- step[[3]]
    if (isTRUE(part1)) {
      moving <- rev(stacks[[from]][seq_len(n)])
    } else {
      moving <- stacks[[from]][seq_len(n)]
    }
    stacks[[from]] <- 
      stacks[[from]][seq(n + 1, length(stacks[[from]]))]
    stacks[[to]] <- c(moving, stacks[[to]])
  }
  stacks |> 
    map_chr(~ .x[[1]]) |> 
    stri_c(collapse = "")
}
d05_restack(d05_stacks, d05_steps, part1 = TRUE)
```

    ## [1] "VWLCWGSDQ"

## Part 2

``` r
d05_restack(d05_stacks, d05_steps, part1 = FALSE)
```

    ## [1] "TCGLQSLPW"

# Day 6

First `while` loop of the year. At least it’s enclosed in a function.

## Part 1

``` r
d06_input <- readLines(here::here("input/day_06.txt")) |> 
  strsplit("") |> 
  pluck(1)

d06_find_marker <- function(d06_input, type = c("packet", "message")) {
  type <- match.arg(type)
  gap <- switch(type, packet = 3L, message = 13L)
  i <- 0L
  marker <- FALSE
  while (!marker) {
    i <- i + 1L
    marker <- !(any(duplicated(d06_input[seq(i, i + gap)])))
  }
  i + gap
}
d06_find_marker(d06_input, "packet")
```

    ## [1] 1566

## Part 2

``` r
d06_find_marker(d06_input, "message")
```

    ## [1] 2265

# Day 7

This one was really annoying, did not care for it.

## Part 1

``` r
d07_input <- readLines(here::here("input/day_07.txt"))
d07_res <- d07_current <- double()

for (line in d07_input) {
  if (line == "$ ls") {
    d07_current <- c(d07_current, 0)
  } else if (grepl("^\\d", line)) {
    d07_current <- d07_current + as.double(gsub("\\D", "", line))
  } else if (line == "$ cd ..") {
    d07_res <- c(d07_res, tail(d07_current, 1))
    d07_current <- head(d07_current, -1)
  }
}
d07_res <- c(d07_current, d07_res)
sum(d07_res[d07_res < 100000])
```

    ## [1] 1307902

## Part 2

``` r
min(d07_res[d07_res > (30000000 - (70000000 - d07_res[[1]]))])
```

    ## [1] 7068748
