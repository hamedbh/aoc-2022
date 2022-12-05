Advent of Code 2022
================

- <a href="#day-1" id="toc-day-1">Day 1</a>
- <a href="#day-2" id="toc-day-2">Day 2</a>
- <a href="#day-3" id="toc-day-3">Day 3</a>
- <a href="#day-4" id="toc-day-4">Day 4</a>
- <a href="#day-5" id="toc-day-5">Day 5</a>

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
  vapply(sum, integer(1L), na.rm = TRUE)

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

## Part 1

``` r
d05_input <- readLines(here::here("input/day_05.txt"))
d05_crates <- d05_input[seq_len(which(d05_input == "") - 2L)]
d05_procs <- d05_input[seq(which(d05_input == "") + 1L, length(d05_input))] |> 
  map(
    ~ stri_extract_all_regex(.x, "\\d+", simplify = TRUE)[1, ] |> 
      as.integer()
  )

d05_crate_matrix <- seq(from = 2, by = 4, length.out = 9) |> 
  map(~ substr(d05_crates, .x, .x)) |> 
  reduce(rbind) |> 
  t()
d05_stacks <- map(
  seq_len(9),
  ~ grep("[A-Z]", d05_crate_matrix[, .x], value = TRUE)
)
```

``` r
for (proc in d05_procs) {
  n <- proc[[1]]
  from <- proc[[2]]
  to <- proc[[3]]
  moving <- d05_stacks[[from]][seq_len(n)]
  d05_stacks[[from]] <- 
    d05_stacks[[from]][seq(n + 1, length(d05_stacks[[from]]))]
  d05_stacks[[to]] <- c(moving, d05_stacks[[to]])
}
# d05_restack <- function(stacks, procs) {
#   
# }

d05_stacks |> 
  map_chr(~ .x[[1]]) |> 
  stri_c(collapse = "")
```

    ## [1] "TCGLQSLPW"

## Part 2
