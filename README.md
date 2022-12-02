Advent of Code 2022
================

- <a href="#day-1" id="toc-day-1">Day 1</a>
- <a href="#day-2" id="toc-day-2">Day 2</a>

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
