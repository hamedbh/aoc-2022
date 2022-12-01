Advent of Code 2022
================

- <a href="#day-1" id="toc-day-1">Day 1</a>

``` r
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

Here’s my work on Advent of Code 2022.

# Day 1

## Part 1

``` r
d01_input <- tibble(calories = readLines(here::here("input/day_01.txt"))) |> 
  mutate(elf = cumsum(calories == "") + 1) |> 
  filter(calories != "") |> 
  mutate(across(calories, as.integer)) |> 
  group_by(elf) |> 
  summarise(across(calories, sum))

d01_input |> 
  slice_max(calories, n = 1)
```

    ## # A tibble: 1 × 2
    ##     elf calories
    ##   <dbl>    <int>
    ## 1   184    67450

## Part 2

``` r
d01_input |> 
  slice_max(calories, n = 3) |> 
  summarise(across(calories, sum))
```

    ## # A tibble: 1 × 1
    ##   calories
    ##      <int>
    ## 1   199357
