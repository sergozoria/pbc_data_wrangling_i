Data Import - Data Wrangling I
================
September 16th, 2025

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.2
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(haven)
```

Let’s import a dataset

``` r
litters_df =
  read_csv("data/FAS_litters.csv")
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): Group, Litter Number, GD0 weight, GD18 weight
    ## dbl (4): GD of Birth, Pups born alive, Pups dead @ birth, Pups survive
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Always look at your data!

``` r
names(litters_df)
```

    ## [1] "Group"             "Litter Number"     "GD0 weight"       
    ## [4] "GD18 weight"       "GD of Birth"       "Pups born alive"  
    ## [7] "Pups dead @ birth" "Pups survive"

Update the names in ‘litters_df’

``` r
litters_df = 
  janitor::clean_names(litters_df)
```

``` r
litters_df
```

    ## # A tibble: 49 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>           <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con7  #85             19.7       34.7                 20               3
    ##  2 Con7  #1/2/95/2       27         42                   19               8
    ##  3 Con7  #5/5/3/83/3-3   26         41.4                 19               6
    ##  4 Con7  #5/4/2/95/2     28.5       44.1                 19               5
    ##  5 Con7  #4/2/95/3-3     <NA>       <NA>                 20               6
    ##  6 Con7  #2/2/95/3-2     <NA>       <NA>                 20               6
    ##  7 Con7  #1/5/3/83/3-3/2 <NA>       <NA>                 20               9
    ##  8 Con8  #3/83/3-3       <NA>       <NA>                 20               9
    ##  9 Con8  #2/95/3         <NA>       <NA>                 20               8
    ## 10 Con8  #3/5/2/2/95     28.5       <NA>                 20               8
    ## # ℹ 39 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

Sometimes skimming data is near?

``` r
skimr::skim(litters_df)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | litters_df |
| Number of rows                                   | 49         |
| Number of columns                                | 8          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 4          |
| numeric                                          | 4          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| group         |         0 |          1.00 |   4 |   4 |     0 |        6 |          0 |
| litter_number |         0 |          1.00 |   3 |  15 |     0 |       49 |          0 |
| gd0_weight    |        13 |          0.73 |   1 |   4 |     0 |       26 |          0 |
| gd18_weight   |        15 |          0.69 |   1 |   4 |     0 |       31 |          0 |

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate |  mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:----------------|----------:|--------------:|------:|-----:|----:|----:|----:|----:|-----:|:------|
| gd_of_birth     |         0 |             1 | 19.65 | 0.48 |  19 |  19 |  20 |  20 |   20 | ▅▁▁▁▇ |
| pups_born_alive |         0 |             1 |  7.35 | 1.76 |   3 |   6 |   8 |   8 |   11 | ▁▃▂▇▁ |
| pups_dead_birth |         0 |             1 |  0.33 | 0.75 |   0 |   0 |   0 |   0 |    4 | ▇▂▁▁▁ |
| pups_survive    |         0 |             1 |  6.41 | 2.05 |   1 |   5 |   7 |   8 |    9 | ▁▃▂▇▇ |

Let’s now fix the missing data vlue

``` r
read_csv("data/FAS_litters.csv", na = c("NA", ".", ""), skip = 5)
```

    ## New names:
    ## Rows: 44 Columns: 8
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (2): Con7, #4/2/95/3-3 dbl (6): NA...3, NA...4, 20, 6...6, 0, 6...8
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `NA` -> `NA...3`
    ## • `NA` -> `NA...4`
    ## • `6` -> `6...6`
    ## • `6` -> `6...8`

    ## # A tibble: 44 × 8
    ##    Con7  `#4/2/95/3-3`   NA...3 NA...4  `20` `6...6`   `0` `6...8`
    ##    <chr> <chr>            <dbl>  <dbl> <dbl>   <dbl> <dbl>   <dbl>
    ##  1 Con7  #2/2/95/3-2       NA       NA    20       6     0       4
    ##  2 Con7  #1/5/3/83/3-3/2   NA       NA    20       9     0       9
    ##  3 Con8  #3/83/3-3         NA       NA    20       9     1       8
    ##  4 Con8  #2/95/3           NA       NA    20       8     0       8
    ##  5 Con8  #3/5/2/2/95       28.5     NA    20       8     0       8
    ##  6 Con8  #5/4/3/83/3       28       NA    19       9     0       8
    ##  7 Con8  #1/6/2/2/95-2     NA       NA    20       7     0       6
    ##  8 Con8  #3/5/3/83/3-3-2   NA       NA    20       8     0       8
    ##  9 Con8  #2/2/95/2         NA       NA    19       5     0       4
    ## 10 Con8  #3/6/2/2/95-3     NA       NA    20       7     0       7
    ## # ℹ 34 more rows

``` r
pups_df = 
  read_csv("data/FAS_pups.csv",
           skip = 3,
           na = c("NA", ".", "")
  )
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pups_df = 
  janitor::clean_names(pups_df)
```

Alright, what about excel

CSVs are really great but sometimes you get an excel file. Givne the
option, always use the csv files.

``` r
mlb_df =
  read_excel("data/mlb11.xlsx")
```

Import LotR word counts

``` r
fotr_df =
  read_excel("data/LotR_Words.xlsx", range = "B3:D6")
```

let’s now read a SAS file. We’ll now import the pulse dataset

``` r
pulse_df =
  read_sas("data/public_pulse_data.sas7bdat")

pulse_df =
  janitor::clean_names(pulse_df)
```

why do I hate read read.csv so much???

``` r
litters_df_base =
  read.csv("data/FAS_litters.csv")
```

what about data exporting

``` r
write_csv(fotr_df, "data/fort_df.csv")
```
