# Aggregate `data` by `response`, `confidence`, and other columns

Counts number of rows in `data` with unique combinations values in the
columns `response`, `confidence`, and any other columns in `...`.

## Usage

``` r
aggregate_metad(
  data,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  .name = "N",
  K = NULL
)
```

## Arguments

- data:

  The data frame to aggregate

- ...:

  Grouping columns in `data`. These columns will be converted to
  factors.

- .stimulus:

  The name of "stimulus" column

- .response:

  The name of "response" column

- .confidence:

  The name of "confidence" column

- .joint_response:

  The name of "joint_response" column

- .name:

  The name of the resulting column containing trial counts

- K:

  The number of confidence levels in `data`. If `NULL`, this is
  estimated from `data`.

## Value

A tibble with one row per combination of the variables in `...`, and
another column named by the value of `.response` containing trial
counts. For \\K\\ confidence levels, this will be an \\N \times K\*4\\
matrix, such that the columns represent (for stimulus \\S\\, type 1
response \\R\\, and type 2 response \\C\\): \$\$ \[N\_{S=0, R=0, C=K},
\ldots, N\_{S=0, R=0, C=1}, \\ N\_{S=0, R=1, C=1}, \ldots, N\_{S=0, R=1,
C=K}, \\ N\_{S=1, R=0, C=K}, \ldots, N\_{S=1, R=0, C=1}, \\ N\_{S=1,
R=1, C=1}, \ldots, N\_{S=1, R=1, C=K}\] \\ \$\$

## Details

The data frame `data` must have one column with the name given by
`.stimulus`. Additionally, it must have either:

- Two columns with names given by `.response` and `.confidence`

- One column with the name given by `.joint_response`

Finally, it must also have columns for any additional variables in
`...`.

## Examples

``` r
# aggregate a dataset without grouping factors
d <- sim_metad()
aggregate_metad(d)
#> # A tibble: 1 × 3
#>     N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"] [,"N_0_6"]
#>   <int> <int>       <int>      <int>      <int>      <int>      <int>      <int>
#> 1    50    50           8          5          4         12         10          7
#> # ℹ 1 more variable: N[7:16] <int>

# aggregate a dataset with grouping factors
d2 <- sim_metad_condition()
aggregate_metad(d2, condition)
#> # A tibble: 2 × 4
#>   condition   N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"]
#>   <fct>     <int> <int>       <int>      <int>      <int>      <int>      <int>
#> 1 1            50    50           7          4         10         16          5
#> 2 2            50    50           3          6         10         13          9
#> # ℹ 1 more variable: N[6:16] <int>

# can also aggregate ignoring grouping factors
aggregate_metad(d2)
#> # A tibble: 1 × 3
#>     N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"] [,"N_0_6"]
#>   <int> <int>       <int>      <int>      <int>      <int>      <int>      <int>
#> 1   100   100          10         10         20         29         14          9
#> # ℹ 1 more variable: N[7:16] <int>

# aggregate data with only `joint_response` column
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
d |>
  ungroup() |>
  mutate(joint_response = joint_response(
    response, confidence,
    n_distinct(confidence)
  )) |>
  select(-response, -confidence) |>
  aggregate_metad()
#> # A tibble: 1 × 3
#>     N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"] [,"N_0_6"]
#>   <int> <int>       <int>      <int>      <int>      <int>      <int>      <int>
#> 1    50    50           8          5          4         12         10          7
#> # ℹ 1 more variable: N[7:16] <int>
```
