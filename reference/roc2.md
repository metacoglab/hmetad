# Calculate empirical type 2 receiver operating characteristic curves

Given a dataset `data`, determine the cumulative probability of each
type 2 responses conditional on accuracy, optionally conditional on type
1 response.

## Usage

``` r
roc2(
  data,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  K = NULL,
  bounds = FALSE,
  by_response = TRUE
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

- K:

  The number of confidence levels in `data`. If `NULL`, this is
  estimated from `data` using the maximum value of either the confidence
  column or joint response column.

- bounds:

  If `TRUE`, include the endpoints of the ROC at \\(0, 0)\\ and \\(1,
  1)\\. Otherwise, the endpoints are excluded.

- by_response:

  If `TRUE` (default), calculate type 2 ROCs conditional on type 1
  response.

## Value

A tibble with columns:

- `...`: the grouping columns in `data`

- `{.response}` (if `by_response=TRUE`): the type 1 response

- `{.confidence}`: the type 2 response

- `n_0`: the number of rows in `data` with `stimulus=0` and the
  corresponding `joint_response`

- `n_1`: the number of rows in `data` with `stimulus=1` and the
  corresponding `joint_response`

- `p_0`: for incorrect trials, the proportion of rows in `data` with
  confidence equal to `confidence`

- `p_1`: for correct trials the proportion of rows in `data` with
  confidence equal to `confidence`

- `p_fa2`: for incorrect trials, the proportion of rows in `data` with
  confidence greater than `confidence`

- `p_hit2`: for correct trials, the proportion of rows in `data` with
  confidence greater than `confidence`

## See also

[`roc2_draws()`](https://metacoglab.github.io/hmetad/reference/roc2_draws.md),
[`roc2_rvars()`](https://metacoglab.github.io/hmetad/reference/roc2_draws.md)

## Examples

``` r
# calculate type 2 ROCs by stimulus
roc2(example_data())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 6 × 8
#> # Groups:   response [2]
#>   response confidence   n_0   n_1   p_0   p_1 p_hit2  p_fa2
#>      <int>      <int> <int> <int> <dbl> <dbl>  <dbl>  <dbl>
#> 1        0          1    75    86 0.15  0.172  0.756 0.522 
#> 2        0          2    46   101 0.092 0.202  0.469 0.229 
#> 3        0          3    26    94 0.052 0.188  0.202 0.0637
#> 4        1          1    74    86 0.148 0.172  0.749 0.5   
#> 5        1          2    44   104 0.088 0.208  0.446 0.203 
#> 6        1          3    24    75 0.048 0.15   0.227 0.0405

# calculate type 2 ROCs by condition, averaging over type 1 responses
roc2(sim_metad_condition(), condition, by_response = FALSE)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 6 × 8
#> # Groups:   condition [2]
#>   condition confidence   n_0   n_1   p_0   p_1  p_fa2 p_hit2
#>       <int>      <int> <int> <int> <dbl> <dbl>  <dbl>  <dbl>
#> 1         1          1    14    21  0.14  0.21 0.469   0.715
#> 2         1          2    10    20  0.1   0.2  0.0625  0.443
#> 3         1          3     2    18  0.02  0.18 0       0.207
#> 4         2          1    17    12  0.17  0.12 0.461   0.821
#> 5         2          2     9    17  0.09  0.17 0.147   0.571
#> 6         2          3     4    19  0.04  0.19 0.0263  0.297
```
