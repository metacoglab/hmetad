# Calculate empirical pseudo-type 1 receiver operating characteristic curves

Given a dataset `data`, determine the cumulative probability of each
combination of type 1 and type 2 responses conditional on stimulus.

## Usage

``` r
roc1(
  data,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  K = NULL,
  bounds = FALSE
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

## Value

A tibble with columns:

- `...`: the grouping columns in `data`

- `{.response}`: the type 1 response

- `{.confidence}`: the type 2 response

- `{.joint_response}`: the joint type 1/type 2 response

- `n_0`: the number of rows in `data` with `stimulus=0` and the
  corresponding joint response

- `n_1`: the number of rows in `data` with `stimulus=1` and the
  corresponding joint response

- `p_0`: where `stimulus=0`, the proportion of rows in `data` with joint
  response equal to `.joint_response`

- `p_1`: where `stimulus=1`, the proportion of rows in `data` with joint
  response equal to `.joint_response`

- `p_fa`: where `stimulus=0`, the proportion of rows in `data` with
  joint response greater than `.joint_response`

- `p_hit`: where `stimulus=1`, the proportion of rows in `data` with
  joint response greater than `.joint_response`

## See also

[`roc1_draws()`](https://metacoglab.github.io/hmetad/reference/roc1_draws.md),
[`roc1_rvars()`](https://metacoglab.github.io/hmetad/reference/roc1_draws.md)

## Examples

``` r
# calculate type 1 ROCs
roc1(example_data())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 7 × 9
#>   response confidence joint_response   n_0   n_1   p_0   p_1   p_fa p_hit
#>      <int>      <int>          <int> <int> <int> <dbl> <dbl>  <dbl> <dbl>
#> 1        0          4              1    71    10 0.142 0.02  0.858  0.98 
#> 2        0          3              2    94    26 0.188 0.052 0.67   0.928
#> 3        0          2              3   101    46 0.202 0.092 0.468  0.836
#> 4        0          1              4    86    75 0.172 0.15  0.296  0.686
#> 5        1          1              5    74    86 0.148 0.172 0.148  0.514
#> 6        1          2              6    44   104 0.088 0.208 0.0600 0.306
#> 7        1          3              7    24    75 0.048 0.15  0.0120 0.156

# calculate type 1 ROCs by condition
roc1(sim_metad_condition(), condition)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 14 × 10
#> # Groups:   condition [2]
#>    condition response confidence joint_response   n_0   n_1   p_0   p_1   p_fa
#>        <int>    <int>      <int>          <int> <int> <int> <dbl> <dbl>  <dbl>
#>  1         1        0          4              1    13     2  0.26  0.04 0.74  
#>  2         1        0          3              2     6     2  0.12  0.04 0.62  
#>  3         1        0          2              3    10     3  0.2   0.06 0.42  
#>  4         1        0          1              4     5     6  0.1   0.12 0.32  
#>  5         1        1          1              5     8    11  0.16  0.22 0.16  
#>  6         1        1          2              6     4     9  0.08  0.18 0.0800
#>  7         1        1          3              7     1     8  0.02  0.16 0.0600
#>  8         2        0          4              1     9     2  0.18  0.04 0.82  
#>  9         2        0          3              2    10     7  0.2   0.14 0.62  
#> 10         2        0          2              3     6     5  0.12  0.1  0.5   
#> 11         2        0          1              4     8     1  0.16  0.02 0.34  
#> 12         2        1          1              5    13    10  0.26  0.2  0.0800
#> 13         2        1          2              6     2    10  0.04  0.2  0.0400
#> 14         2        1          3              7     2     8  0.04  0.16 0     
#> # ℹ 1 more variable: p_hit <dbl>
```
