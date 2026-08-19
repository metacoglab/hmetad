# Calculate empirical joint type 1/type 2 response probabilities

Given a dataset `data`, determine the probability of each combination of
type 1 and type 2 responses, optionally conditional on stimulus.

## Usage

``` r
joint_probabilities(
  data,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  K = NULL,
  by_stimulus = TRUE
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

- by_stimulus:

  If `TRUE` (default), calculate type 2 response probabilities
  conditional on stimulus.

## Value

A tibble with columns:

- `...`: the grouping columns in `data`

- `{.stimulus}` (if `by_stimulus=TRUE`): the stimulus

- `{.response}`: the type 1 response

- `{.confidence}`: the type 2 response

- `{.joint_response}`: the joint type 1/type 2 response

- `n`: the number of rows in `data` with the corresponding `stimulus`
  (if `by_stimulus=TRUE`), `response`, `confidence`, and
  `joint_response`

- `p`: the proportion of rows in `data` with the corresponding
  `response` (per `stimulus` if `by_stimulus=TRUE`)

## See also

[`epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`epred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# calculate type 2 response probabilities by stimulus
joint_probabilities(example_data())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 16 × 6
#> # Groups:   stimulus [2]
#>    stimulus response confidence joint_response     n     p
#>       <int>    <int>      <int>          <int> <int> <dbl>
#>  1        0        0          4              1    71 0.142
#>  2        0        0          3              2    94 0.188
#>  3        0        0          2              3   101 0.202
#>  4        0        0          1              4    86 0.172
#>  5        0        1          1              5    74 0.148
#>  6        0        1          2              6    44 0.088
#>  7        0        1          3              7    24 0.048
#>  8        0        1          4              8     6 0.012
#>  9        1        0          4              1    10 0.02 
#> 10        1        0          3              2    26 0.052
#> 11        1        0          2              3    46 0.092
#> 12        1        0          1              4    75 0.15 
#> 13        1        1          1              5    86 0.172
#> 14        1        1          2              6   104 0.208
#> 15        1        1          3              7    75 0.15 
#> 16        1        1          4              8    78 0.156

# calculate type 2 response probabilities by condition, averaging over stimuli
joint_probabilities(sim_metad_condition(), condition, by_stimulus = FALSE)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 16 × 6
#> # Groups:   condition [2]
#>    condition response confidence joint_response     n     p
#>        <int>    <int>      <int>          <int> <int> <dbl>
#>  1         1        0          4              1    13  0.13
#>  2         1        0          3              2    13  0.13
#>  3         1        0          2              3     8  0.08
#>  4         1        0          1              4    16  0.16
#>  5         1        1          1              5    16  0.16
#>  6         1        1          2              6    11  0.11
#>  7         1        1          3              7    11  0.11
#>  8         1        1          4              8    12  0.12
#>  9         2        0          4              1     8  0.08
#> 10         2        0          3              2     9  0.09
#> 11         2        0          2              3    22  0.22
#> 12         2        0          1              4    18  0.18
#> 13         2        1          1              5    18  0.18
#> 14         2        1          2              6     9  0.09
#> 15         2        1          3              7     7  0.07
#> 16         2        1          4              8     9  0.09
```
