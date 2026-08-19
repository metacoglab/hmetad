# Calculate empirical type 2 response probabilities

Given a dataset `data`, determine the probability of each type 2
response, optionally conditional on stimulus and/or type 1 response.

## Usage

``` r
type2_probabilities(
  data,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  K = NULL,
  by_stimulus = TRUE,
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

- by_stimulus:

  If `TRUE` (default), calculate type 2 response probabilities
  conditional on stimulus.

- by_response:

  If `TRUE` (default), calculate type 2 response probabilities
  conditional on type 1 response.

## Value

A tibble with columns:

- `...`: the grouping columns in `data`

- `{.stimulus}` (if `by_stimulus=TRUE`): the stimulus

- `{.response}` (if `by_response=TRUE`): the type 1 response

- `{.confidence}`: the type 2 response

- `{.joint_response}` (if `by_response=TRUE`): the joint type 1/type 2
  response

- `n`: the number of rows in `data` with the corresponding `stimulus`
  (if `by_stimulus=TRUE`), `response` (if `by_response=TRUE`), and
  `confidence`

- `p`: the proportion of rows in `data` with the corresponding
  `response` (per `stimulus` if `by_stimulus=TRUE` and per `response` if
  `by_response=TRUE`)

## See also

[`type2_draws()`](https://metacoglab.github.io/hmetad/reference/type2_draws.md)

## Examples

``` r
# calculate type 2 response probabilities by stimulus
type2_probabilities(example_data())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 16 × 6
#> # Groups:   stimulus [2]
#>    stimulus response confidence joint_response     n      p
#>       <int>    <int>      <int>          <int> <int>  <dbl>
#>  1        0        0          1              4    86 0.244 
#>  2        0        0          2              3   101 0.287 
#>  3        0        0          3              2    94 0.267 
#>  4        0        0          4              1    71 0.202 
#>  5        0        1          1              5    74 0.5   
#>  6        0        1          2              6    44 0.297 
#>  7        0        1          3              7    24 0.162 
#>  8        0        1          4              8     6 0.0405
#>  9        1        0          1              4    75 0.478 
#> 10        1        0          2              3    46 0.293 
#> 11        1        0          3              2    26 0.166 
#> 12        1        0          4              1    10 0.0637
#> 13        1        1          1              5    86 0.251 
#> 14        1        1          2              6   104 0.303 
#> 15        1        1          3              7    75 0.219 
#> 16        1        1          4              8    78 0.227 

# calculate type 2 response probabilities by condition, averaging over stimuli
type2_probabilities(sim_metad_condition(), condition, by_stimulus = FALSE)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 16 × 6
#> # Groups:   condition [2]
#>    condition response confidence joint_response     n     p
#>        <int>    <int>      <int>          <int> <int> <dbl>
#>  1         1        0          1              4    15 0.288
#>  2         2        0          1              4    15 0.349
#>  3         1        0          2              3    17 0.327
#>  4         2        0          2              3    15 0.349
#>  5         1        0          3              2     8 0.154
#>  6         2        0          3              2     6 0.140
#>  7         1        0          4              1    12 0.231
#>  8         2        0          4              1     7 0.163
#>  9         1        1          1              5    18 0.375
#> 10         2        1          1              5    19 0.333
#> 11         1        1          2              6    12 0.25 
#> 12         2        1          2              6    11 0.193
#> 13         1        1          3              7     6 0.125
#> 14         2        1          3              7    15 0.263
#> 15         1        1          4              8    12 0.25 
#> 16         2        1          4              8    12 0.211
```
