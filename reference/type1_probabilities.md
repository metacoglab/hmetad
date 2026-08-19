# Calculate empirical type 1 response probabilities

Given a dataset `data`, determine the probability of each type 1
response, optionally conditional on stimulus.

## Usage

``` r
type1_probabilities(
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

  If `TRUE` (default), calculate conditional type 1 response
  probabilities \\P(R=r \vert S=s)\\. Otherwise, calculate unconditional
  response probabilities \\P(R=r)\\ as an unweighted average over
  stimuli.

## Value

A tibble with columns:

- `...`: the grouping columns in `data`

- `{.stimulus}` (if `by_stimulus=TRUE`): the stimulus

- `{.response}`: the type 1 response

- `n`: the number of rows in `data` with the corresponding `stimulus`
  (if `by_stimulus=TRUE`) and `response`

- `p`: the proportion of rows in `data` with the corresponding
  `response` (per `stimulus` if `by_stimulus=TRUE`)

## See also

[`type1_draws()`](https://metacoglab.github.io/hmetad/reference/type1_draws.md)

## Examples

``` r
# calculate response probabilities by stimulus
type1_probabilities(example_data())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 4 × 4
#> # Groups:   stimulus [2]
#>   stimulus response     n     p
#>      <int>    <int> <int> <dbl>
#> 1        0        0   352 0.704
#> 2        0        1   148 0.296
#> 3        1        0   157 0.314
#> 4        1        1   343 0.686

# calculate response probabilities by condition, averaging over stimuli
type1_probabilities(sim_metad_condition(), condition, by_stimulus = FALSE)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 4 × 4
#> # Groups:   condition [2]
#>   condition response     n     p
#>       <int>    <int> <int> <dbl>
#> 1         1        0    51  0.51
#> 2         1        1    49  0.49
#> 3         2        0    52  0.52
#> 4         2        1    48  0.48
```
