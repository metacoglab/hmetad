# Calculate empirical mean confidence

Given a dataset `data`, determine the mean confidence rating, optionally
conditional on stimulus and/or type 1 response.

## Usage

``` r
mean_confidence(
  data,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  K = NULL,
  by_stimulus = TRUE,
  by_response = TRUE,
  by_correct = FALSE
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

  If `TRUE` (default), calculate mean confidence conditional on
  stimulus. Ignored if `by_correct=TRUE`.

- by_response:

  If `TRUE` (default), calculate mean confidence conditional on type 2
  response. Ignored if `by_correct=TRUE`.

- by_correct:

  If `FALSE` (default), calculate mean confidence conditional on
  stimulus and/or type 1 response. If `TRUE`, instead calculate mean
  confidence conditional on accuracy.

## Value

A tibble with columns:

- `...`: the grouping columns in `data`

- `{.stimulus}`: the stimulus (if `by_stimulus=TRUE`)

- `{.response}`: the type 1 response (if `by_response=TRUE`)

- `correct`: the accuracy (if `by_correct=TRUE`)

- `mean_confidence`: the mean confidence rating

## See also

[`mean_confidence_draws()`](https://metacoglab.github.io/hmetad/reference/mean_conf_draws.md),
[`mean_confidence_rvars()`](https://metacoglab.github.io/hmetad/reference/mean_conf_draws.md)

## Examples

``` r
# calculate mean confidence by stimulus and response
mean_confidence(example_data())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 4 × 3
#> # Groups:   stimulus [2]
#>   stimulus response mean_confidence
#>      <int>    <int>           <dbl>
#> 1        0        0            2.43
#> 2        0        1            1.74
#> 3        1        0            1.82
#> 4        1        1            2.42

# calculate mean confidence by accuracy
mean_confidence(example_data(), by_correct = TRUE)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 2 × 2
#>   correct mean_confidence
#>     <int>           <dbl>
#> 1       0            1.78
#> 2       1            2.42

# calculate mean confidence by condition, averaging over type 1 responses
mean_confidence(sim_metad_condition(), condition, by_response = FALSE)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 4 × 3
#> # Groups:   condition [2]
#>   condition stimulus mean_confidence
#>       <int>    <int>           <dbl>
#> 1         1        0            2.24
#> 2         1        1            2   
#> 3         2        0            2.26
#> 4         2        1            2.22
```
