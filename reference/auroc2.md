# Calculate the area under empirical type 2 receiver operating characteristic curves

Calculate the area under empirical type 2 receiver operating
characteristic curves

## Usage

``` r
auroc2(
  data,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  K = NULL,
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

- by_response:

  If `TRUE` (default), calculate type 2 ROCs conditional on type 1
  response.

## Value

A tibble with columns:

- `...`: the grouping columns in `data`

- `{.response}` (if `by_response=TRUE`): the type 1 response

- `auroc2`: the area under the type 2 ROC

## See also

[`roc2()`](https://metacoglab.github.io/hmetad/reference/roc2.md),
[`auroc2_draws()`](https://metacoglab.github.io/hmetad/reference/auroc2_draws.md),
[`auroc2_rvars()`](https://metacoglab.github.io/hmetad/reference/auroc2_draws.md)

## Examples

``` r
# calculate type 2 ROCs by stimulus
auroc2(example_data())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 2 × 2
#>   response auroc2
#>      <int>  <dbl>
#> 1        0  0.661
#> 2        1  0.674

# calculate type 2 ROCs by condition, averaging over type 1 responses
auroc2(sim_metad_condition(), condition, by_response = FALSE)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 2 × 2
#>   condition auroc2
#>       <int>  <dbl>
#> 1         1  0.701
#> 2         2  0.710
```
