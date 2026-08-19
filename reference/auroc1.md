# Calculate area under the empirical pseudo-type 1 receiver operating characteristic curve

Calculate area under the empirical pseudo-type 1 receiver operating
characteristic curve

## Usage

``` r
auroc1(
  data,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
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

- K:

  The number of confidence levels in `data`. If `NULL`, this is
  estimated from `data` using the maximum value of either the confidence
  column or joint response column.

## Value

A tibble with columns:

- `...`: the grouping columns in `data`

- `auroc1`: the area under the pseudo type 1 ROC curve

## See also

[`roc1()`](https://metacoglab.github.io/hmetad/reference/roc1.md),
[`auroc1_draws()`](https://metacoglab.github.io/hmetad/reference/auroc1_draws.md),
[`auroc1_rvars()`](https://metacoglab.github.io/hmetad/reference/auroc1_draws.md)

## Examples

``` r
# calculate area under the type 1 ROC
auroc1(example_data())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 1 × 1
#>   auroc1
#>    <dbl>
#> 1  0.766

# calculate type 1 ROCs by condition
auroc1(sim_metad_condition(), condition)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 2 × 2
#>   condition auroc1
#>       <int>  <dbl>
#> 1         1  0.689
#> 2         2  0.732
```
