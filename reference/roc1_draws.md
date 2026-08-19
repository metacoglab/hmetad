# Obtain posterior draws of the pseudo type 1 receiver operating characteristic (ROC) curve.

Given a data frame and a meta-d' model, adds estimates of the cumulative
probability over joint_responses. For `roc1_draws` and `add_roc1_draws`,
estimates are returned in a tidy tibble with one row per posterior draw
and per joint response. For `roc1_rvars` and `add_roc1_rvars`,
parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata` and per joint response.

## Usage

``` r
roc1_draws(
  object,
  newdata,
  ...,
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  bounds = FALSE
)

add_roc1_draws(newdata, object, ...)

roc1_rvars(
  object,
  newdata,
  ...,
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  bounds = FALSE
)

add_roc1_rvars(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional parameters passed to
  [tidybayes::epred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
  or
  [tidybayes::epred_rvars](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

- .response:

  The name of "response" column

- .confidence:

  The name of "confidence" column

- .joint_response:

  The name of "joint_response" column

- bounds:

  If `TRUE`, include the endpoints of the ROC at \\(0, 0)\\ and \\(1,
  1)\\. Otherwise, the endpoints are excluded.

## Value

a tibble containing posterior draws of the pseudo type 1 ROC with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `roc1_draws` and
  `add_roc1_draws`, identifiers for the posterior sample

- `{.joint_response}`: the combined type 1 / type 2 response (\\J \in
  \[1, 2K\]\\) for \\K\\ confidence levels)

- `{.response}`: the type 1 response for perceived stimulus presence
  (\\R \in \\0, 1\\\\)

- `{.confidence}`: the type 2 confidence response (\\C \in \[1, K\]\\)

- `p_fa`: the cumulative probability of a 'present'/'old' response for
  `stimulus==0` (\\P(J \ge j \\\vert\\ S=0)\\)

- `p_hit`: the cumulative probability of a 'present'/'old' response for
  `stimulus==1` (\\P(J \ge j \\\vert\\ S=1)\\)

## See also

[`roc1()`](https://metacoglab.github.io/hmetad/reference/roc1.md),
[`tidybayes::epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::epred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# compute pseudo-type 1 ROC curve
# equivalent to ``
roc1_draws(example_model(), newdata)
#> # A tibble: 7,000 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <int>    <int>      <int>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              1        0          4     NA         NA     1 0.871 0.984
#>  2     1              1        0          4     NA         NA     2 0.855 0.979
#>  3     1              1        0          4     NA         NA     3 0.866 0.982
#>  4     1              1        0          4     NA         NA     4 0.860 0.988
#>  5     1              1        0          4     NA         NA     5 0.869 0.983
#>  6     1              1        0          4     NA         NA     6 0.847 0.981
#>  7     1              1        0          4     NA         NA     7 0.865 0.985
#>  8     1              1        0          4     NA         NA     8 0.868 0.984
#>  9     1              1        0          4     NA         NA     9 0.840 0.984
#> 10     1              1        0          4     NA         NA    10 0.860 0.981
#> # ℹ 6,990 more rows
add_roc1_draws(newdata, example_model())
#> # A tibble: 7,000 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <int>    <int>      <int>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              1        0          4     NA         NA     1 0.871 0.984
#>  2     1              1        0          4     NA         NA     2 0.855 0.979
#>  3     1              1        0          4     NA         NA     3 0.866 0.982
#>  4     1              1        0          4     NA         NA     4 0.860 0.988
#>  5     1              1        0          4     NA         NA     5 0.869 0.983
#>  6     1              1        0          4     NA         NA     6 0.847 0.981
#>  7     1              1        0          4     NA         NA     7 0.865 0.985
#>  8     1              1        0          4     NA         NA     8 0.868 0.984
#>  9     1              1        0          4     NA         NA     9 0.840 0.984
#> 10     1              1        0          4     NA         NA    10 0.860 0.981
#> # ℹ 6,990 more rows

# use posterior::rvar for additional efficiency
# equivalent to `add_roc1_draws(newdata, example_model())`
roc1_rvars(example_model(), newdata)
#> # A tibble: 7 × 6
#> # Groups:   .row, joint_response, response, confidence [7]
#>    .row joint_response response confidence            p_fa          p_hit
#>   <int>          <int>    <int>      <int>      <rvar[1d]>     <rvar[1d]>
#> 1     1              1        0          4  0.854 ± 0.0152  0.98 ± 0.0035
#> 2     1              2        0          3  0.666 ± 0.0200  0.93 ± 0.0086
#> 3     1              3        0          2  0.470 ± 0.0207  0.84 ± 0.0143
#> 4     1              4        0          1  0.296 ± 0.0209  0.69 ± 0.0204
#> 5     1              5        1          1  0.152 ± 0.0133  0.51 ± 0.0202
#> 6     1              6        1          2  0.058 ± 0.0077  0.31 ± 0.0187
#> 7     1              7        1          3  0.017 ± 0.0035  0.15 ± 0.0147

# include the ROC bounds
# equivalent to `add_roc1_draws(newdata, example_model(), bounds = TRUE)`
roc1_draws(example_model(), newdata, bounds = TRUE)
#> # A tibble: 9,000 × 9
#> # Groups:   .row, joint_response, response, confidence [9]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <dbl>    <int>      <int>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              0        0          5     NA         NA     1     1     1
#>  2     1              0        0          5     NA         NA     2     1     1
#>  3     1              0        0          5     NA         NA     3     1     1
#>  4     1              0        0          5     NA         NA     4     1     1
#>  5     1              0        0          5     NA         NA     5     1     1
#>  6     1              0        0          5     NA         NA     6     1     1
#>  7     1              0        0          5     NA         NA     7     1     1
#>  8     1              0        0          5     NA         NA     8     1     1
#>  9     1              0        0          5     NA         NA     9     1     1
#> 10     1              0        0          5     NA         NA    10     1     1
#> # ℹ 8,990 more rows
# }
```
