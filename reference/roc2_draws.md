# Obtain posterior draws of the response-specific type 2 receiver operating characteristic (ROC) curves.

Given a data frame and a meta-d' model, adds estimates of the cumulative
probability over confidence for each type 1 response. For `roc2_draws`
and `add_roc2_draws`, estimates are returned in a tidy tibble with one
row per posterior draw and per joint response. For `roc2_rvars` and
`add_roc2_rvars`, parameters are returned as
[`posterior::rvar`](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata` and per joint response.

## Usage

``` r
roc2_draws(
  object,
  newdata,
  ...,
  .response = "response",
  .confidence = "confidence",
  bounds = FALSE,
  by_response = TRUE
)

add_roc2_draws(newdata, object, ...)

roc2_rvars(
  object,
  newdata,
  ...,
  .response = "response",
  .confidence = "confidence",
  bounds = FALSE,
  by_response = TRUE
)

add_roc2_rvars(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional parameters passed to
  [tidybayes::epred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)

- .response:

  The name of "response" column

- .confidence:

  The name of "confidence" column

- bounds:

  If `TRUE`, include the endpoints of the ROC at \\(0, 0)\\ and \\(1,
  1)\\. Otherwise, the endpoints are excluded.

- by_response:

  If `TRUE` (default), compute separate ROCs for each type 1 response.
  Otherwise, average ROCs across both type 1 responses.

## Value

a tibble containing posterior draws of the pseudo type 1 ROC with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `roc2_draws` and
  `add_roc2_draws`, identifiers for the posterior sample

- `{.response}`: the type 1 response for perceived stimulus presence
  (\\R \in \\0, 1\\\\)

- `{.confidence}`: the type 2 confidence response (\\C \in \[1, K\]\\)

- `p_fa2`: the cumulative probability of an incorrect response (\\P(C\ge
  c \\\vert\\ R\ne S)\\)

- `p_hit2`: the cumulative probability of a correct response (\\P(C\ge c
  \\\vert\\ R = S)\\)

## See also

[`roc2()`](https://metacoglab.github.io/hmetad/reference/roc2.md),
[`tidybayes::epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::epred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# compute type 2 ROC curve
# equivalent to `add_roc2_draws(newdata, example_model())`
roc2_draws(example_model(), newdata)
#> # A tibble: 6,000 × 8
#> # Groups:   .row, response, confidence [6]
#>     .row response confidence .chain .iteration .draw p_hit2  p_fa2
#>    <int>    <int>      <int>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          3     NA         NA     1  0.180 0.0459
#>  2     1        0          3     NA         NA     2  0.210 0.0706
#>  3     1        0          3     NA         NA     3  0.190 0.0517
#>  4     1        0          3     NA         NA     4  0.202 0.0380
#>  5     1        0          3     NA         NA     5  0.194 0.0515
#>  6     1        0          3     NA         NA     6  0.215 0.0612
#>  7     1        0          3     NA         NA     7  0.197 0.0462
#>  8     1        0          3     NA         NA     8  0.187 0.0498
#>  9     1        0          3     NA         NA     9  0.222 0.0494
#> 10     1        0          3     NA         NA    10  0.193 0.0563
#> # ℹ 5,990 more rows

# use posterior::rvar for additional efficiency
# equivalent to `add_roc2_rvars(newdata, example_model())`
roc2_rvars(example_model(), newdata)
#> # A tibble: 6 × 5
#> # Groups:   .row, response, confidence [6]
#>    .row response confidence        p_hit2          p_fa2
#>   <int>    <int>      <int>    <rvar[1d]>     <rvar[1d]>
#> 1     1        0          1  0.75 ± 0.020  0.526 ± 0.032
#> 2     1        0          2  0.47 ± 0.024  0.216 ± 0.024
#> 3     1        0          3  0.21 ± 0.021  0.053 ± 0.011
#> 4     1        1          1  0.74 ± 0.019  0.515 ± 0.030
#> 5     1        1          2  0.45 ± 0.024  0.197 ± 0.023
#> 6     1        1          3  0.22 ± 0.021  0.058 ± 0.011

# include the ROC bounds
# equivalent to `roc2_draws(newdata, example_model(), bounds = TRUE)`
roc2_draws(example_model(), newdata, bounds = TRUE)
#> # A tibble: 10,000 × 8
#> # Groups:   .row, response, confidence [10]
#>     .row response confidence .chain .iteration .draw p_hit2  p_fa2
#>    <int>    <int>      <int>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          3     NA         NA     1  0.180 0.0459
#>  2     1        0          3     NA         NA     2  0.210 0.0706
#>  3     1        0          3     NA         NA     3  0.190 0.0517
#>  4     1        0          3     NA         NA     4  0.202 0.0380
#>  5     1        0          3     NA         NA     5  0.194 0.0515
#>  6     1        0          3     NA         NA     6  0.215 0.0612
#>  7     1        0          3     NA         NA     7  0.197 0.0462
#>  8     1        0          3     NA         NA     8  0.187 0.0498
#>  9     1        0          3     NA         NA     9  0.222 0.0494
#> 10     1        0          3     NA         NA    10  0.193 0.0563
#> # ℹ 9,990 more rows
# }
```
