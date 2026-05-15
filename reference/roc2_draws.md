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
roc2_draws(object, newdata, ..., bounds = FALSE)

add_roc2_draws(newdata, object, ...)

roc2_rvars(object, newdata, ..., bounds = FALSE)

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

- bounds:

  If `TRUE`, include the endpoints of the ROC at \\(0, 0)\\ and \\(1,
  1)\\. Otherwise, the endpoints are excluded.

## Value

a tibble containing posterior draws of the pseudo type 1 ROC with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `roc2_draws` and
  `add_roc2_draws`, identifiers for the posterior sample

- `response`: the type 1 response for perceived stimulus presence (\\R
  \in \\0, 1\\\\)

- `confidence`: the type 2 confidence response (\\C \in \[1, K\]\\)

- `p_fa2`: the cumulative probability of an incorrect response (\\P(C\ge
  c \\\vert\\ R\ne S)\\)

- `p_hit2`: the cumulative probability of a correct response (\\P(C\ge c
  \\\vert\\ R = S)\\)

## See also

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
#>    <int>    <int>      <dbl>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          4     NA         NA     1  0.219 0.0617
#>  2     1        0          4     NA         NA     2  0.188 0.0394
#>  3     1        0          4     NA         NA     3  0.177 0.0423
#>  4     1        0          4     NA         NA     4  0.190 0.0447
#>  5     1        0          4     NA         NA     5  0.190 0.0496
#>  6     1        0          4     NA         NA     6  0.224 0.0520
#>  7     1        0          4     NA         NA     7  0.207 0.0518
#>  8     1        0          4     NA         NA     8  0.217 0.0612
#>  9     1        0          4     NA         NA     9  0.182 0.0459
#> 10     1        0          4     NA         NA    10  0.238 0.0543
#> # ℹ 5,990 more rows

# use posterior::rvar for additional efficiency
# equivalent to `add_roc2_rvars(newdata, example_model())`
roc2_rvars(example_model(), newdata)
#> # A tibble: 6 × 5
#> # Groups:   .row, response, confidence [6]
#>    .row response confidence        p_hit2          p_fa2
#>   <int>    <int>      <dbl>    <rvar[1d]>     <rvar[1d]>
#> 1     1        0          2  0.75 ± 0.020  0.526 ± 0.032
#> 2     1        0          3  0.47 ± 0.024  0.215 ± 0.026
#> 3     1        0          4  0.21 ± 0.020  0.053 ± 0.011
#> 4     1        1          1  0.74 ± 0.019  0.516 ± 0.030
#> 5     1        1          2  0.45 ± 0.023  0.197 ± 0.024
#> 6     1        1          3  0.22 ± 0.020  0.059 ± 0.012

# include the ROC bounds
# equivalent to `roc2_draws(newdata, example_model(), bounds = TRUE)`
roc2_draws(example_model(), newdata, bounds = TRUE)
#> # A tibble: 10,000 × 8
#> # Groups:   .row, response, confidence [10]
#>     .row response confidence .chain .iteration .draw p_hit2  p_fa2
#>    <int>    <dbl>      <dbl>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          4     NA         NA     1  0.219 0.0617
#>  2     1        0          4     NA         NA     2  0.188 0.0394
#>  3     1        0          4     NA         NA     3  0.177 0.0423
#>  4     1        0          4     NA         NA     4  0.190 0.0447
#>  5     1        0          4     NA         NA     5  0.190 0.0496
#>  6     1        0          4     NA         NA     6  0.224 0.0520
#>  7     1        0          4     NA         NA     7  0.207 0.0518
#>  8     1        0          4     NA         NA     8  0.217 0.0612
#>  9     1        0          4     NA         NA     9  0.182 0.0459
#> 10     1        0          4     NA         NA    10  0.238 0.0543
#> # ℹ 9,990 more rows
# }
```
