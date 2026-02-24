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

## Examples

``` r
if (FALSE) { # \dontrun{
  # running few iterations so example runs quickly, use more in practice
  example_data <- sim_metad(N_trials=1000)
  example_model <- fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
} # }
example_model <- hmetad:::example_model
newdata <- tidyr::tibble(.row = 1)

# compute type 2 ROC curve
roc2_draws(example_model, newdata)
#> # A tibble: 1,500 × 8
#> # Groups:   .row, response, confidence [6]
#>     .row response confidence .chain .iteration .draw p_hit2  p_fa2
#>    <int>    <int>      <dbl>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          4     NA         NA     1  0.279 0.0877
#>  2     1        0          4     NA         NA     2  0.302 0.0820
#>  3     1        0          4     NA         NA     3  0.261 0.0706
#>  4     1        0          4     NA         NA     4  0.233 0.0639
#>  5     1        0          4     NA         NA     5  0.264 0.0722
#>  6     1        0          4     NA         NA     6  0.260 0.0664
#>  7     1        0          4     NA         NA     7  0.267 0.0781
#>  8     1        0          4     NA         NA     8  0.264 0.0822
#>  9     1        0          4     NA         NA     9  0.268 0.0627
#> 10     1        0          4     NA         NA    10  0.256 0.0777
#> # ℹ 1,490 more rows
add_roc2_draws(newdata, example_model)
#> # A tibble: 1,500 × 8
#> # Groups:   .row, response, confidence [6]
#>     .row response confidence .chain .iteration .draw p_hit2  p_fa2
#>    <int>    <int>      <dbl>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          4     NA         NA     1  0.279 0.0877
#>  2     1        0          4     NA         NA     2  0.302 0.0820
#>  3     1        0          4     NA         NA     3  0.261 0.0706
#>  4     1        0          4     NA         NA     4  0.233 0.0639
#>  5     1        0          4     NA         NA     5  0.264 0.0722
#>  6     1        0          4     NA         NA     6  0.260 0.0664
#>  7     1        0          4     NA         NA     7  0.267 0.0781
#>  8     1        0          4     NA         NA     8  0.264 0.0822
#>  9     1        0          4     NA         NA     9  0.268 0.0627
#> 10     1        0          4     NA         NA    10  0.256 0.0777
#> # ℹ 1,490 more rows

# use posterior::rvar for additional efficiency
roc2_rvars(example_model, newdata)
#> # A tibble: 6 × 5
#> # Groups:   .row, response, confidence [6]
#>    .row response confidence        p_hit2          p_fa2
#>   <int>    <int>      <dbl>    <rvar[1d]>     <rvar[1d]>
#> 1     1        0          2  0.74 ± 0.020  0.500 ± 0.033
#> 2     1        0          3  0.48 ± 0.024  0.217 ± 0.024
#> 3     1        0          4  0.27 ± 0.023  0.080 ± 0.014
#> 4     1        1          1  0.75 ± 0.019  0.525 ± 0.032
#> 5     1        1          2  0.45 ± 0.023  0.195 ± 0.024
#> 6     1        1          3  0.23 ± 0.021  0.062 ± 0.013
add_roc2_rvars(newdata, example_model)
#> # A tibble: 6 × 5
#> # Groups:   .row, response, confidence [6]
#>    .row response confidence        p_hit2          p_fa2
#>   <int>    <int>      <dbl>    <rvar[1d]>     <rvar[1d]>
#> 1     1        0          2  0.74 ± 0.020  0.500 ± 0.033
#> 2     1        0          3  0.48 ± 0.024  0.217 ± 0.024
#> 3     1        0          4  0.27 ± 0.023  0.080 ± 0.014
#> 4     1        1          1  0.75 ± 0.019  0.525 ± 0.032
#> 5     1        1          2  0.45 ± 0.023  0.195 ± 0.024
#> 6     1        1          3  0.23 ± 0.021  0.062 ± 0.013

# include the ROC bounds
roc2_draws(example_model, newdata, bounds = TRUE)
#> # A tibble: 2,500 × 8
#> # Groups:   .row, response, confidence [10]
#>     .row response confidence .chain .iteration .draw p_hit2  p_fa2
#>    <int>    <dbl>      <dbl>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          4     NA         NA     1  0.279 0.0877
#>  2     1        0          4     NA         NA     2  0.302 0.0820
#>  3     1        0          4     NA         NA     3  0.261 0.0706
#>  4     1        0          4     NA         NA     4  0.233 0.0639
#>  5     1        0          4     NA         NA     5  0.264 0.0722
#>  6     1        0          4     NA         NA     6  0.260 0.0664
#>  7     1        0          4     NA         NA     7  0.267 0.0781
#>  8     1        0          4     NA         NA     8  0.264 0.0822
#>  9     1        0          4     NA         NA     9  0.268 0.0627
#> 10     1        0          4     NA         NA    10  0.256 0.0777
#> # ℹ 2,490 more rows
```
