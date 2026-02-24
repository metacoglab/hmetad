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
roc1_draws(object, newdata, ..., bounds = FALSE)

add_roc1_draws(newdata, object, ...)

roc1_rvars(object, newdata, ..., bounds = FALSE)

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

- bounds:

  If `TRUE`, include the endpoints of the ROC at \\(0, 0)\\ and \\(1,
  1)\\. Otherwise, the endpoints are excluded.

## Value

a tibble containing posterior draws of the pseudo type 1 ROC with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `roc1_draws` and
  `add_roc1_draws`, identifiers for the posterior sample

- `joint_response`: the combined type 1 / type 2 response (\\J \in \[1,
  2K\]\\) for \\K\\ confidence levels)

- `response`: the type 1 response for perceived stimulus presence (\\R
  \in \\0, 1\\\\)

- `confidence`: the type 2 confidence response (\\C \in \[1, K\]\\)

- `p_fa`: the cumulative probability of a 'present'/'old' response for
  `stimulus==0` (\\P(J \ge j \\\vert\\ S=0)\\)

- `p_hit`: the cumulative probability of a 'present'/'old' response for
  `stimulus==1` (\\P(J \ge j \\\vert\\ S=1)\\)

## Examples

``` r
if (FALSE) { # \dontrun{
  # running few iterations so example runs quickly, use more in practice
  example_data <- sim_metad(N_trials=1000)
  example_model <- fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
} # }
example_model <- hmetad:::example_model
newdata <- tidyr::tibble(.row = 1)

# compute pseudo-type 1 ROC curve
roc1_draws(example_model, newdata)
#> # A tibble: 1,750 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <int>    <int>      <dbl>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              1        0          4     NA         NA     1 0.809 0.974
#>  2     1              1        0          4     NA         NA     2 0.791 0.976
#>  3     1              1        0          4     NA         NA     3 0.829 0.979
#>  4     1              1        0          4     NA         NA     4 0.826 0.980
#>  5     1              1        0          4     NA         NA     5 0.804 0.975
#>  6     1              1        0          4     NA         NA     6 0.815 0.978
#>  7     1              1        0          4     NA         NA     7 0.817 0.978
#>  8     1              1        0          4     NA         NA     8 0.824 0.976
#>  9     1              1        0          4     NA         NA     9 0.816 0.981
#> 10     1              1        0          4     NA         NA    10 0.828 0.975
#> # ℹ 1,740 more rows
add_roc1_draws(newdata, example_model)
#> # A tibble: 1,750 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <int>    <int>      <dbl>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              1        0          4     NA         NA     1 0.809 0.974
#>  2     1              1        0          4     NA         NA     2 0.791 0.976
#>  3     1              1        0          4     NA         NA     3 0.829 0.979
#>  4     1              1        0          4     NA         NA     4 0.826 0.980
#>  5     1              1        0          4     NA         NA     5 0.804 0.975
#>  6     1              1        0          4     NA         NA     6 0.815 0.978
#>  7     1              1        0          4     NA         NA     7 0.817 0.978
#>  8     1              1        0          4     NA         NA     8 0.824 0.976
#>  9     1              1        0          4     NA         NA     9 0.816 0.981
#> 10     1              1        0          4     NA         NA    10 0.828 0.975
#> # ℹ 1,740 more rows

# use posterior::rvar for additional efficiency
roc1_rvars(example_model, newdata)
#> # A tibble: 7 × 6
#> # Groups:   .row, joint_response, response, confidence [7]
#>    .row joint_response response confidence            p_fa          p_hit
#>   <int>          <int>    <int>      <dbl>      <rvar[1d]>     <rvar[1d]>
#> 1     1              1        0          4  0.812 ± 0.0169  0.98 ± 0.0045
#> 2     1              2        0          3  0.665 ± 0.0194  0.93 ± 0.0085
#> 3     1              3        0          2  0.485 ± 0.0219  0.85 ± 0.0135
#> 4     1              4        0          1  0.301 ± 0.0215  0.69 ± 0.0202
#> 5     1              5        1          1  0.158 ± 0.0142  0.52 ± 0.0212
#> 6     1              6        1          2  0.059 ± 0.0078  0.31 ± 0.0188
#> 7     1              7        1          3  0.019 ± 0.0039  0.16 ± 0.0152
add_roc1_draws(newdata, example_model)
#> # A tibble: 1,750 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <int>    <int>      <dbl>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              1        0          4     NA         NA     1 0.809 0.974
#>  2     1              1        0          4     NA         NA     2 0.791 0.976
#>  3     1              1        0          4     NA         NA     3 0.829 0.979
#>  4     1              1        0          4     NA         NA     4 0.826 0.980
#>  5     1              1        0          4     NA         NA     5 0.804 0.975
#>  6     1              1        0          4     NA         NA     6 0.815 0.978
#>  7     1              1        0          4     NA         NA     7 0.817 0.978
#>  8     1              1        0          4     NA         NA     8 0.824 0.976
#>  9     1              1        0          4     NA         NA     9 0.816 0.981
#> 10     1              1        0          4     NA         NA    10 0.828 0.975
#> # ℹ 1,740 more rows

# include the ROC bounds
roc1_draws(example_model, newdata, bounds = TRUE)
#> # A tibble: 2,250 × 9
#> # Groups:   .row, joint_response, response, confidence [9]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <dbl>    <dbl>      <dbl>  <int>      <int> <int> <dbl> <dbl>
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
#> # ℹ 2,240 more rows
```
