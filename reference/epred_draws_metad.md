# Obtain posterior draws of joint response probabilities

Given a data frame and a meta-d' model, adds estimates of joint type 1
and type 2 response probabilities. For `epred_draws_metad` and
`add_epred_draws_metad`, estimates are returned in a tidy tibble with
one row per posterior draw. For `epred_rvars_metad` and
`add_epred_rvars_metad`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
epred_draws_metad(object, newdata, ...)

add_epred_draws_metad(newdata, object, ...)

epred_rvars_metad(object, newdata, ...)

add_epred_rvars_metad(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional arguments passed to
  [tidybayes::add_epred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
  or
  [tidybayes::add_epred_rvars](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Value

a tibble containing posterior draws of model parameters with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `epred_draws_metad`, identifiers
  for the posterior sample

- `stimulus`, `joint_response`, `response`, `confidence`: identifiers
  for the response type

- `.epred`: probability of the type 1 and type 2 response given the
  stimulus, \\P(R, C \\\vert\\ S)\\

## Examples

``` r
if (FALSE) { # \dontrun{
# running few iterations so example runs quickly, use more in practice
example_data <- sim_metad(N_trials = 1000)
example_model <- fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
} # }
example_model <- hmetad:::example_model
newdata <- tidyr::tibble(.row = 1)

# obtain model predictions
epred_draws_metad(example_model, newdata)
#> # A tibble: 4,000 × 9
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence .epred .chain .iteration
#>    <int>    <int>          <int>    <int>      <dbl>  <dbl>  <int>      <int>
#>  1     1        0              1        0          4  0.191     NA         NA
#>  2     1        0              1        0          4  0.209     NA         NA
#>  3     1        0              1        0          4  0.171     NA         NA
#>  4     1        0              1        0          4  0.174     NA         NA
#>  5     1        0              1        0          4  0.196     NA         NA
#>  6     1        0              1        0          4  0.185     NA         NA
#>  7     1        0              1        0          4  0.183     NA         NA
#>  8     1        0              1        0          4  0.176     NA         NA
#>  9     1        0              1        0          4  0.184     NA         NA
#> 10     1        0              1        0          4  0.172     NA         NA
#> # ℹ 3,990 more rows
#> # ℹ 1 more variable: .draw <int>
add_epred_draws_metad(newdata, example_model)
#> # A tibble: 4,000 × 9
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence .epred .chain .iteration
#>    <int>    <int>          <int>    <int>      <dbl>  <dbl>  <int>      <int>
#>  1     1        0              1        0          4  0.191     NA         NA
#>  2     1        0              1        0          4  0.209     NA         NA
#>  3     1        0              1        0          4  0.171     NA         NA
#>  4     1        0              1        0          4  0.174     NA         NA
#>  5     1        0              1        0          4  0.196     NA         NA
#>  6     1        0              1        0          4  0.185     NA         NA
#>  7     1        0              1        0          4  0.183     NA         NA
#>  8     1        0              1        0          4  0.176     NA         NA
#>  9     1        0              1        0          4  0.184     NA         NA
#> 10     1        0              1        0          4  0.172     NA         NA
#> # ℹ 3,990 more rows
#> # ℹ 1 more variable: .draw <int>

# obtain model predictions (posterior::rvar)
epred_rvars_metad(example_model, newdata)
#> # A tibble: 16 × 6
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence          .epred
#>    <int>    <int>          <int>    <int>      <dbl>      <rvar[1d]>
#>  1     1        0              1        0          4  0.188 ± 0.0169
#>  2     1        0              2        0          3  0.147 ± 0.0142
#>  3     1        0              3        0          2  0.180 ± 0.0159
#>  4     1        0              4        0          1  0.184 ± 0.0150
#>  5     1        0              5        1          1  0.143 ± 0.0144
#>  6     1        0              6        1          2  0.099 ± 0.0099
#>  7     1        0              7        1          3  0.040 ± 0.0051
#>  8     1        0              8        1          4  0.019 ± 0.0039
#>  9     1        1              1        0          4  0.025 ± 0.0045
#> 10     1        1              2        0          3  0.043 ± 0.0057
#> 11     1        1              3        0          2  0.088 ± 0.0087
#> 12     1        1              4        0          1  0.155 ± 0.0149
#> 13     1        1              5        1          1  0.171 ± 0.0131
#> 14     1        1              6        1          2  0.208 ± 0.0145
#> 15     1        1              7        1          3  0.153 ± 0.0139
#> 16     1        1              8        1          4  0.158 ± 0.0152
add_epred_rvars_metad(newdata, example_model)
#> # A tibble: 16 × 6
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence          .epred
#>    <int>    <int>          <int>    <int>      <dbl>      <rvar[1d]>
#>  1     1        0              1        0          4  0.188 ± 0.0169
#>  2     1        0              2        0          3  0.147 ± 0.0142
#>  3     1        0              3        0          2  0.180 ± 0.0159
#>  4     1        0              4        0          1  0.184 ± 0.0150
#>  5     1        0              5        1          1  0.143 ± 0.0144
#>  6     1        0              6        1          2  0.099 ± 0.0099
#>  7     1        0              7        1          3  0.040 ± 0.0051
#>  8     1        0              8        1          4  0.019 ± 0.0039
#>  9     1        1              1        0          4  0.025 ± 0.0045
#> 10     1        1              2        0          3  0.043 ± 0.0057
#> 11     1        1              3        0          2  0.088 ± 0.0087
#> 12     1        1              4        0          1  0.155 ± 0.0149
#> 13     1        1              5        1          1  0.171 ± 0.0131
#> 14     1        1              6        1          2  0.208 ± 0.0145
#> 15     1        1              7        1          3  0.153 ± 0.0139
#> 16     1        1              8        1          4  0.158 ± 0.0152
```
