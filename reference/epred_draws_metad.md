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

## See also

[`tidybayes::epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::epred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# obtain model predictions
# equivalent to `add_epred_draws_metad(newdata, example_model())`
epred_draws_metad(example_model(), newdata)
#> # A tibble: 16,000 × 9
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence .epred .chain .iteration
#>    <int>    <int>          <int>    <int>      <dbl>  <dbl>  <int>      <int>
#>  1     1        0              1        0          4  0.164     NA         NA
#>  2     1        0              1        0          4  0.126     NA         NA
#>  3     1        0              1        0          4  0.119     NA         NA
#>  4     1        0              1        0          4  0.129     NA         NA
#>  5     1        0              1        0          4  0.133     NA         NA
#>  6     1        0              1        0          4  0.153     NA         NA
#>  7     1        0              1        0          4  0.144     NA         NA
#>  8     1        0              1        0          4  0.153     NA         NA
#>  9     1        0              1        0          4  0.124     NA         NA
#> 10     1        0              1        0          4  0.165     NA         NA
#> # ℹ 15,990 more rows
#> # ℹ 1 more variable: .draw <int>

# obtain model predictions (`posterior::rvar`)
# equivalent to `add_epred_rvars_metad(newdata, example_model())`
epred_rvars_metad(example_model(), newdata)
#> # A tibble: 16 × 6
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence          .epred
#>    <int>    <int>          <int>    <int>      <dbl>      <rvar[1d]>
#>  1     1        0              1        0          4  0.145 ± 0.0150
#>  2     1        0              2        0          3  0.188 ± 0.0157
#>  3     1        0              3        0          2  0.196 ± 0.0145
#>  4     1        0              4        0          1  0.174 ± 0.0144
#>  5     1        0              5        1          1  0.143 ± 0.0136
#>  6     1        0              6        1          2  0.094 ± 0.0090
#>  7     1        0              7        1          3  0.041 ± 0.0055
#>  8     1        0              8        1          4  0.017 ± 0.0036
#>  9     1        1              1        0          4  0.017 ± 0.0036
#> 10     1        1              2        0          3  0.051 ± 0.0066
#> 11     1        1              3        0          2  0.098 ± 0.0093
#> 12     1        1              4        0          1  0.149 ± 0.0146
#> 13     1        1              5        1          1  0.177 ± 0.0142
#> 14     1        1              6        1          2  0.202 ± 0.0148
#> 15     1        1              7        1          3  0.158 ± 0.0135
#> 16     1        1              8        1          4  0.149 ± 0.0147
# }
```
