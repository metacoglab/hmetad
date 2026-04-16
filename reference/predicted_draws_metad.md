# Obtain posterior predictions of joint responses

Given a data frame and a meta-d' model, adds predictions of joint type 1
and type 2 responses For `predicted_draws_metad` and
`add_predicted_draws_metad`, predictions are returned in a tidy tibble
with one row per posterior draw. For `predicted_rvars_metad` and
`add_predicted_rvars_metad`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
predicted_draws_metad(object, newdata, ...)

add_predicted_draws_metad(newdata, object, ...)

predicted_rvars_metad(object, newdata, ...)

add_predicted_rvars_metad(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional arguments passed to
  [tidybayes::add_predicted_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
  or
  [tidybayes::add_predicted_rvars](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Value

a tibble containing posterior draws of model parameters with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `predicted_draws_metad`,
  identifiers for the posterior sample

- `stimulus`, `joint_response`, `response`, `confidence`: identifiers
  for the response type

- `.prediction`: predicted type 1 and type 2 responses given the
  stimulus

## See also

[`tidybayes::predicted_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::predicted_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
newdata <- aggregate_metad(example_data)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`

# obtain model predictions
# equivalent to `add_predicted_draws_metad(newdata, example_model)`
predicted_draws_metad(example_model, newdata)

# obtain model predictions (posterior::rvar)
# equivalent to `add_predicted_rvars_metad(newdata, example_model)`
predicted_rvars_metad(example_model, newdata)
#> # A tibble: 16 × 9
#> # Groups:   .row, N_0, N_1, N, stimulus, joint_response, response, confidence
#> #   [16]
#>     .row   N_0   N_1 N[,"N_0_1"] stimulus joint_response response confidence
#>    <int> <int> <int>       <int>    <int>          <int>    <int>      <dbl>
#>  1     1   500   500          89        0              1        0          4
#>  2     1   500   500          89        0              2        0          3
#>  3     1   500   500          89        0              3        0          2
#>  4     1   500   500          89        0              4        0          1
#>  5     1   500   500          89        0              5        1          1
#>  6     1   500   500          89        0              6        1          2
#>  7     1   500   500          89        0              7        1          3
#>  8     1   500   500          89        0              8        1          4
#>  9     1   500   500          89        1              1        0          4
#> 10     1   500   500          89        1              2        0          3
#> 11     1   500   500          89        1              3        0          2
#> 12     1   500   500          89        1              4        0          1
#> 13     1   500   500          89        1              5        1          1
#> 14     1   500   500          89        1              6        1          2
#> 15     1   500   500          89        1              7        1          3
#> 16     1   500   500          89        1              8        1          4
#> # ℹ 2 more variables: N[2:16] <int>, .prediction <rvar[1d]>
```
