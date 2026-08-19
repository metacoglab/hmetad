# Obtain posterior draws of an index of metacognitive bias

Computes \\\textrm{meta-}\Delta\\, an index of metacognitive bias.
\\\textrm{meta-}\Delta\\ is the distance between `meta_c` and the
average of the the confidence criteria `meta_c2_0` and `meta_c2_1`. For
`metacognitive_bias_draws` and `add_metacognitive_bias_draws`,
parameters are returned in a tidy tibble with one row per posterior draw
and per response. For `metacognitive_bias_rvars` and
`add_metacognitive_bias_rvars`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata` and per response.

## Usage

``` r
metacognitive_bias_draws(
  object,
  newdata,
  ...,
  .response = "response",
  by_response = TRUE
)

add_metacognitive_bias_draws(newdata, object, ...)

metacognitive_bias_rvars(
  object,
  newdata,
  ...,
  .response = "response",
  by_response = TRUE
)

add_metacognitive_bias_rvars(newdata, object, ...)
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

- by_response:

  If `TRUE`, compute metacognitive bias separately for the two type 1
  responses. If `FALSE`, compute an un-weighted average of the two
  measures.

## Value

a tibble containing posterior draws of \\\textrm{meta-}\Delta\\ with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `metacognitive_bias_draws` and
  `add_metacognitive_bias_draws`, identifiers for the posterior sample

- `{.response}`: the type 1 response for perceived stimulus presence

- `metacognitive_bias`: the distance between `meta_c` and the average of
  the confidence criteria `meta_c2_{response}`.

## See also

[`tidybayes::linpred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::linpred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# compute metacognitive bias
# equivalent to `add_metacognitive_bias_draws(newdata, example_model())`
metacognitive_bias_draws(example_model(), newdata)
#> # A tibble: 2,000 × 6
#> # Groups:   .row, response [2]
#>     .row response .chain .iteration .draw metacognitive_bias
#>    <int>    <int>  <int>      <int> <int>              <dbl>
#>  1     1        0     NA         NA     1              1.09 
#>  2     1        0     NA         NA     2              0.983
#>  3     1        0     NA         NA     3              1.05 
#>  4     1        0     NA         NA     4              1.05 
#>  5     1        0     NA         NA     5              1.04 
#>  6     1        0     NA         NA     6              1.03 
#>  7     1        0     NA         NA     7              1.02 
#>  8     1        0     NA         NA     8              1.05 
#>  9     1        0     NA         NA     9              1.06 
#> 10     1        0     NA         NA    10              1.07 
#> # ℹ 1,990 more rows

# use `posterior::rvar` for increased efficiency
# equivalent to `add_metacognitive_bias_rvars(newdata, example_model())`
metacognitive_bias_rvars(example_model(), newdata)
#> # A tibble: 2 × 3
#> # Groups:   .row, response [2]
#>    .row response metacognitive_bias
#>   <dbl>    <int>         <rvar[1d]>
#> 1     1        0          1 ± 0.044
#> 2     1        1          1 ± 0.043

# average over the two type 1 responses
metacognitive_bias_rvars(example_model(), newdata, by_response = FALSE)
#> # A tibble: 1 × 2
#> # Groups:   .row [1]
#>    .row metacognitive_bias
#>   <dbl>         <rvar[1d]>
#> 1     1          1 ± 0.028
# }
```
