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
metacognitive_bias_draws(object, newdata, ..., by_response = TRUE)

add_metacognitive_bias_draws(newdata, object, ...)

metacognitive_bias_rvars(object, newdata, ..., by_response = TRUE)

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

- `response`: the type 1 response for perceived stimulus presence

- `metacognitive_bias`: the distance between `meta_c` and the average of
  the confidence criteria `meta_c2_{response}`.

## Examples

``` r
if (FALSE) { # \dontrun{
# running few iterations so example runs quickly, use more in practice
example_data <- sim_metad(N_trials = 1000)
example_model <- fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
} # }
example_model <- hmetad:::example_model
newdata <- tidyr::tibble(.row = 1)

# compute metacognitive bias
metacognitive_bias_draws(example_model, newdata)
#> # A tibble: 500 × 6
#> # Groups:   .row, response [2]
#>     .row response .chain .iteration .draw metacognitive_bias
#>    <int>    <int>  <int>      <int> <int>              <dbl>
#>  1     1        0     NA         NA     1              0.949
#>  2     1        0     NA         NA     2              0.938
#>  3     1        0     NA         NA     3              0.974
#>  4     1        0     NA         NA     4              1.08 
#>  5     1        0     NA         NA     5              1.03 
#>  6     1        0     NA         NA     6              1.04 
#>  7     1        0     NA         NA     7              0.948
#>  8     1        0     NA         NA     8              0.941
#>  9     1        0     NA         NA     9              0.969
#> 10     1        0     NA         NA    10              1.01 
#> # ℹ 490 more rows
add_metacognitive_bias_draws(newdata, example_model)
#> # A tibble: 500 × 6
#> # Groups:   .row, response [2]
#>     .row response .chain .iteration .draw metacognitive_bias
#>    <int>    <int>  <int>      <int> <int>              <dbl>
#>  1     1        0     NA         NA     1              0.949
#>  2     1        0     NA         NA     2              0.938
#>  3     1        0     NA         NA     3              0.974
#>  4     1        0     NA         NA     4              1.08 
#>  5     1        0     NA         NA     5              1.03 
#>  6     1        0     NA         NA     6              1.04 
#>  7     1        0     NA         NA     7              0.948
#>  8     1        0     NA         NA     8              0.941
#>  9     1        0     NA         NA     9              0.969
#> 10     1        0     NA         NA    10              1.01 
#> # ℹ 490 more rows

# use posterior::rvar for increased efficiency
metacognitive_bias_rvars(example_model, newdata)
#> # A tibble: 2 × 3
#> # Groups:   .row, response [2]
#>    .row response metacognitive_bias
#>   <dbl>    <int>         <rvar[1d]>
#> 1     1        0       0.98 ± 0.044
#> 2     1        1       1.01 ± 0.047
add_metacognitive_bias_rvars(newdata, example_model)
#> # A tibble: 2 × 3
#> # Groups:   .row, response [2]
#>    .row response metacognitive_bias
#>   <dbl>    <int>         <rvar[1d]>
#> 1     1        0       0.98 ± 0.044
#> 2     1        1       1.01 ± 0.047

# average over the two type 1 responses
metacognitive_bias_draws(example_model, newdata, by_response = FALSE)
#> # A tibble: 250 × 5
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw metacognitive_bias
#>    <int>  <int>      <int> <int>              <dbl>
#>  1     1     NA         NA     1              1.00 
#>  2     1     NA         NA     2              1.01 
#>  3     1     NA         NA     3              0.994
#>  4     1     NA         NA     4              0.984
#>  5     1     NA         NA     5              1.02 
#>  6     1     NA         NA     6              0.996
#>  7     1     NA         NA     7              0.996
#>  8     1     NA         NA     8              1.00 
#>  9     1     NA         NA     9              1.02 
#> 10     1     NA         NA    10              1.01 
#> # ℹ 240 more rows
metacognitive_bias_rvars(example_model, newdata, by_response = FALSE)
#> # A tibble: 1 × 2
#> # Groups:   .row [1]
#>    .row metacognitive_bias
#>   <dbl>         <rvar[1d]>
#> 1     1        0.99 ± 0.03
```
