# Calculate posterior draws of type 2 response probabilities

Given a data frame and a meta-d' model, adds estimates of type 2
response probabilities (i.e., \\P(C=c \vert S=s, R=r)\\, \\P(C=c \vert
S=s)\\, \\P(C=c \vert R=r)\\ or \\P(C=c)\\ for stimulus \\S\\), type 1
response \\R\\, and type 2 response \\C\\. For `type2_draws_metad` and
`add_type2_draws_metad`, estimates are returned in a tidy tibble with
one row per posterior draw. For `type2_rvars_metad` and
`add_type2_rvars_metad`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
type2_draws(
  object,
  newdata,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  by_stimulus = TRUE,
  by_response = TRUE
)

add_type2_draws(newdata, object, ...)

type2_rvars(
  object,
  newdata,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  by_stimulus = TRUE,
  by_response = TRUE
)

add_type2_rvars(newdata, object, ...)
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

- .stimulus:

  The name of "stimulus" column

- .response:

  The name of "response" column

- .confidence:

  The name of "confidence" column

- by_stimulus:

  If `TRUE` (default), calculate type 2 response probabilities
  separately by stimulus. Otherwise, calculate unconditional type 2
  response probabilities as an unweighted average over stimuli.

- by_response:

  If `TRUE` (default), calculate type 2 response probabilities
  separately by type 1 response. Otherwise, calculate unconditional type
  2 response probabilities as an unweighted average over type 1
  responses.

## Value

a tibble containing posterior draws of model parameters with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `epred_draws_metad`, identifiers
  for the posterior sample

- `{.stimulus}`, `{.response}`, `{.confidence}`: identifiers for the
  response type

- `.epred`: probability of the type 1 and type 2 response given the
  stimulus, \\P(R, C \\\vert\\ S)\\

## See also

[`type2_probabilities()`](https://metacoglab.github.io/hmetad/reference/type2_probabilities.md)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# obtain model predictions
# equivalent to `add_type2_draws(newdata, example_model())`
type2_draws(example_model(), newdata)
#> # A tibble: 16,000 × 6
#> # Groups:   .row, stimulus, response, confidence [16]
#>     .row stimulus response confidence .draw .epred
#>    <int>    <int>    <int>      <int> <int>  <dbl>
#>  1     1        0        0          1     1  0.259
#>  2     1        0        0          1     2  0.265
#>  3     1        0        0          1     3  0.250
#>  4     1        0        0          1     4  0.211
#>  5     1        0        0          1     5  0.244
#>  6     1        0        0          1     6  0.272
#>  7     1        0        0          1     7  0.226
#>  8     1        0        0          1     8  0.257
#>  9     1        0        0          1     9  0.260
#> 10     1        0        0          1    10  0.278
#> # ℹ 15,990 more rows

# obtain model predictions (`posterior::rvar`)
# equivalent to `add_type2_rvars(newdata, example_model(), by_stimulus = FALSE)`
type2_rvars(example_model(), newdata, by_stimulus = FALSE)
#> # A tibble: 8 × 4
#> # Groups:   .row, response [2]
#>    .row response confidence        .epred
#>   <int>    <int>      <int>    <rvar[1d]>
#> 1     1        0          1  0.36 ± 0.023
#> 2     1        0          2  0.29 ± 0.021
#> 3     1        0          3  0.21 ± 0.018
#> 4     1        0          4  0.13 ± 0.014
#> 5     1        1          1  0.37 ± 0.021
#> 6     1        1          2  0.31 ± 0.021
#> 7     1        1          3  0.18 ± 0.017
#> 8     1        1          4  0.14 ± 0.014
# }
```
