# Calculate posterior draws of type 1 response probabilities

Given a data frame and a meta-d' model, adds estimates of type 1
response probabilities (i.e., \\P(R=r \vert S=s)\\ or \\P(R=r)\\ for
type 1 response \\R\\ and stimulus \\S\\). For `type1_draws_metad` and
`add_type1_draws_metad`, estimates are returned in a tidy tibble with
one row per posterior draw. For `type1_rvars_metad` and
`add_type1_rvars_metad`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
type1_draws(
  object,
  newdata,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  by_stimulus = TRUE
)

add_type1_draws(newdata, object, ...)

type1_rvars(
  object,
  newdata,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  by_stimulus = TRUE
)

add_type1_rvars(newdata, object, ...)
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

- .joint_response:

  The name of "joint_response" column

- by_stimulus:

  If `TRUE` (default), calculate conditional type 1 response
  probabilities \\P(R=r \vert S=s)\\. Otherwise, calculate unconditional
  response probabilities \\P(R=r)\\ as an unweighted average over
  stimuli.

## Value

a tibble containing posterior draws of model parameters with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `epred_draws_metad`, identifiers
  for the posterior sample

- `{.stimulus}`, `{.response}`: identifiers for the response type

- `.epred`: probability of the type 1 response (optionally given the
  stimulus)

## See also

[`type1_probabilities()`](https://metacoglab.github.io/hmetad/reference/type1_probabilities.md)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# obtain model predictions
# equivalent to `add_type1_draws(newdata, example_model())`
type1_draws(example_model(), newdata)
#> # A tibble: 4,000 × 5
#> # Groups:   .row, stimulus, response [4]
#>     .row stimulus response .draw .epred
#>    <int>    <int>    <int> <int>  <dbl>
#>  1     1        0        0     1  0.714
#>  2     1        0        0     2  0.691
#>  3     1        0        0     3  0.708
#>  4     1        0        0     4  0.693
#>  5     1        0        0     5  0.677
#>  6     1        0        0     6  0.711
#>  7     1        0        0     7  0.683
#>  8     1        0        0     8  0.702
#>  9     1        0        0     9  0.723
#> 10     1        0        0    10  0.722
#> # ℹ 3,990 more rows

# obtain model predictions (`posterior::rvar`)
# equivalent to `add_type1_rvars(newdata, example_model(), by_stimulus = FALSE)`
type1_rvars(example_model(), newdata, by_stimulus = FALSE)
#> # A tibble: 2 × 3
#> # Groups:   .row, response [2]
#>    .row response        .epred
#>   <int>    <int>    <rvar[1d]>
#> 1     1        0  0.51 ± 0.015
#> 2     1        1  0.49 ± 0.015
# }
```
