# Obtain posterior draws of mean confidence

Computes posterior mean confidence conditional on stimulus and response
(\\\mathbb{E}\[C \\\vert\\ S=s,R=r\]\\), stimulus (averaging over
responses, \\\mathbb{E}\[C \\\vert\\ S=s\]\\), response (averaging over
stimuli, \\\mathbb{E}\[C \\\vert\\ R=r\]\\), neither (averaging over
stimuli and responses, \\\mathbb{E}\[C\]\\), or accuracy
(\\\mathbb{E}\[C \\\vert\\ A=(r=s)\]\\). For `mean_confidence_draws` and
`add_mean_confidence_draws`, estimates are returned in a tidy tibble
with one row per posterior draw, stimulus, and response. For
`mean_confidence_rvars` and `add_mean_confidence_rvars`, estimates are
returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

`add_mean_confidence_draws` is an alias of `mean_confidence_draws` with
argument order swapped.

## Usage

``` r
mean_confidence_draws(
  object,
  newdata,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  by_stimulus = TRUE,
  by_response = TRUE,
  by_correct = FALSE
)

add_mean_confidence_draws(newdata, object, ...)

mean_confidence_rvars(
  object,
  newdata,
  ...,
  .stimulus = "stimulus",
  .response = "response",
  by_stimulus = TRUE,
  by_response = TRUE,
  by_correct = FALSE
)

add_mean_confidence_rvars(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional arguments to
  [tidybayes::epred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
  or
  [tidybayes::epred_rvars](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

- .stimulus:

  The name of "stimulus" column

- .response:

  The name of "response" column

- by_stimulus:

  If TRUE, predict mean confidence separately by stimulus. Otherwise,
  predict mean confidence averaging over stimuli. Ignored if
  `by_correct==TRUE`.

- by_response:

  If TRUE, predict mean confidence separately by response Otherwise,
  predict mean confidence averaging over responses. Ignored if
  `by_correct==TRUE`.

- by_correct:

  If TRUE, predict mean confidence separately for correct and incorrect
  responses.

## Value

a tibble containing posterior draws of mean confidence with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `mean_confidence_draws` and
  `add_mean_confidence_draws`, identifiers for the posterior sample

- `{.stimulus}`: indicator for stimulus presence (if
  `by_stimulus==TRUE & by_correct==FALSE`)

- `{.response}`: indicator for type 1 response (if
  `by_response==TRUE & by_correct==FALSE`)

- `correct`: indicator for the accuracy of the type 1 response (if
  `by_correct==TRUE`)

- `.epred`: the predicted mean confidence

## See also

[`mean_confidence()`](https://metacoglab.github.io/hmetad/reference/mean_confidence.md),
[`tidybayes::epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::epred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# compute mean confidence by stimulus and response
# equivalent to `add_mean_confidence_draws(newdata, example_model())`
mean_confidence_draws(example_model(), newdata)
#> # A tibble: 4,000 × 5
#> # Groups:   .row, stimulus, response [4]
#>     .row stimulus response .draw .epred
#>    <int>    <int>    <int> <int>  <dbl>
#>  1     1        0        0     1   2.38
#>  2     1        0        0     2   2.40
#>  3     1        0        0     3   2.41
#>  4     1        0        0     4   2.47
#>  5     1        0        0     5   2.40
#>  6     1        0        0     6   2.41
#>  7     1        0        0     7   2.45
#>  8     1        0        0     8   2.39
#>  9     1        0        0     9   2.46
#> 10     1        0        0    10   2.37
#> # ℹ 3,990 more rows

# compute mean confidence by stimulus
# equivalent to `add_mean_confidence_draws(newdata, example_model(), by_response = FALSE)`
mean_confidence_draws(example_model(), newdata, by_response = FALSE)
#> # A tibble: 2,000 × 4
#> # Groups:   .row, stimulus [2]
#>     .row stimulus .draw .epred
#>    <int>    <int> <int>  <dbl>
#>  1     1        0     1   2.20
#>  2     1        0     2   2.21
#>  3     1        0     3   2.22
#>  4     1        0     4   2.25
#>  5     1        0     5   2.19
#>  6     1        0     6   2.21
#>  7     1        0     7   2.25
#>  8     1        0     8   2.20
#>  9     1        0     9   2.27
#> 10     1        0    10   2.22
#> # ℹ 1,990 more rows

# compute mean confidence by response
# equivalent to `add_mean_confidence_draws(newdata, example_model(), by_stimulus = FALSE)`
mean_confidence_draws(example_model(), newdata, by_stimulus = FALSE)
#> # A tibble: 2,000 × 4
#> # Groups:   .row, response [2]
#>     .row response .draw .epred
#>    <int>    <int> <int>  <dbl>
#>  1     1        0     1   2.18
#>  2     1        0     2   2.24
#>  3     1        0     3   2.22
#>  4     1        0     4   2.25
#>  5     1        0     5   2.20
#>  6     1        0     6   2.22
#>  7     1        0     7   2.25
#>  8     1        0     8   2.21
#>  9     1        0     9   2.24
#> 10     1        0    10   2.19
#> # ℹ 1,990 more rows

# compute mean confidence by accuracy
# equivalent to `add_mean_confidence_draws(newdata, example_model(), by_correct = TRUE)`
mean_confidence_draws(example_model(), newdata, by_correct = TRUE)
#> # A tibble: 2,000 × 4
#> # Groups:   .row, correct [2]
#>     .row correct .draw .epred
#>    <int>   <int> <int>  <dbl>
#>  1     1       0     1   1.77
#>  2     1       0     2   1.82
#>  3     1       0     3   1.79
#>  4     1       0     4   1.75
#>  5     1       0     5   1.78
#>  6     1       0     6   1.76
#>  7     1       0     7   1.81
#>  8     1       0     8   1.77
#>  9     1       0     9   1.76
#> 10     1       0    10   1.81
#> # ℹ 1,990 more rows

# compute mean confidence averaging over stimuli and responses
# equivalent to `add_mean_confidence_draws(newdata, example_model(), ...)`
mean_confidence_draws(example_model(), newdata, by_stimulus = FALSE, by_response = FALSE)
#> # A tibble: 1,000 × 3
#> # Groups:   .row [1]
#>     .row .draw .epred
#>    <int> <int>  <dbl>
#>  1     1     1   2.18
#>  2     1     2   2.20
#>  3     1     3   2.19
#>  4     1     4   2.25
#>  5     1     5   2.19
#>  6     1     6   2.19
#>  7     1     7   2.25
#>  8     1     8   2.19
#>  9     1     9   2.26
#> 10     1    10   2.20
#> # ℹ 990 more rows

# use `posterior::rvar` for increased efficiency
# equivalent to `add_mean_confidence_rvars(newdata, example_model())`
mean_confidence_rvars(example_model(), newdata)
#> # A tibble: 4 × 4
#> # Groups:   .row, stimulus, response [4]
#>    .row stimulus response       .epred
#>   <int>    <int>    <int>   <rvar[1d]>
#> 1     1        0        0  2.4 ± 0.053
#> 2     1        0        1  1.8 ± 0.056
#> 3     1        1        0  1.8 ± 0.059
#> 4     1        1        1  2.4 ± 0.053
# }
```
