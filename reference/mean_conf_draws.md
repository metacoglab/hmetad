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
  by_stimulus = TRUE,
  by_response = TRUE,
  by_correct = FALSE
)

add_mean_confidence_draws(newdata, object, ...)

mean_confidence_rvars(
  object,
  newdata,
  ...,
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

- `stimulus`: indicator for stimulus presence (if
  `by_stimulus==TRUE & by_correct==FALSE`)

- `response`: indicator for type 1 response (if
  `by_response==TRUE & by_correct==FALSE`)

- `correct`: indicator for the accuracy of the type 1 response (if
  `by_correct==TRUE`)

- `.epred`: the predicted mean confidence

## See also

[`tidybayes::epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::epred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# compute mean confidence by stimulus and response
# equivalent to `add_mean_confidence_draws(newdata, example_model())`
mean_confidence_draws(example_model(), newdata)
#> # A tibble: 4,000 × 7
#> # Groups:   .row, stimulus, response [4]
#>     .row .chain .iteration .draw stimulus response .epred
#>    <int>  <int>      <int> <int>    <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0        0   2.55
#>  2     1     NA         NA     1        0        1   1.75
#>  3     1     NA         NA     1        1        0   1.94
#>  4     1     NA         NA     1        1        1   2.36
#>  5     1     NA         NA     2        0        0   2.34
#>  6     1     NA         NA     2        0        1   1.78
#>  7     1     NA         NA     2        1        0   1.66
#>  8     1     NA         NA     2        1        1   2.47
#>  9     1     NA         NA     3        0        0   2.34
#> 10     1     NA         NA     3        0        1   1.77
#> # ℹ 3,990 more rows

# compute mean confidence by stimulus
# equivalent to `add_mean_confidence_draws(newdata, example_model(), by_response = FALSE)`
mean_confidence_draws(example_model(), newdata, by_response = FALSE)
#> # A tibble: 2,000 × 6
#> # Groups:   .row, stimulus [2]
#>     .row .chain .iteration .draw stimulus .epred
#>    <int>  <int>      <int> <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0   2.35
#>  2     1     NA         NA     1        1   2.24
#>  3     1     NA         NA     2        0   2.16
#>  4     1     NA         NA     2        1   2.21
#>  5     1     NA         NA     3        0   2.15
#>  6     1     NA         NA     3        1   2.15
#>  7     1     NA         NA     4        0   2.19
#>  8     1     NA         NA     4        1   2.14
#>  9     1     NA         NA     5        0   2.18
#> 10     1     NA         NA     5        1   2.19
#> # ℹ 1,990 more rows

# compute mean confidence by response
# equivalent to `add_mean_confidence_draws(newdata, example_model(), by_stimulus = FALSE)`
mean_confidence_draws(example_model(), newdata, by_stimulus = FALSE)
#> # A tibble: 2,000 × 6
#> # Groups:   .row, response [2]
#>     .row .chain .iteration .draw response .epred
#>    <int>  <int>      <int> <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0   2.38
#>  2     1     NA         NA     1        1   2.20
#>  3     1     NA         NA     2        0   2.12
#>  4     1     NA         NA     2        1   2.25
#>  5     1     NA         NA     3        0   2.13
#>  6     1     NA         NA     3        1   2.18
#>  7     1     NA         NA     4        0   2.19
#>  8     1     NA         NA     4        1   2.15
#>  9     1     NA         NA     5        0   2.17
#> 10     1     NA         NA     5        1   2.19
#> # ℹ 1,990 more rows

# compute mean confidence by accuracy
# equivalent to `add_mean_confidence_draws(newdata, example_model(), by_correct = TRUE)`
mean_confidence_draws(example_model(), newdata, by_correct = TRUE)
#> # A tibble: 2,000 × 6
#> # Groups:   .row, correct [2]
#>     .row .chain .iteration .draw correct .epred
#>    <int>  <int>      <int> <int>   <int>  <dbl>
#>  1     1     NA         NA     1       0   1.86
#>  2     1     NA         NA     1       1   2.46
#>  3     1     NA         NA     2       0   1.72
#>  4     1     NA         NA     2       1   2.40
#>  5     1     NA         NA     3       0   1.75
#>  6     1     NA         NA     3       1   2.36
#>  7     1     NA         NA     4       0   1.75
#>  8     1     NA         NA     4       1   2.38
#>  9     1     NA         NA     5       0   1.76
#> 10     1     NA         NA     5       1   2.37
#> # ℹ 1,990 more rows

# compute mean confidence averaging over stimuli and responses
# equivalent to `add_mean_confidence_draws(newdata, example_model(), ...)`
mean_confidence_draws(example_model(), newdata, by_stimulus = FALSE, by_response = FALSE)
#> # A tibble: 1,000 × 5
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw .epred
#>    <int>  <int>      <int> <int>  <dbl>
#>  1     1     NA         NA     1   2.30
#>  2     1     NA         NA     2   2.18
#>  3     1     NA         NA     3   2.15
#>  4     1     NA         NA     4   2.17
#>  5     1     NA         NA     5   2.18
#>  6     1     NA         NA     6   2.25
#>  7     1     NA         NA     7   2.18
#>  8     1     NA         NA     8   2.21
#>  9     1     NA         NA     9   2.20
#> 10     1     NA         NA    10   2.25
#> # ℹ 990 more rows

# use `posterior::rvar` for increased efficiency
# equivalent to `add_mean_confidence_rvars(newdata, example_model())`
mean_confidence_rvars(example_model(), newdata)
#> # A tibble: 4 × 4
#> # Groups:   .row, stimulus, response [4]
#>    .row stimulus response       .epred
#>   <int>    <int>    <int>   <rvar[1d]>
#> 1     1        0        0  2.4 ± 0.053
#> 2     1        0        1  1.8 ± 0.059
#> 3     1        1        0  1.8 ± 0.063
#> 4     1        1        1  2.4 ± 0.052
# }
```
