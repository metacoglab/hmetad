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
newdata <- tidyr::tibble(.row = 1)

# compute mean confidence by stimulus and response
# equivalent to `add_mean_confidence_draws(newdata, example_model)`
mean_confidence_draws(example_model, newdata)
#> # A tibble: 4,000 × 7
#> # Groups:   .row, stimulus, response [4]
#>     .row .chain .iteration .draw stimulus response .epred
#>    <int>  <int>      <int> <int>    <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0        0   2.48
#>  2     1     NA         NA     1        0        1   1.75
#>  3     1     NA         NA     1        1        0   1.80
#>  4     1     NA         NA     1        1        1   2.39
#>  5     1     NA         NA     2        0        0   2.56
#>  6     1     NA         NA     2        0        1   1.70
#>  7     1     NA         NA     2        1        0   1.80
#>  8     1     NA         NA     2        1        1   2.41
#>  9     1     NA         NA     3        0        0   2.45
#> 10     1     NA         NA     3        0        1   1.78
#> # ℹ 3,990 more rows

# compute mean confidence by stimulus
# equivalent to `add_mean_confidence_draws(newdata, example_model, by_response = FALSE)`
mean_confidence_draws(example_model, newdata, by_response = FALSE)
#> # A tibble: 2,000 × 6
#> # Groups:   .row, stimulus [2]
#>     .row .chain .iteration .draw stimulus .epred
#>    <int>  <int>      <int> <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0   2.25
#>  2     1     NA         NA     1        1   2.22
#>  3     1     NA         NA     2        0   2.30
#>  4     1     NA         NA     2        1   2.23
#>  5     1     NA         NA     3        0   2.22
#>  6     1     NA         NA     3        1   2.27
#>  7     1     NA         NA     4        0   2.27
#>  8     1     NA         NA     4        1   2.27
#>  9     1     NA         NA     5        0   2.31
#> 10     1     NA         NA     5        1   2.17
#> # ℹ 1,990 more rows

# compute mean confidence by response
# equivalent to `add_mean_confidence_draws(newdata, example_model, by_stimulus = FALSE)`
mean_confidence_draws(example_model, newdata, by_stimulus = FALSE)
#> # A tibble: 2,000 × 6
#> # Groups:   .row, response [2]
#>     .row .chain .iteration .draw response .epred
#>    <int>  <int>      <int> <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0   2.27
#>  2     1     NA         NA     1        1   2.20
#>  3     1     NA         NA     2        0   2.33
#>  4     1     NA         NA     2        1   2.19
#>  5     1     NA         NA     3        0   2.23
#>  6     1     NA         NA     3        1   2.26
#>  7     1     NA         NA     4        0   2.19
#>  8     1     NA         NA     4        1   2.36
#>  9     1     NA         NA     5        0   2.28
#> 10     1     NA         NA     5        1   2.19
#> # ℹ 1,990 more rows

# compute mean confidence by accuracy
# equivalent to `add_mean_confidence_draws(newdata, example_model, by_correct = TRUE)`
mean_confidence_draws(example_model, newdata, by_correct = TRUE)
#> # A tibble: 2,000 × 6
#> # Groups:   .row, correct [2]
#>     .row .chain .iteration .draw correct .epred
#>    <int>  <int>      <int> <int>   <int>  <dbl>
#>  1     1     NA         NA     1       0   1.77
#>  2     1     NA         NA     1       1   2.43
#>  3     1     NA         NA     2       0   1.75
#>  4     1     NA         NA     2       1   2.48
#>  5     1     NA         NA     3       0   1.77
#>  6     1     NA         NA     3       1   2.48
#>  7     1     NA         NA     4       0   1.80
#>  8     1     NA         NA     4       1   2.45
#>  9     1     NA         NA     5       0   1.77
#> 10     1     NA         NA     5       1   2.44
#> # ℹ 1,990 more rows

# compute mean confidence averaging over stimuli and responses
# equivalent to `add_mean_confidence_draws(newdata, example_model, ...)`
mean_confidence_draws(example_model, newdata, by_stimulus = FALSE, by_response = FALSE)
#> # A tibble: 1,000 × 5
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw .epred
#>    <int>  <int>      <int> <int>  <dbl>
#>  1     1     NA         NA     1   2.23
#>  2     1     NA         NA     2   2.26
#>  3     1     NA         NA     3   2.25
#>  4     1     NA         NA     4   2.27
#>  5     1     NA         NA     5   2.24
#>  6     1     NA         NA     6   2.26
#>  7     1     NA         NA     7   2.26
#>  8     1     NA         NA     8   2.23
#>  9     1     NA         NA     9   2.25
#> 10     1     NA         NA    10   2.20
#> # ℹ 990 more rows

# use `posterior::rvar` for increased efficiency
# equivalent to `add_mean_confidence_rvars(newdata, example_model)`
mean_confidence_rvars(example_model, newdata)
#> # A tibble: 4 × 4
#> # Groups:   .row, stimulus, response [4]
#>    .row stimulus response       .epred
#>   <int>    <int>    <int>   <rvar[1d]>
#> 1     1        0        0  2.5 ± 0.057
#> 2     1        0        1  1.8 ± 0.062
#> 3     1        1        0  1.8 ± 0.061
#> 4     1        1        1  2.4 ± 0.054
```
