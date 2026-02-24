# Obtain posterior draws of mean confidence

Computes posterior mean confidence conditional on stimulus and response
(\\\mathbb{E}\[C \\\vert\\ S=s,R=r\]\\), stimulus (averaging over
responses, \\\mathbb{E}\[C \\\vert\\ S=s\]\\), response (averaging over
stimuli, \\\mathbb{E}\[C \\\vert\\ R=r\]\\), or neither (averaging over
stimuli and responses, \\\mathbb{E}\[C\]\\). For `mean_confidence_draws`
and `add_mean_confidence_draws`, estimates are returned in a tidy tibble
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
  by_response = TRUE
)

add_mean_confidence_draws(newdata, object, ...)

mean_confidence_rvars(
  object,
  newdata,
  ...,
  by_stimulus = TRUE,
  by_response = TRUE
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
  predict mean confidence averaging over stimuli.

- by_response:

  If TRUE, predict mean confidence separately by response Otherwise,
  predict mean confidence averaging over responses.

## Value

a tibble containing posterior draws of mean confidence with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `mean_confidence_draws` and
  `add_mean_confidence_draws`, identifiers for the posterior sample

- `stimulus`: indicator for stimulus presence (if `by_stimulus==TRUE`)

- `response`: indicator for type 1 response (if `by_response==TRUE`)

- `.epred`: the predicted mean confidence

## Examples

``` r
if (FALSE) { # \dontrun{
# running few iterations so example runs quickly, use more in practice
example_data <- sim_metad(N_trials = 1000)
example_model <- fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
} # }
example_model <- hmetad:::example_model
newdata <- tidyr::tibble(.row = 1)

# compute mean confidence by stimulus and response
mean_confidence_draws(example_model, newdata)
#> # A tibble: 1,000 × 7
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
#> # ℹ 990 more rows
if (FALSE) { # \dontrun{
add_mean_confidence_draws(newdata, example_model)
} # }

if (FALSE) { # \dontrun{
# compute mean confidence by stimulus
mean_confidence_draws(example_model, newdata, by_response = FALSE)
} # }

if (FALSE) { # \dontrun{
# compute mean confidence by response
mean_confidence_draws(example_model, newdata, by_stimulus = FALSE)
} # }

if (FALSE) { # \dontrun{
# compute mean confidence averaging over stimuli and responses
mean_confidence_draws(example_model, newdata, by_stimulus = FALSE, by_response = FALSE)
} # }

if (FALSE) { # \dontrun{
# use posterior::rvar for increased efficiency
mean_confidence_rvars(example_model, newdata)
} # }
```
