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
# running few iterations so example runs quickly, use more in practice
m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1.9e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.19 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:   1 / 500 [  0%]  (Warmup)
#> Chain 1: Iteration:  50 / 500 [ 10%]  (Warmup)
#> Chain 1: Iteration: 100 / 500 [ 20%]  (Warmup)
#> Chain 1: Iteration: 150 / 500 [ 30%]  (Warmup)
#> Chain 1: Iteration: 200 / 500 [ 40%]  (Warmup)
#> Chain 1: Iteration: 250 / 500 [ 50%]  (Warmup)
#> Chain 1: Iteration: 251 / 500 [ 50%]  (Sampling)
#> Chain 1: Iteration: 300 / 500 [ 60%]  (Sampling)
#> Chain 1: Iteration: 350 / 500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 400 / 500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 450 / 500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 500 / 500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.034 seconds (Warm-up)
#> Chain 1:                0.029 seconds (Sampling)
#> Chain 1:                0.063 seconds (Total)
#> Chain 1: 
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
newdata <- tidyr::tibble(.row = 1)

# compute mean confidence by stimulus and response
mean_confidence_draws(m, newdata)
#> # A tibble: 1,000 × 7
#> # Groups:   .row, stimulus, response [4]
#>     .row .chain .iteration .draw stimulus response .epred
#>    <int>  <int>      <int> <int>    <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0        0   2.21
#>  2     1     NA         NA     1        0        1   1.98
#>  3     1     NA         NA     1        1        0   1.78
#>  4     1     NA         NA     1        1        1   2.42
#>  5     1     NA         NA     2        0        0   2.23
#>  6     1     NA         NA     2        0        1   1.76
#>  7     1     NA         NA     2        1        0   1.43
#>  8     1     NA         NA     2        1        1   2.48
#>  9     1     NA         NA     3        0        0   2.26
#> 10     1     NA         NA     3        0        1   2.01
#> # ℹ 990 more rows
add_mean_confidence_draws(newdata, m)
#> # A tibble: 1,000 × 7
#> # Groups:   .row, stimulus, response [4]
#>     .row .chain .iteration .draw stimulus response .epred
#>    <int>  <int>      <int> <int>    <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0        0   2.21
#>  2     1     NA         NA     1        0        1   1.98
#>  3     1     NA         NA     1        1        0   1.78
#>  4     1     NA         NA     1        1        1   2.42
#>  5     1     NA         NA     2        0        0   2.23
#>  6     1     NA         NA     2        0        1   1.76
#>  7     1     NA         NA     2        1        0   1.43
#>  8     1     NA         NA     2        1        1   2.48
#>  9     1     NA         NA     3        0        0   2.26
#> 10     1     NA         NA     3        0        1   2.01
#> # ℹ 990 more rows

# compute mean confidence by stimulus
mean_confidence_draws(m, newdata, by_response = FALSE)
#> # A tibble: 500 × 6
#> # Groups:   .row, stimulus [2]
#>     .row .chain .iteration .draw stimulus .epred
#>    <int>  <int>      <int> <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0   2.14
#>  2     1     NA         NA     1        1   2.25
#>  3     1     NA         NA     2        0   2.14
#>  4     1     NA         NA     2        1   2.06
#>  5     1     NA         NA     3        0   2.20
#>  6     1     NA         NA     3        1   2.13
#>  7     1     NA         NA     4        0   2.15
#>  8     1     NA         NA     4        1   2.06
#>  9     1     NA         NA     5        0   2.09
#> 10     1     NA         NA     5        1   2.08
#> # ℹ 490 more rows

# compute mean confidence by response
mean_confidence_draws(m, newdata, by_stimulus = FALSE)
#> # A tibble: 500 × 6
#> # Groups:   .row, response [2]
#>     .row .chain .iteration .draw response .epred
#>    <int>  <int>      <int> <int>    <int>  <dbl>
#>  1     1     NA         NA     1        0   2.09
#>  2     1     NA         NA     1        1   2.29
#>  3     1     NA         NA     2        0   1.96
#>  4     1     NA         NA     2        1   2.31
#>  5     1     NA         NA     3        0   2.14
#>  6     1     NA         NA     3        1   2.20
#>  7     1     NA         NA     4        0   2.11
#>  8     1     NA         NA     4        1   2.09
#>  9     1     NA         NA     5        0   1.94
#> 10     1     NA         NA     5        1   2.30
#> # ℹ 490 more rows

# compute mean confidence averaging over stimuli and responses
mean_confidence_draws(m, newdata, by_stimulus = FALSE, by_response = FALSE)
#> # A tibble: 250 × 5
#> # Groups:   .row [1]
#>     .row .chain .iteration .draw .epred
#>    <int>  <int>      <int> <int>  <dbl>
#>  1     1     NA         NA     1   2.20
#>  2     1     NA         NA     2   2.10
#>  3     1     NA         NA     3   2.16
#>  4     1     NA         NA     4   2.10
#>  5     1     NA         NA     5   2.08
#>  6     1     NA         NA     6   2.12
#>  7     1     NA         NA     7   2.28
#>  8     1     NA         NA     8   2.21
#>  9     1     NA         NA     9   2.14
#> 10     1     NA         NA    10   2.21
#> # ℹ 240 more rows

# use posterior::rvar for increased efficiency
mean_confidence_rvars(m, newdata)
#> # A tibble: 4 × 4
#> # Groups:   .row, stimulus, response [4]
#>    .row stimulus response      .epred
#>   <int>    <int>    <int>  <rvar[1d]>
#> 1     1        0        0  2.3 ± 0.17
#> 2     1        0        1  1.8 ± 0.22
#> 3     1        1        0  1.7 ± 0.20
#> 4     1        1        1  2.3 ± 0.18
```
