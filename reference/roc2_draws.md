# Obtain posterior draws of the response-specific type 2 receiver operating characteristic (ROC) curves.

Given a data frame and a meta-d' model, adds estimates of the cumulative
probability over confidence for each type 1 response. For `roc2_draws`
and `add_roc2_draws`, estimates are returned in a tidy tibble with one
row per posterior draw and per joint response. For `roc2_rvars` and
`add_roc2_rvars`, parameters are returned as
[`posterior::rvar`](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata` and per joint response.

## Usage

``` r
roc2_draws(object, newdata, ..., bounds = FALSE)

add_roc2_draws(newdata, object, ...)

roc2_rvars(object, newdata, ..., bounds = FALSE)

add_roc2_rvars(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional parameters passed to
  [tidybayes::epred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)

- bounds:

  If `TRUE`, include the endpoints of the ROC at \\(0, 0)\\ and \\(1,
  1)\\. Otherwise, the endpoints are excluded.

## Value

a tibble containing posterior draws of the pseudo type 1 ROC with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `roc2_draws` and
  `add_roc2_draws`, identifiers for the posterior sample

- `response`: the type 1 response for perceived stimulus presence (\\R
  \in \\0, 1\\\\)

- `confidence`: the type 2 confidence response (\\C \in \[1, K\]\\)

- `p_fa2`: the cumulative probability of an incorrect response (\\P(C\ge
  c \\\vert\\ R\ne S)\\)

- `p_hit2`: the cumulative probability of a correct response (\\P(C\ge c
  \\\vert\\ R = S)\\)

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
#> Chain 1:  Elapsed Time: 0.032 seconds (Warm-up)
#> Chain 1:                0.023 seconds (Sampling)
#> Chain 1:                0.055 seconds (Total)
#> Chain 1: 
#> Warning: The largest R-hat is 1.06, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
newdata <- tidyr::tibble(.row = 1)

# compute type 2 ROC curve
roc2_draws(m, newdata)
#> # A tibble: 1,500 × 8
#> # Groups:   .row, response, confidence [6]
#>     .row response confidence .chain .iteration .draw p_hit2  p_fa2
#>    <int>    <int>      <dbl>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          4     NA         NA     1  0.120 0.0304
#>  2     1        0          4     NA         NA     2  0.183 0.0186
#>  3     1        0          4     NA         NA     3  0.191 0.0911
#>  4     1        0          4     NA         NA     4  0.151 0.0597
#>  5     1        0          4     NA         NA     5  0.132 0.0363
#>  6     1        0          4     NA         NA     6  0.163 0.0860
#>  7     1        0          4     NA         NA     7  0.369 0.199 
#>  8     1        0          4     NA         NA     8  0.282 0.0504
#>  9     1        0          4     NA         NA     9  0.209 0.0710
#> 10     1        0          4     NA         NA    10  0.223 0.0773
#> # ℹ 1,490 more rows
add_roc2_draws(newdata, m)
#> # A tibble: 1,500 × 8
#> # Groups:   .row, response, confidence [6]
#>     .row response confidence .chain .iteration .draw p_hit2  p_fa2
#>    <int>    <int>      <dbl>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          4     NA         NA     1  0.120 0.0304
#>  2     1        0          4     NA         NA     2  0.183 0.0186
#>  3     1        0          4     NA         NA     3  0.191 0.0911
#>  4     1        0          4     NA         NA     4  0.151 0.0597
#>  5     1        0          4     NA         NA     5  0.132 0.0363
#>  6     1        0          4     NA         NA     6  0.163 0.0860
#>  7     1        0          4     NA         NA     7  0.369 0.199 
#>  8     1        0          4     NA         NA     8  0.282 0.0504
#>  9     1        0          4     NA         NA     9  0.209 0.0710
#> 10     1        0          4     NA         NA    10  0.223 0.0773
#> # ℹ 1,490 more rows

# use posterior::rvar for additional efficiency
roc2_rvars(m, newdata)
#> # A tibble: 6 × 5
#> # Groups:   .row, response, confidence [6]
#>    .row response confidence        p_hit2          p_fa2
#>   <int>    <int>      <dbl>    <rvar[1d]>     <rvar[1d]>
#> 1     1        0          2  0.74 ± 0.064  0.525 ± 0.096
#> 2     1        0          3  0.48 ± 0.078  0.243 ± 0.089
#> 3     1        0          4  0.21 ± 0.067  0.068 ± 0.045
#> 4     1        1          1  0.71 ± 0.065  0.488 ± 0.100
#> 5     1        1          2  0.42 ± 0.075  0.190 ± 0.075
#> 6     1        1          3  0.19 ± 0.063  0.055 ± 0.036
add_roc2_rvars(newdata, m)
#> # A tibble: 6 × 5
#> # Groups:   .row, response, confidence [6]
#>    .row response confidence        p_hit2          p_fa2
#>   <int>    <int>      <dbl>    <rvar[1d]>     <rvar[1d]>
#> 1     1        0          2  0.74 ± 0.064  0.525 ± 0.096
#> 2     1        0          3  0.48 ± 0.078  0.243 ± 0.089
#> 3     1        0          4  0.21 ± 0.067  0.068 ± 0.045
#> 4     1        1          1  0.71 ± 0.065  0.488 ± 0.100
#> 5     1        1          2  0.42 ± 0.075  0.190 ± 0.075
#> 6     1        1          3  0.19 ± 0.063  0.055 ± 0.036

# include the ROC bounds
roc2_draws(m, newdata, bounds = TRUE)
#> # A tibble: 2,500 × 8
#> # Groups:   .row, response, confidence [10]
#>     .row response confidence .chain .iteration .draw p_hit2  p_fa2
#>    <int>    <dbl>      <dbl>  <int>      <int> <int>  <dbl>  <dbl>
#>  1     1        0          4     NA         NA     1  0.120 0.0304
#>  2     1        0          4     NA         NA     2  0.183 0.0186
#>  3     1        0          4     NA         NA     3  0.191 0.0911
#>  4     1        0          4     NA         NA     4  0.151 0.0597
#>  5     1        0          4     NA         NA     5  0.132 0.0363
#>  6     1        0          4     NA         NA     6  0.163 0.0860
#>  7     1        0          4     NA         NA     7  0.369 0.199 
#>  8     1        0          4     NA         NA     8  0.282 0.0504
#>  9     1        0          4     NA         NA     9  0.209 0.0710
#> 10     1        0          4     NA         NA    10  0.223 0.0773
#> # ℹ 2,490 more rows
```
