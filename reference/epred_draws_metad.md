# Obtain posterior draws of joint response probabilities

Given a data frame and a meta-d' model, adds estimates of joint type 1
and type 2 response probabilities. For `epred_draws_metad` and
`add_epred_draws_metad`, estimates are returned in a tidy tibble with
one row per posterior draw. For `epred_rvars_metad` and
`add_epred_rvars_metad`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
epred_draws_metad(object, newdata, ...)

add_epred_draws_metad(newdata, object, ...)

epred_rvars_metad(object, newdata, ...)

add_epred_rvars_metad(newdata, object, ...)
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

## Value

a tibble containing posterior draws of model parameters with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `epred_draws_metad`, identifiers
  for the posterior sample

- `stimulus`, `joint_response`, `response`, `confidence`: identifiers
  for the response type

- `.epred`: probability of the type 1 and type 2 response given the
  stimulus, \\P(R, C \\\vert\\ S)\\

## Examples

``` r
# running few iterations so example runs quickly, use more in practice
m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: Rejecting initial value:
#> Chain 1:   Gradient evaluated at the initial value is not finite.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: 
#> Chain 1: Gradient evaluation took 9e-06 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
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
#> Chain 1:  Elapsed Time: 0.027 seconds (Warm-up)
#> Chain 1:                0.023 seconds (Sampling)
#> Chain 1:                0.05 seconds (Total)
#> Chain 1: 
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
newdata <- tidyr::tibble(.row = 1)

# obtain model predictions
epred_draws_metad(m, newdata)
#> # A tibble: 4,000 × 9
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence .epred .chain .iteration
#>    <int>    <int>          <int>    <int>      <dbl>  <dbl>  <int>      <int>
#>  1     1        0              1        0          4 0.0863     NA         NA
#>  2     1        0              1        0          4 0.173      NA         NA
#>  3     1        0              1        0          4 0.103      NA         NA
#>  4     1        0              1        0          4 0.148      NA         NA
#>  5     1        0              1        0          4 0.127      NA         NA
#>  6     1        0              1        0          4 0.198      NA         NA
#>  7     1        0              1        0          4 0.143      NA         NA
#>  8     1        0              1        0          4 0.151      NA         NA
#>  9     1        0              1        0          4 0.122      NA         NA
#> 10     1        0              1        0          4 0.0978     NA         NA
#> # ℹ 3,990 more rows
#> # ℹ 1 more variable: .draw <int>
add_epred_draws_metad(newdata, m)
#> # A tibble: 4,000 × 9
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence .epred .chain .iteration
#>    <int>    <int>          <int>    <int>      <dbl>  <dbl>  <int>      <int>
#>  1     1        0              1        0          4 0.0863     NA         NA
#>  2     1        0              1        0          4 0.173      NA         NA
#>  3     1        0              1        0          4 0.103      NA         NA
#>  4     1        0              1        0          4 0.148      NA         NA
#>  5     1        0              1        0          4 0.127      NA         NA
#>  6     1        0              1        0          4 0.198      NA         NA
#>  7     1        0              1        0          4 0.143      NA         NA
#>  8     1        0              1        0          4 0.151      NA         NA
#>  9     1        0              1        0          4 0.122      NA         NA
#> 10     1        0              1        0          4 0.0978     NA         NA
#> # ℹ 3,990 more rows
#> # ℹ 1 more variable: .draw <int>

# obtain model predictions (posterior::rvar)
epred_rvars_metad(m, newdata)
#> # A tibble: 16 × 6
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence         .epred
#>    <int>    <int>          <int>    <int>      <dbl>     <rvar[1d]>
#>  1     1        0              1        0          4  0.140 ± 0.045
#>  2     1        0              2        0          3  0.186 ± 0.048
#>  3     1        0              3        0          2  0.278 ± 0.049
#>  4     1        0              4        0          1  0.169 ± 0.046
#>  5     1        0              5        1          1  0.118 ± 0.047
#>  6     1        0              6        1          2  0.067 ± 0.023
#>  7     1        0              7        1          3  0.021 ± 0.011
#>  8     1        0              8        1          4  0.022 ± 0.015
#>  9     1        1              1        0          4  0.020 ± 0.013
#> 10     1        1              2        0          3  0.050 ± 0.020
#> 11     1        1              3        0          2  0.133 ± 0.035
#> 12     1        1              4        0          1  0.134 ± 0.041
#> 13     1        1              5        1          1  0.213 ± 0.048
#> 14     1        1              6        1          2  0.202 ± 0.048
#> 15     1        1              7        1          3  0.093 ± 0.035
#> 16     1        1              8        1          4  0.155 ± 0.046
add_epred_rvars_metad(newdata, m)
#> # A tibble: 16 × 6
#> # Groups:   .row, stimulus, joint_response, response, confidence [16]
#>     .row stimulus joint_response response confidence         .epred
#>    <int>    <int>          <int>    <int>      <dbl>     <rvar[1d]>
#>  1     1        0              1        0          4  0.140 ± 0.045
#>  2     1        0              2        0          3  0.186 ± 0.048
#>  3     1        0              3        0          2  0.278 ± 0.049
#>  4     1        0              4        0          1  0.169 ± 0.046
#>  5     1        0              5        1          1  0.118 ± 0.047
#>  6     1        0              6        1          2  0.067 ± 0.023
#>  7     1        0              7        1          3  0.021 ± 0.011
#>  8     1        0              8        1          4  0.022 ± 0.015
#>  9     1        1              1        0          4  0.020 ± 0.013
#> 10     1        1              2        0          3  0.050 ± 0.020
#> 11     1        1              3        0          2  0.133 ± 0.035
#> 12     1        1              4        0          1  0.134 ± 0.041
#> 13     1        1              5        1          1  0.213 ± 0.048
#> 14     1        1              6        1          2  0.202 ± 0.048
#> 15     1        1              7        1          3  0.093 ± 0.035
#> 16     1        1              8        1          4  0.155 ± 0.046
```
