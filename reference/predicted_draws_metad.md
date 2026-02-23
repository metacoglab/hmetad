# Obtain posterior predictions of joint responses

Given a data frame and a meta-d' model, adds predictions of joint type 1
and type 2 responses For `predicted_draws_metad` and
`add_predicted_draws_metad`, predictions are returned in a tidy tibble
with one row per posterior draw. For `predicted_rvars_metad` and
`add_predicted_rvars_metad`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
predicted_draws_metad(object, newdata, ...)

add_predicted_draws_metad(newdata, object, ...)

predicted_rvars_metad(object, newdata, ...)

add_predicted_rvars_metad(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional arguments passed to
  [tidybayes::add_predicted_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
  or
  [tidybayes::add_predicted_rvars](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Value

a tibble containing posterior draws of model parameters with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `predicted_draws_metad`,
  identifiers for the posterior sample

- `stimulus`, `joint_response`, `response`, `confidence`: identifiers
  for the response type

- `.prediction`: predicted type 1 and type 2 responses given the
  stimulus

## Examples

``` r
# running few iterations so example runs quickly, use more in practice
m <- fit_metad(N ~ 1, sim_metad(), chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1.8e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.18 seconds.
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
#> Chain 1:  Elapsed Time: 0.031 seconds (Warm-up)
#> Chain 1:                0.026 seconds (Sampling)
#> Chain 1:                0.057 seconds (Total)
#> Chain 1: 
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

# obtain model predictions
predicted_draws_metad(m, m$data)
add_predicted_draws_metad(m$data, m)

# obtain model predictions (posterior::rvar)
predicted_rvars_metad(m, m$data)
#> # A tibble: 16 × 7
#> # Groups:   .row, N, stimulus, joint_response, response, confidence [16]
#>     .row N[,"N_0_1"] stimulus joint_response response confidence  .prediction
#>    <int>       <int>    <int>          <int>    <int>      <dbl>   <rvar[1d]>
#>  1     1          11        0              1        0          4   9.87 ± 4.0
#>  2     1          11        0              2        0          3   8.94 ± 3.6
#>  3     1          11        0              3        0          2   8.37 ± 3.2
#>  4     1          11        0              4        0          1  12.25 ± 4.2
#>  5     1          11        0              5        1          1   4.24 ± 2.5
#>  6     1          11        0              6        1          2   3.62 ± 2.4
#>  7     1          11        0              7        1          3   1.74 ± 1.6
#>  8     1          11        0              8        1          4   0.98 ± 1.1
#>  9     1          11        1              1        0          4   2.76 ± 2.1
#> 10     1          11        1              2        0          3   3.52 ± 2.1
#> 11     1          11        1              3        0          2   4.47 ± 2.3
#> 12     1          11        1              4        0          1   8.98 ± 3.7
#> 13     1          11        1              5        1          1   8.75 ± 3.7
#> 14     1          11        1              6        1          2   9.77 ± 3.7
#> 15     1          11        1              7        1          3   6.17 ± 3.1
#> 16     1          11        1              8        1          4   5.57 ± 2.9
#> # ℹ 1 more variable: N[2:16] <int>
add_predicted_rvars_metad(m$data, m)
#> # A tibble: 16 × 7
#> # Groups:   .row, N, stimulus, joint_response, response, confidence [16]
#>     .row N[,"N_0_1"] stimulus joint_response response confidence .prediction
#>    <int>       <int>    <int>          <int>    <int>      <dbl>  <rvar[1d]>
#>  1     1          11        0              1        0          4   9.9 ± 3.8
#>  2     1          11        0              2        0          3   9.2 ± 3.4
#>  3     1          11        0              3        0          2   8.1 ± 3.3
#>  4     1          11        0              4        0          1  11.7 ± 4.1
#>  5     1          11        0              5        1          1   4.4 ± 2.5
#>  6     1          11        0              6        1          2   3.7 ± 2.3
#>  7     1          11        0              7        1          3   1.7 ± 1.5
#>  8     1          11        0              8        1          4   1.1 ± 1.2
#>  9     1          11        1              1        0          4   2.6 ± 2.1
#> 10     1          11        1              2        0          3   3.6 ± 2.1
#> 11     1          11        1              3        0          2   4.5 ± 2.6
#> 12     1          11        1              4        0          1   8.9 ± 3.7
#> 13     1          11        1              5        1          1   9.0 ± 3.6
#> 14     1          11        1              6        1          2   9.8 ± 3.5
#> 15     1          11        1              7        1          3   6.1 ± 3.0
#> 16     1          11        1              8        1          4   5.6 ± 2.9
#> # ℹ 1 more variable: N[2:16] <int>
```
