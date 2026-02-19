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
#> Chain 1:  Elapsed Time: 0.039 seconds (Warm-up)
#> Chain 1:                0.054 seconds (Sampling)
#> Chain 1:                0.093 seconds (Total)
#> Chain 1: 
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess

# obtain model predictions
predicted_draws_metad(m, m$data)
add_predicted_draws_metad(m$data, m)

# obtain model predictions (posterior::rvar)
predicted_rvars_metad(m, m$data)
#> # A tibble: 16 × 7
#> # Groups:   .row, N, stimulus, joint_response, response, confidence [16]
#>     .row N[,"N_0_1"] stimulus joint_response response confidence  .prediction
#>    <int>       <int>    <int>          <int>    <int>      <dbl>   <rvar[1d]>
#>  1     1           9        0              1        0          4   8.13 ± 3.6
#>  2     1           9        0              2        0          3   8.07 ± 3.6
#>  3     1           9        0              3        0          2  11.45 ± 3.6
#>  4     1           9        0              4        0          1  10.01 ± 4.0
#>  5     1           9        0              5        1          1   5.65 ± 3.2
#>  6     1           9        0              6        1          2   4.04 ± 2.3
#>  7     1           9        0              7        1          3   1.87 ± 1.5
#>  8     1           9        0              8        1          4   0.78 ± 1.0
#>  9     1           9        1              1        0          4   1.30 ± 1.3
#> 10     1           9        1              2        0          3   2.83 ± 2.0
#> 11     1           9        1              3        0          2   6.61 ± 3.0
#> 12     1           9        1              4        0          1  11.02 ± 3.9
#> 13     1           9        1              5        1          1   7.78 ± 3.4
#> 14     1           9        1              6        1          2   8.44 ± 3.5
#> 15     1           9        1              7        1          3   6.06 ± 2.9
#> 16     1           9        1              8        1          4   5.95 ± 3.0
#> # ℹ 1 more variable: N[2:16] <int>
add_predicted_rvars_metad(m$data, m)
#> # A tibble: 16 × 7
#> # Groups:   .row, N, stimulus, joint_response, response, confidence [16]
#>     .row N[,"N_0_1"] stimulus joint_response response confidence  .prediction
#>    <int>       <int>    <int>          <int>    <int>      <dbl>   <rvar[1d]>
#>  1     1           9        0              1        0          4   8.31 ± 3.3
#>  2     1           9        0              2        0          3   8.28 ± 3.4
#>  3     1           9        0              3        0          2  10.99 ± 3.8
#>  4     1           9        0              4        0          1  10.02 ± 3.7
#>  5     1           9        0              5        1          1   5.87 ± 3.0
#>  6     1           9        0              6        1          2   3.82 ± 2.4
#>  7     1           9        0              7        1          3   1.83 ± 1.5
#>  8     1           9        0              8        1          4   0.88 ± 1.0
#>  9     1           9        1              1        0          4   1.41 ± 1.4
#> 10     1           9        1              2        0          3   2.80 ± 2.1
#> 11     1           9        1              3        0          2   6.56 ± 3.1
#> 12     1           9        1              4        0          1  10.75 ± 4.2
#> 13     1           9        1              5        1          1   7.82 ± 3.2
#> 14     1           9        1              6        1          2   8.51 ± 3.4
#> 15     1           9        1              7        1          3   6.26 ± 2.7
#> 16     1           9        1              8        1          4   5.88 ± 3.0
#> # ℹ 1 more variable: N[2:16] <int>
```
