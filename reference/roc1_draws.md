# Obtain posterior draws of the pseudo type 1 receiver operating characteristic (ROC) curve.

Given a data frame and a meta-d' model, adds estimates of the cumulative
probability over joint_responses. For `roc1_draws` and `add_roc1_draws`,
estimates are returned in a tidy tibble with one row per posterior draw
and per joint response. For `roc1_rvars` and `add_roc1_rvars`,
parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata` and per joint response.

## Usage

``` r
roc1_draws(object, newdata, ..., bounds = FALSE)

add_roc1_draws(newdata, object, ...)

roc1_rvars(object, newdata, ..., bounds = FALSE)

add_roc1_rvars(newdata, object, ...)
```

## Arguments

- object:

  The `brms` model with the `metad` family

- newdata:

  A data frame from which to generate posterior predictions

- ...:

  Additional parameters passed to
  [tidybayes::epred_draws](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
  or
  [tidybayes::epred_rvars](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

- bounds:

  If `TRUE`, include the endpoints of the ROC at \\(0, 0)\\ and \\(1,
  1)\\. Otherwise, the endpoints are excluded.

## Value

a tibble containing posterior draws of the pseudo type 1 ROC with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `roc1_draws` and
  `add_roc1_draws`, identifiers for the posterior sample

- `joint_response`: the combined type 1 / type 2 response (\\J \in \[1,
  2K\]\\) for \\K\\ confidence levels)

- `response`: the type 1 response for perceived stimulus presence (\\R
  \in \\0, 1\\\\)

- `confidence`: the type 2 confidence response (\\C \in \[1, K\]\\)

- `p_fa`: the cumulative probability of a 'present'/'old' response for
  `stimulus==0` (\\P(J \ge j \\\vert\\ S=0)\\)

- `p_hit`: the cumulative probability of a 'present'/'old' response for
  `stimulus==1` (\\P(J \ge j \\\vert\\ S=1)\\)

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
#> Chain 1:  Elapsed Time: 0.043 seconds (Warm-up)
#> Chain 1:                0.045 seconds (Sampling)
#> Chain 1:                0.088 seconds (Total)
#> Chain 1: 
#> Warning: The largest R-hat is 1.07, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
newdata <- tidyr::tibble(.row = 1)

# compute pseudo-type 1 ROC curve
roc1_draws(m, newdata)
#> # A tibble: 1,750 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <int>    <int>      <dbl>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              1        0          4     NA         NA     1 0.784 0.976
#>  2     1              1        0          4     NA         NA     2 0.767 0.956
#>  3     1              1        0          4     NA         NA     3 0.765 0.957
#>  4     1              1        0          4     NA         NA     4 0.865 0.985
#>  5     1              1        0          4     NA         NA     5 0.770 0.915
#>  6     1              1        0          4     NA         NA     6 0.798 0.964
#>  7     1              1        0          4     NA         NA     7 0.888 0.971
#>  8     1              1        0          4     NA         NA     8 0.730 0.947
#>  9     1              1        0          4     NA         NA     9 0.851 0.962
#> 10     1              1        0          4     NA         NA    10 0.728 0.981
#> # ℹ 1,740 more rows
add_roc1_draws(newdata, m)
#> # A tibble: 1,750 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <int>    <int>      <dbl>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              1        0          4     NA         NA     1 0.784 0.976
#>  2     1              1        0          4     NA         NA     2 0.767 0.956
#>  3     1              1        0          4     NA         NA     3 0.765 0.957
#>  4     1              1        0          4     NA         NA     4 0.865 0.985
#>  5     1              1        0          4     NA         NA     5 0.770 0.915
#>  6     1              1        0          4     NA         NA     6 0.798 0.964
#>  7     1              1        0          4     NA         NA     7 0.888 0.971
#>  8     1              1        0          4     NA         NA     8 0.730 0.947
#>  9     1              1        0          4     NA         NA     9 0.851 0.962
#> 10     1              1        0          4     NA         NA    10 0.728 0.981
#> # ℹ 1,740 more rows

# use posterior::rvar for additional efficiency
roc1_rvars(m, newdata)
#> # A tibble: 7 × 6
#> # Groups:   .row, joint_response, response, confidence [7]
#>    .row joint_response response confidence           p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          4  0.804 ± 0.051  0.96 ± 0.016
#> 2     1              2        0          3  0.657 ± 0.064  0.93 ± 0.025
#> 3     1              3        0          2  0.447 ± 0.059  0.87 ± 0.042
#> 4     1              4        0          1  0.237 ± 0.056  0.81 ± 0.057
#> 5     1              5        1          1  0.135 ± 0.037  0.52 ± 0.067
#> 6     1              6        1          2  0.066 ± 0.026  0.29 ± 0.063
#> 7     1              7        1          3  0.025 ± 0.013  0.12 ± 0.041
add_roc1_draws(newdata, m)
#> # A tibble: 1,750 × 9
#> # Groups:   .row, joint_response, response, confidence [7]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <int>    <int>      <dbl>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              1        0          4     NA         NA     1 0.784 0.976
#>  2     1              1        0          4     NA         NA     2 0.767 0.956
#>  3     1              1        0          4     NA         NA     3 0.765 0.957
#>  4     1              1        0          4     NA         NA     4 0.865 0.985
#>  5     1              1        0          4     NA         NA     5 0.770 0.915
#>  6     1              1        0          4     NA         NA     6 0.798 0.964
#>  7     1              1        0          4     NA         NA     7 0.888 0.971
#>  8     1              1        0          4     NA         NA     8 0.730 0.947
#>  9     1              1        0          4     NA         NA     9 0.851 0.962
#> 10     1              1        0          4     NA         NA    10 0.728 0.981
#> # ℹ 1,740 more rows

# include the ROC bounds
roc1_draws(m, newdata, bounds = TRUE)
#> # A tibble: 2,250 × 9
#> # Groups:   .row, joint_response, response, confidence [9]
#>     .row joint_response response confidence .chain .iteration .draw  p_fa p_hit
#>    <int>          <dbl>    <dbl>      <dbl>  <int>      <int> <int> <dbl> <dbl>
#>  1     1              0        0          5     NA         NA     1     1     1
#>  2     1              0        0          5     NA         NA     2     1     1
#>  3     1              0        0          5     NA         NA     3     1     1
#>  4     1              0        0          5     NA         NA     4     1     1
#>  5     1              0        0          5     NA         NA     5     1     1
#>  6     1              0        0          5     NA         NA     6     1     1
#>  7     1              0        0          5     NA         NA     7     1     1
#>  8     1              0        0          5     NA         NA     8     1     1
#>  9     1              0        0          5     NA         NA     9     1     1
#> 10     1              0        0          5     NA         NA    10     1     1
#> # ℹ 2,240 more rows
```
