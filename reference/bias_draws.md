# Obtain posterior draws of an index of metacognitive bias

Computes \\\textrm{meta-}\Delta\\, an index of metacognitive bias.
\\\textrm{meta-}\Delta\\ is the distance between `meta_c` and the
average of the the confidence criteria `meta_c2_0` and `meta_c2_1`. For
`metacognitive_bias_draws` and `add_metacognitive_bias_draws`,
parameters are returned in a tidy tibble with one row per posterior draw
and per response. For `metacognitive_bias_rvars` and
`add_metacognitive_bias_rvars`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata` and per response.

## Usage

``` r
metacognitive_bias_draws(object, newdata, ..., by_response = TRUE)

add_metacognitive_bias_draws(newdata, object, ...)

metacognitive_bias_rvars(object, newdata, ..., by_response = TRUE)

add_metacognitive_bias_rvars(newdata, object, ...)
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

- by_response:

  If `TRUE`, compute metacognitive bias separately for the two type 1
  responses. If `FALSE`, compute an un-weighted average of the two
  measures.

## Value

a tibble containing posterior draws of \\\textrm{meta-}\Delta\\ with the
following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `metacognitive_bias_draws` and
  `add_metacognitive_bias_draws`, identifiers for the posterior sample

- `response`: the type 1 response for perceived stimulus presence

- `metacognitive_bias`: the distance between `meta_c` and the average of
  the confidence criteria `meta_c2_{response}`.

## See also

[`tidybayes::linpred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html),
[`tidybayes::linpred_rvars()`](https://mjskay.github.io/tidybayes/reference/add_predicted_rvars.html)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# compute metacognitive bias
# equivalent to `add_metacognitive_bias_draws(newdata, example_model())`
metacognitive_bias_draws(example_model(), newdata)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: Rejecting initial value:
#> Chain 1:   Error evaluating the log probability at the initial value.
#> Chain 1: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
#> Chain 1: Rejecting initial value:
#> Chain 1:   Error evaluating the log probability at the initial value.
#> Chain 1: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2.2e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.22 seconds.
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
#> Chain 1:  Elapsed Time: 0.026 seconds (Warm-up)
#> Chain 1:                0.018 seconds (Sampling)
#> Chain 1:                0.044 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: Rejecting initial value:
#> Chain 2:   Error evaluating the log probability at the initial value.
#> Chain 2: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[7] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.3e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:   1 / 500 [  0%]  (Warmup)
#> Chain 2: Iteration:  50 / 500 [ 10%]  (Warmup)
#> Chain 2: Iteration: 100 / 500 [ 20%]  (Warmup)
#> Chain 2: Iteration: 150 / 500 [ 30%]  (Warmup)
#> Chain 2: Iteration: 200 / 500 [ 40%]  (Warmup)
#> Chain 2: Iteration: 250 / 500 [ 50%]  (Warmup)
#> Chain 2: Iteration: 251 / 500 [ 50%]  (Sampling)
#> Chain 2: Iteration: 300 / 500 [ 60%]  (Sampling)
#> Chain 2: Iteration: 350 / 500 [ 70%]  (Sampling)
#> Chain 2: Iteration: 400 / 500 [ 80%]  (Sampling)
#> Chain 2: Iteration: 450 / 500 [ 90%]  (Sampling)
#> Chain 2: Iteration: 500 / 500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.028 seconds (Warm-up)
#> Chain 2:                0.024 seconds (Sampling)
#> Chain 2:                0.052 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: Rejecting initial value:
#> Chain 3:   Error evaluating the log probability at the initial value.
#> Chain 3: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[6] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1.2e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:   1 / 500 [  0%]  (Warmup)
#> Chain 3: Iteration:  50 / 500 [ 10%]  (Warmup)
#> Chain 3: Iteration: 100 / 500 [ 20%]  (Warmup)
#> Chain 3: Iteration: 150 / 500 [ 30%]  (Warmup)
#> Chain 3: Iteration: 200 / 500 [ 40%]  (Warmup)
#> Chain 3: Iteration: 250 / 500 [ 50%]  (Warmup)
#> Chain 3: Iteration: 251 / 500 [ 50%]  (Sampling)
#> Chain 3: Iteration: 300 / 500 [ 60%]  (Sampling)
#> Chain 3: Iteration: 350 / 500 [ 70%]  (Sampling)
#> Chain 3: Iteration: 400 / 500 [ 80%]  (Sampling)
#> Chain 3: Iteration: 450 / 500 [ 90%]  (Sampling)
#> Chain 3: Iteration: 500 / 500 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0.027 seconds (Warm-up)
#> Chain 3:                0.021 seconds (Sampling)
#> Chain 3:                0.048 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1.2e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:   1 / 500 [  0%]  (Warmup)
#> Chain 4: Iteration:  50 / 500 [ 10%]  (Warmup)
#> Chain 4: Iteration: 100 / 500 [ 20%]  (Warmup)
#> Chain 4: Iteration: 150 / 500 [ 30%]  (Warmup)
#> Chain 4: Iteration: 200 / 500 [ 40%]  (Warmup)
#> Chain 4: Iteration: 250 / 500 [ 50%]  (Warmup)
#> Chain 4: Iteration: 251 / 500 [ 50%]  (Sampling)
#> Chain 4: Iteration: 300 / 500 [ 60%]  (Sampling)
#> Chain 4: Iteration: 350 / 500 [ 70%]  (Sampling)
#> Chain 4: Iteration: 400 / 500 [ 80%]  (Sampling)
#> Chain 4: Iteration: 450 / 500 [ 90%]  (Sampling)
#> Chain 4: Iteration: 500 / 500 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0.026 seconds (Warm-up)
#> Chain 4:                0.022 seconds (Sampling)
#> Chain 4:                0.048 seconds (Total)
#> Chain 4: 
#> # A tibble: 2,000 × 6
#> # Groups:   .row, response [2]
#>     .row response .chain .iteration .draw metacognitive_bias
#>    <int>    <int>  <int>      <int> <int>              <dbl>
#>  1     1        0     NA         NA     1              0.957
#>  2     1        0     NA         NA     2              1.11 
#>  3     1        0     NA         NA     3              1.10 
#>  4     1        0     NA         NA     4              1.07 
#>  5     1        0     NA         NA     5              1.07 
#>  6     1        0     NA         NA     6              0.990
#>  7     1        0     NA         NA     7              1.04 
#>  8     1        0     NA         NA     8              1.00 
#>  9     1        0     NA         NA     9              1.05 
#> 10     1        0     NA         NA    10              1.02 
#> # ℹ 1,990 more rows

# use `posterior::rvar` for increased efficiency
# equivalent to `add_metacognitive_bias_rvars(newdata, example_model())`
metacognitive_bias_rvars(example_model(), newdata)
#> # A tibble: 2 × 3
#> # Groups:   .row, response [2]
#>    .row response metacognitive_bias
#>   <dbl>    <int>         <rvar[1d]>
#> 1     1        0          1 ± 0.045
#> 2     1        1          1 ± 0.044

# average over the two type 1 responses
metacognitive_bias_rvars(example_model(), newdata, by_response = FALSE)
#> # A tibble: 1 × 2
#> # Groups:   .row [1]
#>    .row metacognitive_bias
#>   <dbl>         <rvar[1d]>
#> 1     1           1 ± 0.03
# }
```
