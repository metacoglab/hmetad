# Obtain posterior draws of the area under the pseudo type 1 receiver operating characteristic (ROC) curve.

Given a data frame and a meta-d' model, adds estimates of the area under
the type 1 ROC curve. For `auroc1_draws` and `add_auroc1_draws`,
estimates are returned in a tidy tibble with one row per posterior draw.
For `auroc1_rvars` and `add_auroc1_rvars`, parameters are returned as
[posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)s,
with one row per row in `newdata`.

## Usage

``` r
auroc1_draws(
  object,
  newdata,
  ...,
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response"
)

add_auroc1_draws(newdata, object, ...)

auroc1_rvars(
  object,
  newdata,
  ...,
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response"
)

add_auroc1_rvars(newdata, object, ...)
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

- .response:

  The name of "response" column

- .confidence:

  The name of "confidence" column

- .joint_response:

  The name of "joint_response" column

## Value

a tibble containing posterior draws of the area under the pseudo type 1
ROC with the following columns:

- `.row`: the row of `newdata`

- `.chain`, `.iteration`, `.draw`: for `auroc1_draws` and
  `add_auroc1_draws`, identifiers for the posterior sample

- `auroc1`: the area under the pseudo type 1 ROC curve

## See also

[`auroc1()`](https://metacoglab.github.io/hmetad/reference/auroc1.md),
[`roc1_draws()`](https://metacoglab.github.io/hmetad/reference/roc1_draws.md),
[`roc1_rvars()`](https://metacoglab.github.io/hmetad/reference/roc1_draws.md)

## Examples

``` r
# \donttest{
newdata <- tidyr::tibble(.row = 1)

# compute pseudo-type 1 ROC curve
# equivalent to `auroc1_draws(example_model(), newdata)`
add_auroc1_draws(newdata, example_model())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2.3e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.23 seconds.
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
#> Chain 1:  Elapsed Time: 0.03 seconds (Warm-up)
#> Chain 1:                0.025 seconds (Sampling)
#> Chain 1:                0.055 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
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
#> Chain 2:  Elapsed Time: 0.027 seconds (Warm-up)
#> Chain 2:                0.018 seconds (Sampling)
#> Chain 2:                0.045 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: Rejecting initial value:
#> Chain 3:   Error evaluating the log probability at the initial value.
#> Chain 3: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
#> Chain 3: Rejecting initial value:
#> Chain 3:   Error evaluating the log probability at the initial value.
#> Chain 3: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1.3e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
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
#> Chain 3:  Elapsed Time: 0.026 seconds (Warm-up)
#> Chain 3:                0.02 seconds (Sampling)
#> Chain 3:                0.046 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: Rejecting initial value:
#> Chain 4:   Error evaluating the log probability at the initial value.
#> Chain 4: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[7] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
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
#> Chain 4:  Elapsed Time: 0.031 seconds (Warm-up)
#> Chain 4:                0.022 seconds (Sampling)
#> Chain 4:                0.053 seconds (Total)
#> Chain 4: 
#> # A tibble: 1,000 × 3
#> # Groups:   .row [1]
#>     .row .draw auroc1
#>    <int> <int>  <dbl>
#>  1     1     1  0.754
#>  2     1     2  0.754
#>  3     1     3  0.749
#>  4     1     4  0.768
#>  5     1     5  0.742
#>  6     1     6  0.764
#>  7     1     7  0.758
#>  8     1     8  0.761
#>  9     1     9  0.777
#> 10     1    10  0.756
#> # ℹ 990 more rows

# use posterior::rvar for additional efficiency
# equivalent to `add_auroc1_draws(newdata, example_model())`
auroc1_rvars(example_model(), newdata)
#> # A tibble: 1 × 2
#>    .row        auroc1
#>   <int>    <rvar[1d]>
#> 1     1  0.77 ± 0.015
# }
```
