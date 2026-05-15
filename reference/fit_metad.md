# Fit the meta-d' model using `brms` package

This function is a wrapper around
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) using
a custom family for the meta-d' model.

## Usage

``` r
fit_metad(
  formula,
  data,
  ...,
  aggregate = TRUE,
  .stimulus = "stimulus",
  .response = "response",
  .confidence = "confidence",
  .joint_response = "joint_response",
  K = NULL,
  distribution = "normal",
  metac_absolute = TRUE,
  stanvars = NULL,
  categorical = FALSE,
  logit = TRUE
)
```

## Arguments

- formula:

  A model formula for some or all parameters of the `metad` brms family.
  To display all parameter names for a model with `K` confidence levels,
  use `metad(K)`.

- data:

  A tibble containing the data to fit the model.

  - If `aggregate`==TRUE, `data` should have one row per observation
    with columns `stimulus`, `response`, `confidence`, and any other
    variables in `formula`

  - If `aggregate`==FALSE, it should be aggregated to have one row per
    cell of the design matrix, with joint type 1/type 2 response counts
    in a matrix column (see
    [`aggregate_metad()`](https://metacoglab.github.io/hmetad/reference/aggregate_metad.md)).

- ...:

  Additional parameters passed to the `brm` function.

- aggregate:

  If `TRUE`, automatically aggregate `data` by the variables included in
  `formula` using
  [`aggregate_metad()`](https://metacoglab.github.io/hmetad/reference/aggregate_metad.md).
  Otherwise, `data` should already be aggregated.

- .stimulus:

  The name of "stimulus" column

- .response:

  The name of "response" column

- .confidence:

  The name of "confidence" column

- .joint_response:

  The name of "joint_response" column

- K:

  The number of confidence levels in `data`. If `NULL`, this is
  estimated from `data` using the maximum level of either the confidence
  column or joint response column.

- distribution:

  The noise distribution to use for the signal detection model. By
  default, uses a normal distribution with a mean parameterized by
  `dprime`.

- metac_absolute:

  If `TRUE`, fix the type 2 criterion to be equal to the type 1
  criterion. Otherwise, equate the criteria relatively such that
  metac/metadprime = c/dprime.

- stanvars:

  Additional `stanvars` to pass to the model code, for example to define
  an alternative distribution or a custom model prior (see
  [`brms::stanvar()`](https://paulbuerkner.com/brms/reference/stanvar.html)).

- categorical:

  If `FALSE` (default), use the multinomial likelihood over aggregated
  data. If `TRUE`, use the categorical likelihood over individual
  trials.

- logit:

  If `TRUE` (default), use the logit parameterization of the likelihood
  over the log joint response probabilities. If `FALSE`, use the
  standard parameterization of the likelihood over the actual joint
  response probabilities. In most cases, the logit parameterization
  should provide more stable numerical computations, but the standard
  parameterization might be preferable in some settings.

## Value

A `brmsfit` object containing the fitted model

## Details

`fit_metad(formula, data, ...)` is approximately the same as
`brm(formula, data=aggregate_metad(data, ...), family=metad(...), stanvars=stanvars_metad(...), ...)`.
For some models, it may often be easier to use the more explicit version
than using `fit_metad`.

## Examples

``` r
# check which parameters the model has
metad(3)
#> 
#> Custom family: metad__3__normal__absolute__multinomial 
#> Link function: log 
#> Parameters: mu, dprime, c, metac2zero1diff, metac2zero2diff, metac2one1diff, metac2one2diff 
#> 

# fit a basic model on simulated data
# (use `empty=true` to bypass fitting, *do not use in real analysis*)
fit_metad(N ~ 1, sim_metad(), empty = TRUE)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#> 
#> The model does not contain posterior draws.
# \donttest{
# fit a basic model on simulated data
fit_metad(N ~ 1, sim_metad())
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2.1e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.21 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.118 seconds (Warm-up)
#> Chain 1:                0.112 seconds (Sampling)
#> Chain 1:                0.23 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.3e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.111 seconds (Warm-up)
#> Chain 2:                0.112 seconds (Sampling)
#> Chain 2:                0.223 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1.3e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0.114 seconds (Warm-up)
#> Chain 3:                0.102 seconds (Sampling)
#> Chain 3:                0.216 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1.2e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0.109 seconds (Warm-up)
#> Chain 4:                0.119 seconds (Sampling)
#> Chain 4:                0.228 seconds (Total)
#> Chain 4: 
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     0.18      0.31    -0.51     0.74 1.00     2847     2447
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.56      0.28     1.03     2.13 1.00     4120     2983
#> c                  -0.09      0.13    -0.34     0.18 1.00     3952     3008
#> metac2zero1diff     0.64      0.14     0.39     0.94 1.00     3894     3223
#> metac2zero2diff     0.50      0.13     0.28     0.78 1.00     4591     3109
#> metac2zero3diff     0.58      0.16     0.30     0.93 1.00     4640     2960
#> metac2one1diff      0.52      0.13     0.30     0.80 1.00     4131     2640
#> metac2one2diff      0.80      0.17     0.49     1.17 1.00     4292     3013
#> metac2one3diff      0.69      0.18     0.36     1.07 1.00     4352     2527
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).

# fit a model with condition-level effects
fit_metad(
  bf(
    N ~ condition,
    dprime + c + metac2zero1diff + metac2zero2diff +
      metac2one1diff + metac2one1diff ~ condition
  ),
  data = sim_metad_condition()
)
#> `hmetad` has inferred that there are K=4 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 3.7e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.37 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.926 seconds (Warm-up)
#> Chain 1:                1.838 seconds (Sampling)
#> Chain 1:                2.764 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: Rejecting initial value:
#> Chain 2:   Error evaluating the log probability at the initial value.
#> Chain 2: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 159, column 6 to column 200)
#> Chain 2: Rejecting initial value:
#> Chain 2:   Error evaluating the log probability at the initial value.
#> Chain 2: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 159, column 6 to column 200)
#> Chain 2: Rejecting initial value:
#> Chain 2:   Error evaluating the log probability at the initial value.
#> Chain 2: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 159, column 6 to column 200)
#> Chain 2: 
#> Chain 2: Gradient evaluation took 2.5e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.25 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.824 seconds (Warm-up)
#> Chain 2:                1.851 seconds (Sampling)
#> Chain 2:                2.675 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 2.5e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.25 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 1.69 seconds (Warm-up)
#> Chain 3:                1.868 seconds (Sampling)
#> Chain 3:                3.558 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 2.6e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.26 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 3.404 seconds (Warm-up)
#> Chain 4:                1.779 seconds (Sampling)
#> Chain 4:                5.183 seconds (Total)
#> Chain 4: 
#> Warning: There were 1 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log; dprime = identity; c = identity; metac2zero1diff = log; metac2zero2diff = log; metac2one1diff = log 
#> Formula: N ~ condition 
#>          dprime ~ condition
#>          c ~ condition
#>          metac2zero1diff ~ condition
#>          metac2zero2diff ~ condition
#>          metac2one1diff ~ condition
#>    Data: data.aggregated (Number of observations: 2) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Regression Coefficients:
#>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> Intercept                    -6.34     15.29   -44.66    14.95 1.01      854
#> dprime_Intercept              1.48      0.60     0.32     2.63 1.00     3526
#> c_Intercept                  -0.45      0.28    -1.00     0.10 1.00     2545
#> metac2zero1diff_Intercept    -1.39      0.53    -2.48    -0.40 1.00     2559
#> metac2zero2diff_Intercept    -0.94      0.62    -2.22     0.21 1.00     3016
#> metac2one1diff_Intercept     -0.89      0.49    -1.88     0.06 1.00     2882
#> condition                     1.26      9.59   -17.51    20.53 1.01      677
#> dprime_condition             -0.36      0.37    -1.07     0.35 1.00     3608
#> c_condition                   0.26      0.18    -0.09     0.61 1.00     2571
#> metac2zero1diff_condition     0.46      0.31    -0.15     1.07 1.00     2572
#> metac2zero2diff_condition     0.01      0.40    -0.76     0.79 1.00     3018
#> metac2one1diff_condition      0.12      0.31    -0.49     0.73 1.00     2829
#>                           Tail_ESS
#> Intercept                      527
#> dprime_Intercept              2688
#> c_Intercept                   2566
#> metac2zero1diff_Intercept     2254
#> metac2zero2diff_Intercept     2271
#> metac2one1diff_Intercept      2518
#> condition                      459
#> dprime_condition              2790
#> c_condition                   2391
#> metac2zero1diff_condition     2580
#> metac2zero2diff_condition     2509
#> metac2one1diff_condition      2354
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> metac2zero3diff     0.57      0.12     0.37     0.81 1.00     3394     2385
#> metac2one2diff      0.41      0.07     0.28     0.57 1.00     3261     2507
#> metac2one3diff      0.55      0.11     0.36     0.77 1.00     3332     2584
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
# }
```
