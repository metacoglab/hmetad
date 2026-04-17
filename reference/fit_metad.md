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
#> Chain 1: Gradient evaluation took 1.8e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.18 seconds.
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
#> Chain 1:  Elapsed Time: 0.138 seconds (Warm-up)
#> Chain 1:                0.105 seconds (Sampling)
#> Chain 1:                0.243 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.2e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
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
#> Chain 2:  Elapsed Time: 0.135 seconds (Warm-up)
#> Chain 2:                0.125 seconds (Sampling)
#> Chain 2:                0.26 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: Rejecting initial value:
#> Chain 3:   Error evaluating the log probability at the initial value.
#> Chain 3: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
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
#> Chain 3:  Elapsed Time: 0.113 seconds (Warm-up)
#> Chain 3:                0.201 seconds (Sampling)
#> Chain 3:                0.314 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: Rejecting initial value:
#> Chain 4:   Error evaluating the log probability at the initial value.
#> Chain 4: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
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
#> Chain 4:  Elapsed Time: 0.129 seconds (Warm-up)
#> Chain 4:                0.174 seconds (Sampling)
#> Chain 4:                0.303 seconds (Total)
#> Chain 4: 
#> Warning: There were 2 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#> Warning: There were 2 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept    -2.82      3.68   -11.05    -0.10 1.01      687      243
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.09      0.27     0.54     1.60 1.00     3149     2356
#> c                   0.03      0.13    -0.22     0.29 1.00     2666     2857
#> metac2zero1diff     0.33      0.09     0.18     0.54 1.00     3105     2229
#> metac2zero2diff     0.48      0.11     0.29     0.71 1.00     3488     2297
#> metac2zero3diff     0.70      0.16     0.42     1.05 1.00     3337     2755
#> metac2one1diff      0.56      0.11     0.36     0.80 1.00     2766     2290
#> metac2one2diff      0.60      0.14     0.36     0.88 1.00     3424     2735
#> metac2one3diff      0.48      0.16     0.21     0.84 1.00     3360     2130
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
#> Chain 1: Rejecting initial value:
#> Chain 1:   Error evaluating the log probability at the initial value.
#> Chain 1: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[6] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 159, column 6 to column 200)
#> Chain 1: Rejecting initial value:
#> Chain 1:   Error evaluating the log probability at the initial value.
#> Chain 1: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[7] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 159, column 6 to column 200)
#> Chain 1: 
#> Chain 1: Gradient evaluation took 4.2e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.42 seconds.
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
#> Chain 1:  Elapsed Time: 0.65 seconds (Warm-up)
#> Chain 1:                1.113 seconds (Sampling)
#> Chain 1:                1.763 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 2.4e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.24 seconds.
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
#> Chain 2:  Elapsed Time: 0.521 seconds (Warm-up)
#> Chain 2:                1.016 seconds (Sampling)
#> Chain 2:                1.537 seconds (Total)
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
#> Chain 3:  Elapsed Time: 0.735 seconds (Warm-up)
#> Chain 3:                0.969 seconds (Sampling)
#> Chain 3:                1.704 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 2.4e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.24 seconds.
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
#> Chain 4:  Elapsed Time: 0.744 seconds (Warm-up)
#> Chain 4:                0.974 seconds (Sampling)
#> Chain 4:                1.718 seconds (Total)
#> Chain 4: 
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
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
#> Intercept                    -5.26     10.52   -32.10     1.49 1.01      561
#> dprime_Intercept              0.97      0.61    -0.19     2.14 1.00     4186
#> c_Intercept                  -0.52      0.30    -1.11     0.05 1.00     3358
#> metac2zero1diff_Intercept    -1.58      0.62    -2.85    -0.41 1.00     3249
#> metac2zero2diff_Intercept    -1.60      0.62    -2.86    -0.48 1.00     3104
#> metac2one1diff_Intercept     -0.44      0.47    -1.38     0.43 1.00     2899
#> condition                     2.54      5.33    -1.43    15.91 1.01      555
#> dprime_condition              0.10      0.38    -0.64     0.85 1.00     4318
#> c_condition                   0.31      0.19    -0.06     0.67 1.00     3515
#> metac2zero1diff_condition     0.44      0.37    -0.29     1.18 1.00     3724
#> metac2zero2diff_condition     0.64      0.36    -0.05     1.35 1.00     3217
#> metac2one1diff_condition     -0.10      0.31    -0.70     0.51 1.00     2864
#>                           Tail_ESS
#> Intercept                      317
#> dprime_Intercept              2700
#> c_Intercept                   3140
#> metac2zero1diff_Intercept     2855
#> metac2zero2diff_Intercept     2284
#> metac2one1diff_Intercept      2758
#> condition                      316
#> dprime_condition              2624
#> c_condition                   2812
#> metac2zero1diff_condition     2826
#> metac2zero2diff_condition     2763
#> metac2one1diff_condition      2839
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> metac2zero3diff     0.43      0.10     0.26     0.63 1.00     4418     2825
#> metac2one2diff      0.48      0.09     0.33     0.67 1.00     3457     1775
#> metac2one3diff      0.41      0.09     0.25     0.62 1.00     3939     2690
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
# }
```
