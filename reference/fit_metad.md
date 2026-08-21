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
  allow_negative_values = FALSE,
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

- allow_negative_values:

  If `FALSE` (default), M-ratio is modeled on the logarithmic scale to
  prevent negative values. If `TRUE`, M-ratio is modeled on the identity
  scale which allows for `M <= 0`. Note that if
  `allow_negative_values=TRUE`, priors for the `Intercept` should be
  centered at `1` (not at `0` as on the logarithmic scale).

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
#> Chain 1:  Elapsed Time: 0.105 seconds (Warm-up)
#> Chain 1:                0.208 seconds (Sampling)
#> Chain 1:                0.313 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: Rejecting initial value:
#> Chain 2:   Error evaluating the log probability at the initial value.
#> Chain 2: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[8] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 81, column 6 to column 185)
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
#> Chain 2:  Elapsed Time: 0.234 seconds (Warm-up)
#> Chain 2:                0.117 seconds (Sampling)
#> Chain 2:                0.351 seconds (Total)
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
#> Chain 3:  Elapsed Time: 0.144 seconds (Warm-up)
#> Chain 3:                0.195 seconds (Sampling)
#> Chain 3:                0.339 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1.3e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
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
#> Chain 4:  Elapsed Time: 0.124 seconds (Warm-up)
#> Chain 4:                0.108 seconds (Sampling)
#> Chain 4:                0.232 seconds (Total)
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
#> Intercept    -0.56      0.94    -3.07     0.48 1.00     1385      606
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.36      0.27     0.82     1.90 1.00     3771     2746
#> c                   0.05      0.14    -0.22     0.32 1.00     3291     2896
#> metac2zero1diff     0.67      0.14     0.42     0.96 1.00     3053     2562
#> metac2zero2diff     0.36      0.11     0.18     0.59 1.00     3223     2297
#> metac2zero3diff     0.45      0.13     0.22     0.74 1.00     4001     2614
#> metac2one1diff      0.38      0.10     0.21     0.59 1.00     3001     2750
#> metac2one2diff      0.52      0.12     0.31     0.78 1.00     3601     2187
#> metac2one3diff      0.56      0.14     0.31     0.86 1.00     3526     2477
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
#> Chain 1: 
#> Chain 1: Gradient evaluation took 4.1e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.41 seconds.
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
#> Chain 1:  Elapsed Time: 0.433 seconds (Warm-up)
#> Chain 1:                0.658 seconds (Sampling)
#> Chain 1:                1.091 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: Rejecting initial value:
#> Chain 2:   Error evaluating the log probability at the initial value.
#> Chain 2: Exception: Exception: multinomial_logit_lpmf: log-probabilities parameter[5] is -inf, but must be finite! (in 'anon_model', line 43, column 2 to line 46, column 66) (in 'anon_model', line 159, column 6 to column 200)
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
#> Chain 2:  Elapsed Time: 0.377 seconds (Warm-up)
#> Chain 2:                0.744 seconds (Sampling)
#> Chain 2:                1.121 seconds (Total)
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
#> Chain 3:  Elapsed Time: 0.634 seconds (Warm-up)
#> Chain 3:                1.044 seconds (Sampling)
#> Chain 3:                1.678 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 2.5e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.25 seconds.
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
#> Chain 4:  Elapsed Time: 0.488 seconds (Warm-up)
#> Chain 4:                1.873 seconds (Sampling)
#> Chain 4:                2.361 seconds (Total)
#> Chain 4: 
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
#> Intercept                    -1.60      6.69   -20.66     7.74 1.01      528
#> dprime_Intercept              1.17      0.59     0.05     2.32 1.00     4018
#> c_Intercept                   0.09      0.29    -0.45     0.65 1.00     2812
#> metac2zero1diff_Intercept    -1.22      0.55    -2.36    -0.17 1.00     2525
#> metac2zero2diff_Intercept    -0.27      0.52    -1.30     0.72 1.00     3507
#> metac2one1diff_Intercept     -1.26      0.56    -2.42    -0.23 1.00     2721
#> condition                     0.30      4.03    -8.18     9.96 1.01      519
#> dprime_condition             -0.06      0.37    -0.77     0.67 1.00     3893
#> c_condition                  -0.02      0.18    -0.37     0.33 1.00     2721
#> metac2zero1diff_condition     0.30      0.34    -0.36     0.97 1.00     2511
#> metac2zero2diff_condition    -0.32      0.36    -1.03     0.36 1.00     2839
#> metac2one1diff_condition      0.37      0.34    -0.27     1.06 1.00     2480
#>                           Tail_ESS
#> Intercept                      477
#> dprime_Intercept              3011
#> c_Intercept                   2801
#> metac2zero1diff_Intercept     2534
#> metac2zero2diff_Intercept     3038
#> metac2one1diff_Intercept      2581
#> condition                      508
#> dprime_condition              3014
#> c_condition                   2669
#> metac2zero1diff_condition     2526
#> metac2zero2diff_condition     2596
#> metac2one1diff_condition      2518
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> metac2zero3diff     0.55      0.11     0.35     0.78 1.00     3441     2707
#> metac2one2diff      0.55      0.09     0.38     0.74 1.00     3321     2727
#> metac2one3diff      0.71      0.14     0.46     1.01 1.00     3257     2379
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
# }
```
