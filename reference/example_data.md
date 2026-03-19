# Simulated data for example model fitting

A simulated data set of 1000 trials from a two-alternative forced choice
task with 4 levels of confidence.

## Usage

``` r
example_data
```

## Format

A tibble of 1000 observations containing the following columns:

- `trial`: the trial number

- `stimulus`: the stimulus presence (`0` or `1`)

- `response`: the simulated type 1 response

- `confidence`: the simulated type 2 response

- `correct`: the accuracy of the simulated type 1 response

- `dprime`, `c`, `meta_dprime`, `M`, `meta_c2_0`, `meta_c2_1`: the
  parameters of the model used for simulation

- `theta`, `theta_1`, `theta_2`: the joint, type 1, and type 2 response
  probabilities of the model used for simulation

## Source

Generated using the code `sim_metad(N_trials = 1000)`

## See also

[`sim_metad()`](https://metacoglab.github.io/hmetad/reference/sim_metad.md)

## Examples

``` r
# \donttest{
fit_metad(N ~ 1, example_data, chains = 1, iter = 500)
#> Compiling Stan program...
#> Start sampling
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 3e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.3 seconds.
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
#> Chain 1:                0.024 seconds (Sampling)
#> Chain 1:                0.051 seconds (Total)
#> Chain 1: 
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#>  Family: metad__4__normal__absolute__multinomial 
#>   Links: mu = log 
#> Formula: N ~ 1 
#>    Data: data.aggregated (Number of observations: 1) 
#>   Draws: 1 chains, each with iter = 500; warmup = 250; thin = 1;
#>          total post-warmup draws = 250
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     0.09      0.14    -0.22     0.33 1.02      208       83
#> 
#> Further Distributional Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> dprime              1.03      0.09     0.87     1.21 1.01      221      209
#> c                   0.01      0.04    -0.06     0.10 1.00      190      206
#> metac2zero1diff     0.51      0.04     0.44     0.58 1.01      232      170
#> metac2zero2diff     0.47      0.04     0.41     0.55 1.00      313      222
#> metac2zero3diff     0.47      0.05     0.38     0.57 1.00      397      160
#> metac2one1diff      0.47      0.03     0.41     0.53 1.00      272      195
#> metac2one2diff      0.55      0.05     0.47     0.65 1.04      372       85
#> metac2one3diff      0.51      0.05     0.41     0.61 1.00      203      178
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
# }
```
