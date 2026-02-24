# Estimating trial-level effects

## Introduction

By default, the `hmetad` package uses aggregated data (i.e., counts of
the number of trials with the same stimulus, type 1 response, and type 2
response.) This is because data aggregation makes model fitting and
simulation much more efficient. But sometimes researchers will be
interested in trial-level effects.

One common example would be what is often called “crossed random
effects”. For example, in a design where all participants make responses
to the same set of items, a researcher might want to estimate both
participant-level and item-level effects on their model parameters.

We can simulate data from such a design like so:

``` r
library(tidyverse)
library(tidybayes)
library(hmetad)

## average model parameters
K <- 3 ## number of confidence levels
mu_log_M <- -0.5
mu_dprime <- 1.5
mu_c <- 0
mu_c2_0 <- rep(-1, K - 1)
mu_c2_1 <- rep(-1, K - 1)

## participant-level standard deviations
sd_log_M_participant <- 0.25
sd_dprime_participant <- 0.5
sd_c_participant <- 0.33
sd_c2_0_participant <- cov_matrix(rep(0.25, K - 1), diag(K - 1))
sd_c2_1_participant <- cov_matrix(rep(0.25, K - 1), diag(K - 1))

## item-level standard deviations
sd_log_M_item <- 0.1
sd_dprime_item <- 0.5
sd_c_item <- 0.75
sd_c2_0_item <- cov_matrix(rep(0.1, K - 1), diag(K - 1))
sd_c2_1_item <- cov_matrix(rep(0.1, K - 1), diag(K - 1))


## simulate data
d <- expand_grid(
  participant = 1:50,
  item = 1:10
) |>
  ## simulate participant-level differences
  group_by(participant) |>
  mutate(
    z_log_M_participant = rnorm(1, sd = sd_log_M_participant),
    z_dprime_participant = rnorm(1, sd = sd_dprime_participant),
    z_c_participant = rnorm(1, sd = sd_c_participant),
    z_c2_0_participant = list(rmulti_normal(1, mu = rep(0, K - 1), Sigma = sd_c2_0_participant)),
    z_c2_1_participant = list(rmulti_normal(1, mu = rep(0, K - 1), Sigma = sd_c2_1_participant))
  ) |>
  ## simulate item-level differences
  group_by(item) |>
  mutate(
    z_log_M_item = rnorm(1, sd = sd_log_M_item),
    z_dprime_item = rnorm(1, sd = sd_dprime_item),
    z_c_item = rnorm(1, sd = sd_c_item),
    z_c2_0_item = list(rmulti_normal(1, mu = rep(0, K - 1), Sigma = sd_c2_0_item)),
    z_c2_1_item = list(rmulti_normal(1, mu = rep(0, K - 1), Sigma = sd_c2_1_item))
  ) |>
  ungroup() |>
  ## compute model parameters
  mutate(
    log_M = mu_log_M + z_log_M_participant + z_log_M_item,
    dprime = mu_dprime + z_dprime_participant + z_dprime_item,
    c = mu_c + z_c_participant + z_c_item,
    c2_0_diff = map2(
      z_c2_0_participant, z_c2_0_item,
      ~ exp(mu_c2_0 + .x + .y)
    ),
    c2_1_diff = map2(
      z_c2_1_participant, z_c2_1_item,
      ~ exp(mu_c2_1 + .x + .y)
    )
  ) |>
  ## simulate two trials per participant/item (stimulus = 0 and stimulus = 1)
  mutate(trial = pmap(list(dprime, c, log_M, c2_0_diff, c2_1_diff), sim_metad, N_trials = 2)) |>
  select(participant, item, trial) |>
  unnest(trial)
```

    #> # A tibble: 1,000 × 16
    #>    participant  item trial stimulus response correct confidence dprime        c
    #>          <int> <int> <int>    <int>    <int>   <int>      <int>  <dbl>    <dbl>
    #>  1           1     1     1        0        0       1          3   1.76  0.00633
    #>  2           1     1     1        1        0       0          3   1.76  0.00633
    #>  3           1     2     1        0        0       1          1   2.46 -0.652  
    #>  4           1     2     1        1        1       1          3   2.46 -0.652  
    #>  5           1     3     1        0        0       1          2   2.34 -0.848  
    #>  6           1     3     1        1        1       1          3   2.34 -0.848  
    #>  7           1     4     1        0        0       1          2   2.83 -0.859  
    #>  8           1     4     1        1        1       1          3   2.83 -0.859  
    #>  9           1     5     1        0        0       1          3   1.88 -0.382  
    #> 10           1     5     1        1        1       1          3   1.88 -0.382  
    #> # ℹ 990 more rows
    #> # ℹ 7 more variables: meta_dprime <dbl>, M <dbl>, meta_c2_0 <list>,
    #> #   meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>, theta_2 <dbl>

Don’t worry about the details of the simulation code- what matters is
that we have a data set with repeated measures for participants:

``` r
count(d, participant)
#> # A tibble: 50 × 2
#>    participant     n
#>          <int> <int>
#>  1           1    20
#>  2           2    20
#>  3           3    20
#>  4           4    20
#>  5           5    20
#>  6           6    20
#>  7           7    20
#>  8           8    20
#>  9           9    20
#> 10          10    20
#> # ℹ 40 more rows
```

And repeated measures for items:

``` r
count(d, item)
#> # A tibble: 10 × 2
#>     item     n
#>    <int> <int>
#>  1     1   100
#>  2     2   100
#>  3     3   100
#>  4     4   100
#>  5     5   100
#>  6     6   100
#>  7     7   100
#>  8     8   100
#>  9     9   100
#> 10    10   100
```

## Standard model with data aggregation

If we would like, we can use the `fit_metad` function on this data with
participant-level and item-level effects. However, if we aggregate the
data ourselves, we can see that the aggregation doesn’t really help us
here:

``` r
aggregate_metad(d, participant, item)
#> # A tibble: 500 × 5
#>    participant item    N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"]
#>    <fct>       <fct> <int> <int>       <int>      <int>      <int>      <int>
#>  1 1           1         1     1           1          0          0          0
#>  2 1           2         1     1           0          0          1          0
#>  3 1           3         1     1           0          1          0          0
#>  4 1           4         1     1           0          1          0          0
#>  5 1           5         1     1           1          0          0          0
#>  6 1           6         1     1           1          0          0          0
#>  7 1           7         1     1           0          1          0          0
#>  8 1           8         1     1           0          0          0          0
#>  9 1           9         1     1           1          0          0          0
#> 10 1           10        1     1           1          0          0          0
#> # ℹ 490 more rows
#> # ℹ 1 more variable: N[5:12] <int>
```

As you can see, the aggregated data set has `500` rows (with two
observations per row), which is not much smaller than the trial-level
data that we started with! So, in this case, it will probably be easier
*not* to aggregate our data. Nevertheless, there is nothing stopping us
from fitting the model like normal:[¹](#fn1)

``` r
m.multinomial <- fit_metad(
  bf(
    N ~ 1 + (1 | participant) + (1 | item),
    dprime + c +
      metac2zero1diff + metac2zero2diff +
      metac2one1diff + metac2one2diff ~
      1 + (1 | participant) + (1 | item)
  ),
  data = d, init = 0,
  file = "models/multinomial.rds",
  prior = prior(normal(0, .25), class = Intercept) +
    prior(normal(0, .25), class = Intercept, dpar = dprime) +
    prior(normal(0, .25), class = Intercept, dpar = c) +
    prior(normal(0, .1), class = Intercept, dpar = metac2zero1diff) +
    prior(normal(0, .1), class = Intercept, dpar = metac2zero2diff) +
    prior(normal(0, .1), class = Intercept, dpar = metac2one1diff) +
    prior(normal(0, .1), class = Intercept, dpar = metac2one2diff) +
    prior(normal(0, 1), class = sd) +
    prior(normal(0, 1), class = sd, dpar = dprime) +
    prior(normal(0, 1), class = sd, dpar = c) +
    prior(normal(0, 1), class = sd, dpar = metac2zero1diff) +
    prior(normal(0, 1), class = sd, dpar = metac2zero2diff) +
    prior(normal(0, 1), class = sd, dpar = metac2one1diff) +
    prior(normal(0, 1), class = sd, dpar = metac2one2diff)
)
```

    #> 
    #> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
    #> Chain 1: 
    #> Chain 1: Gradient evaluation took 7.9e-05 seconds
    #> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.79 seconds.
    #> Chain 1: Adjust your expectations accordingly!
    #> Chain 1: 
    #> Chain 1: 
    #> Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
    #> Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
    #> Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
    #> Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
    #> Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
    #> Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
    #> Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
    #> Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
    #> Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
    #> Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
    #> Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
    #> Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
    #> Chain 1: 
    #> Chain 1:  Elapsed Time: 0.459 seconds (Warm-up)
    #> Chain 1:                0.253 seconds (Sampling)
    #> Chain 1:                0.712 seconds (Total)
    #> Chain 1: 
    #> 
    #> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
    #> Chain 2: 
    #> Chain 2: Gradient evaluation took 2.9e-05 seconds
    #> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.29 seconds.
    #> Chain 2: Adjust your expectations accordingly!
    #> Chain 2: 
    #> Chain 2: 
    #> Chain 2: Iteration:   1 / 1000 [  0%]  (Warmup)
    #> Chain 2: Iteration: 100 / 1000 [ 10%]  (Warmup)
    #> Chain 2: Iteration: 200 / 1000 [ 20%]  (Warmup)
    #> Chain 2: Iteration: 300 / 1000 [ 30%]  (Warmup)
    #> Chain 2: Iteration: 400 / 1000 [ 40%]  (Warmup)
    #> Chain 2: Iteration: 500 / 1000 [ 50%]  (Warmup)
    #> Chain 2: Iteration: 501 / 1000 [ 50%]  (Sampling)
    #> Chain 2: Iteration: 600 / 1000 [ 60%]  (Sampling)
    #> Chain 2: Iteration: 700 / 1000 [ 70%]  (Sampling)
    #> Chain 2: Iteration: 800 / 1000 [ 80%]  (Sampling)
    #> Chain 2: Iteration: 900 / 1000 [ 90%]  (Sampling)
    #> Chain 2: Iteration: 1000 / 1000 [100%]  (Sampling)
    #> Chain 2: 
    #> Chain 2:  Elapsed Time: 0.437 seconds (Warm-up)
    #> Chain 2:                0.26 seconds (Sampling)
    #> Chain 2:                0.697 seconds (Total)
    #> Chain 2: 
    #> 
    #> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
    #> Chain 3: 
    #> Chain 3: Gradient evaluation took 3e-05 seconds
    #> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.3 seconds.
    #> Chain 3: Adjust your expectations accordingly!
    #> Chain 3: 
    #> Chain 3: 
    #> Chain 3: Iteration:   1 / 1000 [  0%]  (Warmup)
    #> Chain 3: Iteration: 100 / 1000 [ 10%]  (Warmup)
    #> Chain 3: Iteration: 200 / 1000 [ 20%]  (Warmup)
    #> Chain 3: Iteration: 300 / 1000 [ 30%]  (Warmup)
    #> Chain 3: Iteration: 400 / 1000 [ 40%]  (Warmup)
    #> Chain 3: Iteration: 500 / 1000 [ 50%]  (Warmup)
    #> Chain 3: Iteration: 501 / 1000 [ 50%]  (Sampling)
    #> Chain 3: Iteration: 600 / 1000 [ 60%]  (Sampling)
    #> Chain 3: Iteration: 700 / 1000 [ 70%]  (Sampling)
    #> Chain 3: Iteration: 800 / 1000 [ 80%]  (Sampling)
    #> Chain 3: Iteration: 900 / 1000 [ 90%]  (Sampling)
    #> Chain 3: Iteration: 1000 / 1000 [100%]  (Sampling)
    #> Chain 3: 
    #> Chain 3:  Elapsed Time: 0.476 seconds (Warm-up)
    #> Chain 3:                0.255 seconds (Sampling)
    #> Chain 3:                0.731 seconds (Total)
    #> Chain 3: 
    #> 
    #> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
    #> Chain 4: 
    #> Chain 4: Gradient evaluation took 3e-05 seconds
    #> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.3 seconds.
    #> Chain 4: Adjust your expectations accordingly!
    #> Chain 4: 
    #> Chain 4: 
    #> Chain 4: Iteration:   1 / 1000 [  0%]  (Warmup)
    #> Chain 4: Iteration: 100 / 1000 [ 10%]  (Warmup)
    #> Chain 4: Iteration: 200 / 1000 [ 20%]  (Warmup)
    #> Chain 4: Iteration: 300 / 1000 [ 30%]  (Warmup)
    #> Chain 4: Iteration: 400 / 1000 [ 40%]  (Warmup)
    #> Chain 4: Iteration: 500 / 1000 [ 50%]  (Warmup)
    #> Chain 4: Iteration: 501 / 1000 [ 50%]  (Sampling)
    #> Chain 4: Iteration: 600 / 1000 [ 60%]  (Sampling)
    #> Chain 4: Iteration: 700 / 1000 [ 70%]  (Sampling)
    #> Chain 4: Iteration: 800 / 1000 [ 80%]  (Sampling)
    #> Chain 4: Iteration: 900 / 1000 [ 90%]  (Sampling)
    #> Chain 4: Iteration: 1000 / 1000 [100%]  (Sampling)
    #> Chain 4: 
    #> Chain 4:  Elapsed Time: 0.476 seconds (Warm-up)
    #> Chain 4:                0.259 seconds (Sampling)
    #> Chain 4:                0.735 seconds (Total)
    #> Chain 4:
    #>  Family: metad__3__normal__absolute__multinomial 
    #>   Links: mu = log; dprime = identity; c = identity; metac2zero1diff = log; metac2zero2diff = log; metac2one1diff = log; metac2one2diff = log 
    #> Formula: N ~ 1 + (1 | participant) + (1 | item) 
    #>          dprime ~ 1 + (1 | participant) + (1 | item)
    #>          c ~ 1 + (1 | participant) + (1 | item)
    #>          metac2zero1diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2zero2diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2one1diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2one2diff ~ 1 + (1 | participant) + (1 | item)
    #>    Data: data.aggregated (Number of observations: 500) 
    #>   Draws: 4 chains, each with iter = 1000; warmup = 500; thin = 1;
    #>          total post-warmup draws = 2000
    #> 
    #> Multilevel Hyperparameters:
    #> ~item (Number of levels: 10) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.08      0.06     0.00     0.22 1.00
    #> sd(dprime_Intercept)              0.41      0.30     0.02     1.13 1.00
    #> sd(c_Intercept)                   0.60      0.45     0.03     1.69 1.00
    #> sd(metac2zero1diff_Intercept)     0.08      0.06     0.00     0.23 1.00
    #> sd(metac2zero2diff_Intercept)     0.08      0.06     0.00     0.23 1.00
    #> sd(metac2one1diff_Intercept)      0.08      0.06     0.00     0.22 1.01
    #> sd(metac2one2diff_Intercept)      0.08      0.06     0.00     0.22 1.00
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                     1674      963
    #> sd(dprime_Intercept)              1893      999
    #> sd(c_Intercept)                   1856      832
    #> sd(metac2zero1diff_Intercept)     1862     1060
    #> sd(metac2zero2diff_Intercept)     1592      698
    #> sd(metac2one1diff_Intercept)      1527      956
    #> sd(metac2one2diff_Intercept)      1921     1069
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.21      0.15     0.01     0.56 1.00
    #> sd(dprime_Intercept)              0.40      0.31     0.02     1.15 1.00
    #> sd(c_Intercept)                   0.26      0.20     0.01     0.73 1.00
    #> sd(metac2zero1diff_Intercept)     0.20      0.15     0.01     0.55 1.00
    #> sd(metac2zero2diff_Intercept)     0.20      0.15     0.01     0.57 1.00
    #> sd(metac2one1diff_Intercept)      0.20      0.15     0.01     0.55 1.00
    #> sd(metac2one2diff_Intercept)      0.20      0.15     0.01     0.57 1.00
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                     1279      634
    #> sd(dprime_Intercept)              1546      800
    #> sd(c_Intercept)                   1952     1244
    #> sd(metac2zero1diff_Intercept)     1732      951
    #> sd(metac2zero2diff_Intercept)     1043      645
    #> sd(metac2one1diff_Intercept)      2032     1049
    #> sd(metac2one2diff_Intercept)      1773      975
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                    -0.50      0.25    -0.96    -0.02 1.00     2692
    #> dprime_Intercept              1.50      0.25     1.00     2.00 1.00     2654
    #> c_Intercept                   0.00      0.25    -0.48     0.51 1.00     2560
    #> metac2zero1diff_Intercept    -1.00      0.10    -1.19    -0.80 1.00     2170
    #> metac2zero2diff_Intercept    -1.00      0.10    -1.19    -0.79 1.00     2636
    #> metac2one1diff_Intercept     -1.00      0.11    -1.22    -0.79 1.00     1769
    #> metac2one2diff_Intercept     -1.00      0.10    -1.19    -0.81 1.00     2578
    #>                           Tail_ESS
    #> Intercept                     1467
    #> dprime_Intercept              1516
    #> c_Intercept                   1387
    #> metac2zero1diff_Intercept     1293
    #> metac2zero2diff_Intercept     1626
    #> metac2one1diff_Intercept      1157
    #> metac2one2diff_Intercept      1423
    #> 
    #> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

## Data preparation

Fitting the trial-level model does not require data aggregation, however
it still requires a small amount of data preparation. To fit the model,
we will need to things:

- a column with the stimulus per trial (`0` or `1`), and
- a column containing the joint type 1/type 2 responses per trial
  (between `1` and `2*K`).

Our data already has a `stimulus` column but separate columns for the
two responses. So, we can add in a joint response column now:

``` r
d <- d |>
  mutate(joint_response = joint_response(response, confidence, K)) |>
  relocate(joint_response, .after = "stimulus")
```

    #> # A tibble: 1,000 × 17
    #>    participant  item trial stimulus joint_response response correct confidence
    #>          <int> <int> <int>    <int>          <dbl>    <int>   <int>      <int>
    #>  1           1     1     1        0              1        0       1          3
    #>  2           1     1     1        1              1        0       0          3
    #>  3           1     2     1        0              3        0       1          1
    #>  4           1     2     1        1              6        1       1          3
    #>  5           1     3     1        0              2        0       1          2
    #>  6           1     3     1        1              6        1       1          3
    #>  7           1     4     1        0              2        0       1          2
    #>  8           1     4     1        1              6        1       1          3
    #>  9           1     5     1        0              1        0       1          3
    #> 10           1     5     1        1              6        1       1          3
    #> # ℹ 990 more rows
    #> # ℹ 9 more variables: dprime <dbl>, c <dbl>, meta_dprime <dbl>, M <dbl>,
    #> #   meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>,
    #> #   theta_2 <dbl>

## Model fitting

Now that we have our data, we can fit the trial-level model using
`joint_response` as our response variable, `stimulus` as an extra
variable passed to `brms`, and the argument `categorical=TRUE` to tell
`fit_metad` not to aggregate the data:

``` r
m.categorical <- fit_metad(
  bf(
    joint_response | vint(stimulus) ~ 1 + (1 | participant) + (1 | item),
    dprime + c +
      metac2zero1diff + metac2zero2diff +
      metac2one1diff + metac2one2diff ~
      1 + (1 | participant) + (1 | item)
  ),
  data = d, categorical = TRUE, init = 0,
  file = "models/categorical.rds",
  prior = prior(normal(0, .25), class = Intercept) +
    prior(normal(0, .25), class = Intercept, dpar = dprime) +
    prior(normal(0, .25), class = Intercept, dpar = c) +
    prior(normal(0, .1), class = Intercept, dpar = metac2zero1diff) +
    prior(normal(0, .1), class = Intercept, dpar = metac2zero2diff) +
    prior(normal(0, .1), class = Intercept, dpar = metac2one1diff) +
    prior(normal(0, .1), class = Intercept, dpar = metac2one2diff) +
    prior(normal(0, 1), class = sd) +
    prior(normal(0, 1), class = sd, dpar = dprime) +
    prior(normal(0, 1), class = sd, dpar = c) +
    prior(normal(0, 1), class = sd, dpar = metac2zero1diff) +
    prior(normal(0, 1), class = sd, dpar = metac2zero2diff) +
    prior(normal(0, 1), class = sd, dpar = metac2one1diff) +
    prior(normal(0, 1), class = sd, dpar = metac2one2diff)
)
```

    #> 
    #> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
    #> Chain 1: 
    #> Chain 1: Gradient evaluation took 4.1e-05 seconds
    #> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.41 seconds.
    #> Chain 1: Adjust your expectations accordingly!
    #> Chain 1: 
    #> Chain 1: 
    #> Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
    #> Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
    #> Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
    #> Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
    #> Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
    #> Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
    #> Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
    #> Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
    #> Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
    #> Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
    #> Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
    #> Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
    #> Chain 1: 
    #> Chain 1:  Elapsed Time: 0.469 seconds (Warm-up)
    #> Chain 1:                0.325 seconds (Sampling)
    #> Chain 1:                0.794 seconds (Total)
    #> Chain 1: 
    #> 
    #> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
    #> Chain 2: 
    #> Chain 2: Gradient evaluation took 3e-05 seconds
    #> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.3 seconds.
    #> Chain 2: Adjust your expectations accordingly!
    #> Chain 2: 
    #> Chain 2: 
    #> Chain 2: Iteration:   1 / 1000 [  0%]  (Warmup)
    #> Chain 2: Iteration: 100 / 1000 [ 10%]  (Warmup)
    #> Chain 2: Iteration: 200 / 1000 [ 20%]  (Warmup)
    #> Chain 2: Iteration: 300 / 1000 [ 30%]  (Warmup)
    #> Chain 2: Iteration: 400 / 1000 [ 40%]  (Warmup)
    #> Chain 2: Iteration: 500 / 1000 [ 50%]  (Warmup)
    #> Chain 2: Iteration: 501 / 1000 [ 50%]  (Sampling)
    #> Chain 2: Iteration: 600 / 1000 [ 60%]  (Sampling)
    #> Chain 2: Iteration: 700 / 1000 [ 70%]  (Sampling)
    #> Chain 2: Iteration: 800 / 1000 [ 80%]  (Sampling)
    #> Chain 2: Iteration: 900 / 1000 [ 90%]  (Sampling)
    #> Chain 2: Iteration: 1000 / 1000 [100%]  (Sampling)
    #> Chain 2: 
    #> Chain 2:  Elapsed Time: 0.462 seconds (Warm-up)
    #> Chain 2:                0.269 seconds (Sampling)
    #> Chain 2:                0.731 seconds (Total)
    #> Chain 2: 
    #> 
    #> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
    #> Chain 3: 
    #> Chain 3: Gradient evaluation took 2.9e-05 seconds
    #> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.29 seconds.
    #> Chain 3: Adjust your expectations accordingly!
    #> Chain 3: 
    #> Chain 3: 
    #> Chain 3: Iteration:   1 / 1000 [  0%]  (Warmup)
    #> Chain 3: Iteration: 100 / 1000 [ 10%]  (Warmup)
    #> Chain 3: Iteration: 200 / 1000 [ 20%]  (Warmup)
    #> Chain 3: Iteration: 300 / 1000 [ 30%]  (Warmup)
    #> Chain 3: Iteration: 400 / 1000 [ 40%]  (Warmup)
    #> Chain 3: Iteration: 500 / 1000 [ 50%]  (Warmup)
    #> Chain 3: Iteration: 501 / 1000 [ 50%]  (Sampling)
    #> Chain 3: Iteration: 600 / 1000 [ 60%]  (Sampling)
    #> Chain 3: Iteration: 700 / 1000 [ 70%]  (Sampling)
    #> Chain 3: Iteration: 800 / 1000 [ 80%]  (Sampling)
    #> Chain 3: Iteration: 900 / 1000 [ 90%]  (Sampling)
    #> Chain 3: Iteration: 1000 / 1000 [100%]  (Sampling)
    #> Chain 3: 
    #> Chain 3:  Elapsed Time: 0.462 seconds (Warm-up)
    #> Chain 3:                0.265 seconds (Sampling)
    #> Chain 3:                0.727 seconds (Total)
    #> Chain 3: 
    #> 
    #> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
    #> Chain 4: 
    #> Chain 4: Gradient evaluation took 3e-05 seconds
    #> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.3 seconds.
    #> Chain 4: Adjust your expectations accordingly!
    #> Chain 4: 
    #> Chain 4: 
    #> Chain 4: Iteration:   1 / 1000 [  0%]  (Warmup)
    #> Chain 4: Iteration: 100 / 1000 [ 10%]  (Warmup)
    #> Chain 4: Iteration: 200 / 1000 [ 20%]  (Warmup)
    #> Chain 4: Iteration: 300 / 1000 [ 30%]  (Warmup)
    #> Chain 4: Iteration: 400 / 1000 [ 40%]  (Warmup)
    #> Chain 4: Iteration: 500 / 1000 [ 50%]  (Warmup)
    #> Chain 4: Iteration: 501 / 1000 [ 50%]  (Sampling)
    #> Chain 4: Iteration: 600 / 1000 [ 60%]  (Sampling)
    #> Chain 4: Iteration: 700 / 1000 [ 70%]  (Sampling)
    #> Chain 4: Iteration: 800 / 1000 [ 80%]  (Sampling)
    #> Chain 4: Iteration: 900 / 1000 [ 90%]  (Sampling)
    #> Chain 4: Iteration: 1000 / 1000 [100%]  (Sampling)
    #> Chain 4: 
    #> Chain 4:  Elapsed Time: 0.46 seconds (Warm-up)
    #> Chain 4:                0.273 seconds (Sampling)
    #> Chain 4:                0.733 seconds (Total)
    #> Chain 4:
    #>  Family: metad__3__normal__absolute__categorical 
    #>   Links: mu = log; dprime = identity; c = identity; metac2zero1diff = log; metac2zero2diff = log; metac2one1diff = log; metac2one2diff = log 
    #> Formula: joint_response | vint(stimulus) ~ 1 + (1 | participant) + (1 | item) 
    #>          dprime ~ 1 + (1 | participant) + (1 | item)
    #>          c ~ 1 + (1 | participant) + (1 | item)
    #>          metac2zero1diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2zero2diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2one1diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2one2diff ~ 1 + (1 | participant) + (1 | item)
    #>    Data: data.aggregated (Number of observations: 1000) 
    #>   Draws: 4 chains, each with iter = 1000; warmup = 500; thin = 1;
    #>          total post-warmup draws = 2000
    #> 
    #> Multilevel Hyperparameters:
    #> ~item (Number of levels: 10) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.08      0.06     0.00     0.23 1.00
    #> sd(dprime_Intercept)              0.41      0.30     0.03     1.15 1.00
    #> sd(c_Intercept)                   0.60      0.43     0.04     1.61 1.00
    #> sd(metac2zero1diff_Intercept)     0.08      0.06     0.00     0.22 1.00
    #> sd(metac2zero2diff_Intercept)     0.08      0.06     0.00     0.22 1.00
    #> sd(metac2one1diff_Intercept)      0.08      0.06     0.00     0.22 1.00
    #> sd(metac2one2diff_Intercept)      0.08      0.06     0.00     0.22 1.00
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                     1427      896
    #> sd(dprime_Intercept)              1980     1313
    #> sd(c_Intercept)                   2456     1404
    #> sd(metac2zero1diff_Intercept)     1093      547
    #> sd(metac2zero2diff_Intercept)     1213      762
    #> sd(metac2one1diff_Intercept)      1359      856
    #> sd(metac2one2diff_Intercept)      1073      662
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.19      0.15     0.01     0.56 1.00
    #> sd(dprime_Intercept)              0.40      0.32     0.02     1.16 1.00
    #> sd(c_Intercept)                   0.26      0.20     0.01     0.75 1.00
    #> sd(metac2zero1diff_Intercept)     0.20      0.15     0.01     0.58 1.00
    #> sd(metac2zero2diff_Intercept)     0.20      0.15     0.01     0.55 1.00
    #> sd(metac2one1diff_Intercept)      0.20      0.15     0.01     0.55 1.00
    #> sd(metac2one2diff_Intercept)      0.20      0.14     0.01     0.54 1.01
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                     1099      469
    #> sd(dprime_Intercept)              1876     1225
    #> sd(c_Intercept)                   1561      846
    #> sd(metac2zero1diff_Intercept)     1396      758
    #> sd(metac2zero2diff_Intercept)     1487      725
    #> sd(metac2one1diff_Intercept)      1826      960
    #> sd(metac2one2diff_Intercept)      1581      958
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                    -0.50      0.26    -1.01    -0.00 1.00     2138
    #> dprime_Intercept              1.50      0.25     1.01     1.98 1.00     2541
    #> c_Intercept                   0.00      0.25    -0.51     0.49 1.01     2386
    #> metac2zero1diff_Intercept    -1.00      0.10    -1.20    -0.81 1.00     2786
    #> metac2zero2diff_Intercept    -1.00      0.10    -1.19    -0.80 1.00     2676
    #> metac2one1diff_Intercept     -1.00      0.10    -1.18    -0.81 1.00     2539
    #> metac2one2diff_Intercept     -1.00      0.10    -1.21    -0.80 1.00     2308
    #>                           Tail_ESS
    #> Intercept                     1138
    #> dprime_Intercept              1419
    #> c_Intercept                   1333
    #> metac2zero1diff_Intercept     1412
    #> metac2zero2diff_Intercept     1347
    #> metac2one1diff_Intercept      1369
    #> metac2one2diff_Intercept      1347
    #> 
    #> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

As you can see, aside from the way that the data is formatted, this
model is exactly the same as the multinomial model above.

## Extracting model estimates

Obtaining posterior estimates over model parameters, predictions, and
other estimates is very similar to the multinomial model (for details,
see
[`vignette("hmetad")`](https://metacoglab.github.io/hmetad/articles/hmetad.md)).
So, here we will focus on the type 1 ROC curves, this time using
`roc1_rvars` instead of `roc1_draws` for increased efficiency.

To get the posterior estimates, we need to specify a data set to make
predictions for, as well as a random effects formula to use in model
predictions. For example, to estimate a ROC averaging over participants
and items, we can use an empty data set with `re_formula=NA`:

``` r
roc1_rvars(m.multinomial, tibble(.row = 1), re_formula = NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence           p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.552 ± 0.104  0.91 ± 0.047
#> 2     1              2        0          2  0.391 ± 0.100  0.85 ± 0.067
#> 3     1              3        0          1  0.235 ± 0.084  0.76 ± 0.085
#> 4     1              4        1          1  0.152 ± 0.065  0.61 ± 0.101
#> 5     1              5        1          2  0.089 ± 0.046  0.45 ± 0.104
```

The process is exactly the same for the categorical model:

``` r
roc1_rvars(m.categorical, tibble(.row = 1), re_formula = NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence           p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.551 ± 0.105  0.91 ± 0.048
#> 2     1              2        0          2  0.391 ± 0.101  0.85 ± 0.068
#> 3     1              3        0          1  0.235 ± 0.085  0.76 ± 0.086
#> 4     1              4        1          1  0.151 ± 0.066  0.61 ± 0.102
#> 5     1              5        1          2  0.089 ± 0.047  0.45 ± 0.106
```

Next, to get participant-level ROCs (averaging over items), we can use a
data set with one row per participant and only the participant-level
random effects:

``` r
roc1_rvars(m.categorical, distinct(d, participant), re_formula = ~ (1 | participant))
#> # A tibble: 250 × 7
#> # Groups:   .row, participant, joint_response, response, confidence [250]
#>     .row participant joint_response response confidence         p_fa
#>    <int>       <int>          <int>    <int>      <dbl>   <rvar[1d]>
#>  1     1           1              1        0          3  0.55 ± 0.18
#>  2     2           2              1        0          3  0.55 ± 0.17
#>  3     3           3              1        0          3  0.56 ± 0.16
#>  4     4           4              1        0          3  0.56 ± 0.17
#>  5     5           5              1        0          3  0.56 ± 0.18
#>  6     6           6              1        0          3  0.55 ± 0.17
#>  7     7           7              1        0          3  0.56 ± 0.17
#>  8     8           8              1        0          3  0.55 ± 0.17
#>  9     9           9              1        0          3  0.55 ± 0.17
#> 10    10          10              1        0          3  0.55 ± 0.17
#> # ℹ 240 more rows
#> # ℹ 1 more variable: p_hit <rvar[1d]>
```

We can use a similar process to get item-level ROCs (averaging over
participants):

``` r
roc1_rvars(m.categorical, distinct(d, item), re_formula = ~ (1 | item))
#> # A tibble: 50 × 7
#> # Groups:   .row, item, joint_response, response, confidence [50]
#>     .row  item joint_response response confidence         p_fa        p_hit
#>    <int> <int>          <int>    <int>      <dbl>   <rvar[1d]>   <rvar[1d]>
#>  1     1     1              1        0          3  0.56 ± 0.23  0.87 ± 0.16
#>  2     2     2              1        0          3  0.55 ± 0.22  0.87 ± 0.15
#>  3     3     3              1        0          3  0.54 ± 0.23  0.86 ± 0.17
#>  4     4     4              1        0          3  0.54 ± 0.23  0.86 ± 0.17
#>  5     5     5              1        0          3  0.55 ± 0.23  0.87 ± 0.17
#>  6     6     6              1        0          3  0.55 ± 0.22  0.87 ± 0.16
#>  7     7     7              1        0          3  0.54 ± 0.23  0.87 ± 0.16
#>  8     8     8              1        0          3  0.54 ± 0.23  0.86 ± 0.17
#>  9     9     9              1        0          3  0.54 ± 0.24  0.85 ± 0.18
#> 10    10    10              1        0          3  0.55 ± 0.23  0.86 ± 0.16
#> # ℹ 40 more rows
```

## Other benefits

Aside from representing the data in a more convenient format, the
trial-level model should be more useful for things like model comparison
using the `loo` package, multivariate models, and mediation models.
These features should mostly work out the box but they are still under
active development, so stay tuned!

------------------------------------------------------------------------

1.  Note that to keep vignette run time short and to reduce model
    convergence errors, the models in this vignette are actually fit
    using `sample_prior="only"`, which ignores the data. In practice,
    fitting hierarchical models will usually require setting informed
    priors and adjusting the Stan sampler settings.
