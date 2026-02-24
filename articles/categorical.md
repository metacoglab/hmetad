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
  item = 1:25
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

    #> # A tibble: 2,500 × 16
    #>    participant  item trial stimulus response correct
    #>          <int> <int> <int>    <int>    <int>   <int>
    #>  1           1     1     1        0        1       0
    #>  2           1     1     1        1        1       1
    #>  3           1     2     1        0        0       1
    #>  4           1     2     1        1        1       1
    #>  5           1     3     1        0        0       1
    #>  6           1     3     1        1        0       0
    #>  7           1     4     1        0        1       0
    #>  8           1     4     1        1        1       1
    #>  9           1     5     1        0        0       1
    #> 10           1     5     1        1        0       0
    #> # ℹ 2,490 more rows
    #> # ℹ 10 more variables: confidence <int>, dprime <dbl>,
    #> #   c <dbl>, meta_dprime <dbl>, M <dbl>, meta_c2_0 <list>,
    #> #   meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>,
    #> #   theta_2 <dbl>

Don’t worry about the details of the simulation code- what matters is
that we have a data set with repeated measures for participants:

``` r
count(d, participant)
#> # A tibble: 50 × 2
#>    participant     n
#>          <int> <int>
#>  1           1    50
#>  2           2    50
#>  3           3    50
#>  4           4    50
#>  5           5    50
#>  6           6    50
#>  7           7    50
#>  8           8    50
#>  9           9    50
#> 10          10    50
#> # ℹ 40 more rows
```

And repeated measures for items:

``` r
count(d, item)
#> # A tibble: 25 × 2
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
#> # ℹ 15 more rows
```

## Standard model with data aggregation

If we would like, we can use the `fit_metad` function on this data with
participant-level and item-level effects. However, if we aggregate the
data ourselves, we can see that the aggregation doesn’t really help us
here:

``` r
aggregate_metad(d, participant, item)
#> # A tibble: 1,250 × 5
#>    participant item    N_0   N_1 N[,"N_0_1"] [,"N_0_2"]
#>    <fct>       <fct> <int> <int>       <int>      <int>
#>  1 1           1         1     1           0          0
#>  2 1           2         1     1           0          0
#>  3 1           3         1     1           0          1
#>  4 1           4         1     1           0          0
#>  5 1           5         1     1           1          0
#>  6 1           6         1     1           0          0
#>  7 1           7         1     1           0          0
#>  8 1           8         1     1           1          0
#>  9 1           9         1     1           0          0
#> 10 1           10        1     1           0          0
#> # ℹ 1,240 more rows
#> # ℹ 1 more variable: N[3:12] <int>
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
  data = d, init = 0, cores = 4,
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

    #>  Family: metad__3__normal__absolute__multinomial 
    #>   Links: mu = log; dprime = identity; c = identity; metac2zero1diff = log; metac2zero2diff = log; metac2one1diff = log; metac2one2diff = log 
    #> Formula: N ~ 1 + (1 | participant) + (1 | item) 
    #>          dprime ~ 1 + (1 | participant) + (1 | item)
    #>          c ~ 1 + (1 | participant) + (1 | item)
    #>          metac2zero1diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2zero2diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2one1diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2one2diff ~ 1 + (1 | participant) + (1 | item)
    #>    Data: data.aggregated (Number of observations: 1250) 
    #>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    #>          total post-warmup draws = 4000
    #> 
    #> Multilevel Hyperparameters:
    #> ~item (Number of levels: 25) 
    #>                               Estimate Est.Error l-95% CI
    #> sd(Intercept)                     0.22      0.15     0.01
    #> sd(dprime_Intercept)              0.70      0.15     0.46
    #> sd(c_Intercept)                   0.58      0.10     0.42
    #> sd(metac2zero1diff_Intercept)     0.46      0.22     0.06
    #> sd(metac2zero2diff_Intercept)     0.88      0.20     0.52
    #> sd(metac2one1diff_Intercept)      0.54      0.21     0.16
    #> sd(metac2one2diff_Intercept)      0.63      0.20     0.26
    #>                               u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.54 1.00      853     1474
    #> sd(dprime_Intercept)              1.05 1.00     1137     2025
    #> sd(c_Intercept)                   0.80 1.00     1195     1741
    #> sd(metac2zero1diff_Intercept)     0.92 1.01      354      627
    #> sd(metac2zero2diff_Intercept)     1.31 1.00     1239     1975
    #> sd(metac2one1diff_Intercept)      0.96 1.01      400      620
    #> sd(metac2one2diff_Intercept)      1.05 1.00      661      994
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI
    #> sd(Intercept)                     0.21      0.12     0.01
    #> sd(dprime_Intercept)              0.40      0.08     0.25
    #> sd(c_Intercept)                   0.38      0.05     0.29
    #> sd(metac2zero1diff_Intercept)     0.37      0.12     0.18
    #> sd(metac2zero2diff_Intercept)     0.20      0.11     0.01
    #> sd(metac2one1diff_Intercept)      0.29      0.11     0.05
    #> sd(metac2one2diff_Intercept)      0.25      0.11     0.04
    #>                               u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.45 1.00      933     1381
    #> sd(dprime_Intercept)              0.57 1.00     1558     2049
    #> sd(c_Intercept)                   0.49 1.00     1464     2184
    #> sd(metac2zero1diff_Intercept)     0.64 1.01      558      838
    #> sd(metac2zero2diff_Intercept)     0.42 1.00      855     1096
    #> sd(metac2one1diff_Intercept)      0.51 1.01      631      625
    #> sd(metac2one2diff_Intercept)      0.48 1.00      881     1370
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI
    #> Intercept                    -0.18      0.10    -0.40
    #> dprime_Intercept              0.98      0.16     0.64
    #> c_Intercept                   0.14      0.12    -0.09
    #> metac2zero1diff_Intercept    -0.41      0.13    -0.65
    #> metac2zero2diff_Intercept    -0.27      0.11    -0.50
    #> metac2one1diff_Intercept     -0.37      0.13    -0.61
    #> metac2one2diff_Intercept     -0.34      0.13    -0.59
    #>                           u-95% CI Rhat Bulk_ESS Tail_ESS
    #> Intercept                     0.01 1.00     3683     2741
    #> dprime_Intercept              1.27 1.00     1279     1613
    #> c_Intercept                   0.36 1.00     1103     1758
    #> metac2zero1diff_Intercept    -0.14 1.01      583     1513
    #> metac2zero2diff_Intercept    -0.05 1.00     2018     2044
    #> metac2one1diff_Intercept     -0.11 1.01      581     1083
    #> metac2one2diff_Intercept     -0.09 1.00      930     1458
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

    #> # A tibble: 2,500 × 17
    #>    participant  item trial stimulus joint_response response
    #>          <int> <int> <int>    <int>          <dbl>    <int>
    #>  1           1     1     1        0              4        1
    #>  2           1     1     1        1              4        1
    #>  3           1     2     1        0              3        0
    #>  4           1     2     1        1              5        1
    #>  5           1     3     1        0              2        0
    #>  6           1     3     1        1              2        0
    #>  7           1     4     1        0              6        1
    #>  8           1     4     1        1              5        1
    #>  9           1     5     1        0              1        0
    #> 10           1     5     1        1              1        0
    #> # ℹ 2,490 more rows
    #> # ℹ 11 more variables: correct <int>, confidence <int>,
    #> #   dprime <dbl>, c <dbl>, meta_dprime <dbl>, M <dbl>,
    #> #   meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>,
    #> #   theta_1 <dbl>, theta_2 <dbl>

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
  data = d, categorical = TRUE, init = 0, cores = 4,
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

    #>  Family: metad__3__normal__absolute__categorical 
    #>   Links: mu = log; dprime = identity; c = identity; metac2zero1diff = log; metac2zero2diff = log; metac2one1diff = log; metac2one2diff = log 
    #> Formula: joint_response | vint(stimulus) ~ 1 + (1 | participant) + (1 | item) 
    #>          dprime ~ 1 + (1 | participant) + (1 | item)
    #>          c ~ 1 + (1 | participant) + (1 | item)
    #>          metac2zero1diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2zero2diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2one1diff ~ 1 + (1 | participant) + (1 | item)
    #>          metac2one2diff ~ 1 + (1 | participant) + (1 | item)
    #>    Data: data.aggregated (Number of observations: 2500) 
    #>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    #>          total post-warmup draws = 4000
    #> 
    #> Multilevel Hyperparameters:
    #> ~item (Number of levels: 25) 
    #>                               Estimate Est.Error l-95% CI
    #> sd(Intercept)                     0.11      0.09     0.01
    #> sd(dprime_Intercept)              0.72      0.18     0.45
    #> sd(c_Intercept)                   0.72      0.12     0.53
    #> sd(metac2zero1diff_Intercept)     1.05      0.21     0.70
    #> sd(metac2zero2diff_Intercept)     0.78      0.21     0.41
    #> sd(metac2one1diff_Intercept)      0.76      0.22     0.35
    #> sd(metac2one2diff_Intercept)      0.19      0.18     0.00
    #>                               u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.32 1.00     1209     1883
    #> sd(dprime_Intercept)              1.15 1.01      690     1372
    #> sd(c_Intercept)                   0.98 1.00      646     1399
    #> sd(metac2zero1diff_Intercept)     1.52 1.00      796     1562
    #> sd(metac2zero2diff_Intercept)     1.21 1.01      479      757
    #> sd(metac2one1diff_Intercept)      1.22 1.00      487      430
    #> sd(metac2one2diff_Intercept)      0.70 1.01      238      391
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI
    #> sd(Intercept)                     0.23      0.12     0.02
    #> sd(dprime_Intercept)              0.58      0.10     0.41
    #> sd(c_Intercept)                   0.30      0.04     0.22
    #> sd(metac2zero1diff_Intercept)     0.19      0.10     0.01
    #> sd(metac2zero2diff_Intercept)     0.29      0.10     0.09
    #> sd(metac2one1diff_Intercept)      0.39      0.11     0.18
    #> sd(metac2one2diff_Intercept)      0.60      0.16     0.33
    #>                               u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.46 1.01      608      806
    #> sd(dprime_Intercept)              0.80 1.00     1434     2025
    #> sd(c_Intercept)                   0.40 1.00     1430     2160
    #> sd(metac2zero1diff_Intercept)     0.39 1.00      690      903
    #> sd(metac2zero2diff_Intercept)     0.48 1.01      481      420
    #> sd(metac2one1diff_Intercept)      0.64 1.00      623      697
    #> sd(metac2one2diff_Intercept)      0.95 1.01      353     1035
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI
    #> Intercept                    -0.30      0.09    -0.49
    #> dprime_Intercept              1.11      0.18     0.70
    #> c_Intercept                   0.11      0.14    -0.15
    #> metac2zero1diff_Intercept    -0.21      0.11    -0.42
    #> metac2zero2diff_Intercept    -0.26      0.12    -0.51
    #> metac2one1diff_Intercept     -0.28      0.12    -0.52
    #> metac2one2diff_Intercept     -0.44      0.10    -0.63
    #>                           u-95% CI Rhat Bulk_ESS Tail_ESS
    #> Intercept                    -0.13 1.00     2236     2272
    #> dprime_Intercept              1.43 1.01      747     1279
    #> c_Intercept                   0.37 1.01      473      897
    #> metac2zero1diff_Intercept    -0.01 1.00     1908     2442
    #> metac2zero2diff_Intercept    -0.03 1.01      786      933
    #> metac2one1diff_Intercept     -0.05 1.00      798      966
    #> metac2one2diff_Intercept     -0.21 1.00      981     1239
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
#>    .row joint_response response confidence           p_fa
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>
#> 1     1              1        0          3  0.802 ± 0.051
#> 2     1              2        0          2  0.531 ± 0.066
#> 3     1              3        0          1  0.267 ± 0.045
#> 4     1              4        1          1  0.100 ± 0.029
#> 5     1              5        1          2  0.025 ± 0.011
#> # ℹ 1 more variable: p_hit <rvar[1d]>
```

The process is exactly the same for the categorical model:

``` r
roc1_rvars(m.categorical, tibble(.row = 1), re_formula = NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence           p_fa
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>
#> 1     1              1        0          3  0.842 ± 0.048
#> 2     1              2        0          2  0.587 ± 0.069
#> 3     1              3        0          1  0.256 ± 0.053
#> 4     1              4        1          1  0.088 ± 0.030
#> 5     1              5        1          2  0.025 ± 0.012
#> # ℹ 1 more variable: p_hit <rvar[1d]>
```

Next, to get participant-level ROCs (averaging over items), we can use a
data set with one row per participant and only the participant-level
random effects:

``` r
roc1_rvars(m.categorical, distinct(d, participant), re_formula = ~ (1 | participant))
#> # A tibble: 250 × 7
#> # Groups:   .row, participant, joint_response, response,
#> #   confidence [250]
#>     .row participant joint_response response confidence
#>    <int>       <int>          <int>    <int>      <dbl>
#>  1     1           1              1        0          3
#>  2     2           2              1        0          3
#>  3     3           3              1        0          3
#>  4     4           4              1        0          3
#>  5     5           5              1        0          3
#>  6     6           6              1        0          3
#>  7     7           7              1        0          3
#>  8     8           8              1        0          3
#>  9     9           9              1        0          3
#> 10    10          10              1        0          3
#> # ℹ 240 more rows
#> # ℹ 2 more variables: p_fa <rvar[1d]>, p_hit <rvar[1d]>
```

We can use a similar process to get item-level ROCs (averaging over
participants):

``` r
roc1_rvars(m.categorical, distinct(d, item), re_formula = ~ (1 | item))
#> # A tibble: 125 × 7
#> # Groups:   .row, item, joint_response, response, confidence
#> #   [125]
#>     .row  item joint_response response confidence
#>    <int> <int>          <int>    <int>      <dbl>
#>  1     1     1              1        0          3
#>  2     2     2              1        0          3
#>  3     3     3              1        0          3
#>  4     4     4              1        0          3
#>  5     5     5              1        0          3
#>  6     6     6              1        0          3
#>  7     7     7              1        0          3
#>  8     8     8              1        0          3
#>  9     9     9              1        0          3
#> 10    10    10              1        0          3
#> # ℹ 115 more rows
#> # ℹ 2 more variables: p_fa <rvar[1d]>, p_hit <rvar[1d]>
```

## Other benefits

Aside from representing the data in a more convenient format, the
trial-level model should be more useful for things like model comparison
using the `loo` package, multivariate models, and mediation models.
These features should mostly work out of the box but they are still
under active development, so stay tuned!

------------------------------------------------------------------------

1.  Note that in practice, fitting hierarchical models will usually
    require setting informed priors and adjusting the Stan sampler
    settings.
