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
    #>    participant  item trial stimulus response correct confidence dprime
    #>          <int> <int> <int>    <int>    <int>   <int>      <int>  <dbl>
    #>  1           1     1     1        0        0       1          3   1.67
    #>  2           1     1     1        1        1       1          2   1.67
    #>  3           1     2     1        0        0       1          1   1.78
    #>  4           1     2     1        1        1       1          3   1.78
    #>  5           1     3     1        0        0       1          3   1.21
    #>  6           1     3     1        1        0       0          2   1.21
    #>  7           1     4     1        0        0       1          3   1.27
    #>  8           1     4     1        1        1       1          3   1.27
    #>  9           1     5     1        0        0       1          2   1.90
    #> 10           1     5     1        1        0       0          1   1.90
    #> # ℹ 2,490 more rows
    #> # ℹ 8 more variables: c <dbl>, meta_dprime <dbl>, M <dbl>,
    #> #   meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>,
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
#>    participant item    N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"]
#>    <fct>       <fct> <int> <int>       <int>      <int>      <int>
#>  1 1           1         1     1           1          0          0
#>  2 1           2         1     1           0          0          1
#>  3 1           3         1     1           1          0          0
#>  4 1           4         1     1           1          0          0
#>  5 1           5         1     1           0          1          0
#>  6 1           6         1     1           0          0          0
#>  7 1           7         1     1           1          0          0
#>  8 1           8         1     1           1          0          0
#>  9 1           9         1     1           0          0          1
#> 10 1           10        1     1           1          0          0
#> # ℹ 1,240 more rows
#> # ℹ 1 more variable: N[4:12] <int>
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
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.14      0.11     0.01     0.40 1.00
    #> sd(dprime_Intercept)              0.52      0.13     0.32     0.83 1.00
    #> sd(c_Intercept)                   0.64      0.10     0.47     0.88 1.00
    #> sd(metac2zero1diff_Intercept)     0.80      0.24     0.12     1.24 1.03
    #> sd(metac2zero2diff_Intercept)     0.72      0.23     0.23     1.17 1.01
    #> sd(metac2one1diff_Intercept)      0.58      0.26     0.05     1.06 1.02
    #> sd(metac2one2diff_Intercept)      0.79      0.19     0.46     1.21 1.00
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                     1614     2237
    #> sd(dprime_Intercept)              1384     2087
    #> sd(c_Intercept)                   1257     2043
    #> sd(metac2zero1diff_Intercept)      140       27
    #> sd(metac2zero2diff_Intercept)      405      368
    #> sd(metac2one1diff_Intercept)       204      227
    #> sd(metac2one2diff_Intercept)      1059     1874
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.32      0.15     0.04     0.62 1.00
    #> sd(dprime_Intercept)              0.50      0.09     0.35     0.69 1.00
    #> sd(c_Intercept)                   0.31      0.04     0.24     0.41 1.00
    #> sd(metac2zero1diff_Intercept)     0.13      0.11     0.00     0.36 1.02
    #> sd(metac2zero2diff_Intercept)     0.34      0.12     0.14     0.60 1.00
    #> sd(metac2one1diff_Intercept)      0.35      0.15     0.10     0.70 1.01
    #> sd(metac2one2diff_Intercept)      0.17      0.10     0.01     0.39 1.01
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                      865     1116
    #> sd(dprime_Intercept)              1670     2770
    #> sd(c_Intercept)                   1486     2138
    #> sd(metac2zero1diff_Intercept)      196      110
    #> sd(metac2zero2diff_Intercept)      608      547
    #> sd(metac2one1diff_Intercept)       275      318
    #> sd(metac2one2diff_Intercept)       836     1817
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat
    #> Intercept                    -0.32      0.12    -0.55    -0.10 1.00
    #> dprime_Intercept              1.04      0.14     0.74     1.29 1.00
    #> c_Intercept                   0.26      0.12     0.02     0.49 1.01
    #> metac2zero1diff_Intercept    -0.30      0.14    -0.65    -0.06 1.02
    #> metac2zero2diff_Intercept    -0.32      0.13    -0.58    -0.07 1.01
    #> metac2one1diff_Intercept     -0.37      0.14    -0.65    -0.09 1.01
    #> metac2one2diff_Intercept     -0.27      0.11    -0.49    -0.04 1.00
    #>                           Bulk_ESS Tail_ESS
    #> Intercept                     4558     2762
    #> dprime_Intercept              1544     2244
    #> c_Intercept                    665     1556
    #> metac2zero1diff_Intercept      201       51
    #> metac2zero2diff_Intercept      759     1241
    #> metac2one1diff_Intercept       377      679
    #> metac2one2diff_Intercept      1771     1925
    #> 
    #> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    #> and Tail_ESS are effective sample size measures, and Rhat is the potential
    #> scale reduction factor on split chains (at convergence, Rhat = 1).

## Data preparation

Fitting the trial-level model does not require data aggregation, however
it still requires a small amount of data preparation. To fit the model,
we will need two things:

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
    #>    participant  item trial stimulus joint_response response correct
    #>          <int> <int> <int>    <int>          <dbl>    <int>   <int>
    #>  1           1     1     1        0              1        0       1
    #>  2           1     1     1        1              5        1       1
    #>  3           1     2     1        0              3        0       1
    #>  4           1     2     1        1              6        1       1
    #>  5           1     3     1        0              1        0       1
    #>  6           1     3     1        1              2        0       0
    #>  7           1     4     1        0              1        0       1
    #>  8           1     4     1        1              6        1       1
    #>  9           1     5     1        0              2        0       1
    #> 10           1     5     1        1              3        0       0
    #> # ℹ 2,490 more rows
    #> # ℹ 10 more variables: confidence <int>, dprime <dbl>, c <dbl>,
    #> #   meta_dprime <dbl>, M <dbl>, meta_c2_0 <list>, meta_c2_1 <list>,
    #> #   theta <dbl>, theta_1 <dbl>, theta_2 <dbl>

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
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.15      0.11     0.01     0.40 1.00
    #> sd(dprime_Intercept)              0.51      0.12     0.32     0.80 1.00
    #> sd(c_Intercept)                   0.64      0.10     0.47     0.87 1.00
    #> sd(metac2zero1diff_Intercept)     0.85      0.20     0.50     1.26 1.00
    #> sd(metac2zero2diff_Intercept)     0.70      0.24     0.14     1.16 1.00
    #> sd(metac2one1diff_Intercept)      0.58      0.27     0.05     1.06 1.01
    #> sd(metac2one2diff_Intercept)      0.79      0.19     0.46     1.20 1.00
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                     1675     2246
    #> sd(dprime_Intercept)              1244     2394
    #> sd(c_Intercept)                   1832     2722
    #> sd(metac2zero1diff_Intercept)     1372     1765
    #> sd(metac2zero2diff_Intercept)      402      239
    #> sd(metac2one1diff_Intercept)       257      379
    #> sd(metac2one2diff_Intercept)      1453     2325
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.32      0.14     0.04     0.61 1.01
    #> sd(dprime_Intercept)              0.51      0.09     0.35     0.68 1.00
    #> sd(c_Intercept)                   0.31      0.04     0.24     0.41 1.00
    #> sd(metac2zero1diff_Intercept)     0.12      0.08     0.00     0.29 1.00
    #> sd(metac2zero2diff_Intercept)     0.34      0.12     0.14     0.64 1.01
    #> sd(metac2one1diff_Intercept)      0.35      0.15     0.07     0.74 1.00
    #> sd(metac2one2diff_Intercept)      0.16      0.10     0.01     0.38 1.00
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                      777     1074
    #> sd(dprime_Intercept)              1547     2407
    #> sd(c_Intercept)                   1663     2537
    #> sd(metac2zero1diff_Intercept)     1421     2084
    #> sd(metac2zero2diff_Intercept)      496      335
    #> sd(metac2one1diff_Intercept)       349      520
    #> sd(metac2one2diff_Intercept)      1197     1815
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat
    #> Intercept                    -0.32      0.11    -0.55    -0.11 1.00
    #> dprime_Intercept              1.04      0.14     0.74     1.29 1.00
    #> c_Intercept                   0.25      0.12    -0.00     0.49 1.00
    #> metac2zero1diff_Intercept    -0.28      0.12    -0.53    -0.05 1.00
    #> metac2zero2diff_Intercept    -0.32      0.13    -0.60    -0.07 1.00
    #> metac2one1diff_Intercept     -0.37      0.14    -0.65    -0.10 1.00
    #> metac2one2diff_Intercept     -0.27      0.11    -0.50    -0.05 1.00
    #>                           Bulk_ESS Tail_ESS
    #> Intercept                     4309     3080
    #> dprime_Intercept              2057     2154
    #> c_Intercept                   1040     1931
    #> metac2zero1diff_Intercept     2206     1900
    #> metac2zero2diff_Intercept      675      604
    #> metac2one1diff_Intercept       446      944
    #> metac2one2diff_Intercept      2391     2490
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

To get the posterior estimates, we need to specify a dataset to make
predictions for, as well as a random effects formula to use in model
predictions. For example, to estimate a ROC averaging over participants
and items, we can use an empty data set with `re_formula=NA`:

``` r
roc1_rvars(m.multinomial, tibble(.row = 1), re_formula = NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence            p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>      <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.785 ± 0.0568  0.95 ± 0.020
#> 2     1              2        0          2  0.517 ± 0.0690  0.83 ± 0.043
#> 3     1              3        0          1  0.221 ± 0.0421  0.60 ± 0.052
#> 4     1              4        1          1  0.079 ± 0.0249  0.31 ± 0.060
#> 5     1              5        1          2  0.016 ± 0.0077  0.10 ± 0.034
```

The process is exactly the same for the categorical model:

``` r
roc1_rvars(m.categorical, tibble(.row = 1), re_formula = NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence            p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>      <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.790 ± 0.0553  0.95 ± 0.020
#> 2     1              2        0          2  0.526 ± 0.0664  0.83 ± 0.041
#> 3     1              3        0          1  0.224 ± 0.0433  0.60 ± 0.053
#> 4     1              4        1          1  0.080 ± 0.0261  0.31 ± 0.061
#> 5     1              5        1          2  0.017 ± 0.0082  0.10 ± 0.034
```

Next, to get participant-level ROCs (averaging over items), we can use a
dataset with one row per participant and only the participant-level
random effects:

``` r
roc1_rvars(m.categorical, distinct(d, participant), re_formula = ~ (1 | participant))
#> # A tibble: 250 × 7
#> # Groups:   .row, participant, joint_response, response, confidence [250]
#>     .row participant joint_response response confidence          p_fa
#>    <int>       <int>          <int>    <int>      <dbl>    <rvar[1d]>
#>  1     1           1              1        0          3  0.84 ± 0.075
#>  2     2           2              1        0          3  0.89 ± 0.061
#>  3     3           3              1        0          3  0.68 ± 0.106
#>  4     4           4              1        0          3  0.73 ± 0.105
#>  5     5           5              1        0          3  0.76 ± 0.096
#>  6     6           6              1        0          3  0.86 ± 0.079
#>  7     7           7              1        0          3  0.83 ± 0.082
#>  8     8           8              1        0          3  0.68 ± 0.111
#>  9     9           9              1        0          3  0.75 ± 0.099
#> 10    10          10              1        0          3  0.87 ± 0.067
#> # ℹ 240 more rows
#> # ℹ 1 more variable: p_hit <rvar[1d]>
```

We can use a similar process to get item-level ROCs (averaging over
participants):

``` r
roc1_rvars(m.categorical, distinct(d, item), re_formula = ~ (1 | item))
#> # A tibble: 125 × 7
#> # Groups:   .row, item, joint_response, response, confidence [125]
#>     .row  item joint_response response confidence          p_fa
#>    <int> <int>          <int>    <int>      <dbl>    <rvar[1d]>
#>  1     1     1              1        0          3  0.23 ± 0.057
#>  2     2     2              1        0          3  0.26 ± 0.063
#>  3     3     3              1        0          3  0.32 ± 0.065
#>  4     4     4              1        0          3  0.62 ± 0.067
#>  5     5     5              1        0          3  0.74 ± 0.062
#>  6     6     6              1        0          3  0.36 ± 0.069
#>  7     7     7              1        0          3  0.79 ± 0.057
#>  8     8     8              1        0          3  0.31 ± 0.064
#>  9     9     9              1        0          3  0.79 ± 0.057
#> 10    10    10              1        0          3  0.57 ± 0.069
#> # ℹ 115 more rows
#> # ℹ 1 more variable: p_hit <rvar[1d]>
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
