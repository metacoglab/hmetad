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
    #>    participant  item trial stimulus response correct confidence
    #>          <int> <int> <int>    <int>    <int>   <int>      <int>
    #>  1           1     1     1        0        1       0          1
    #>  2           1     1     1        1        1       1          2
    #>  3           1     2     1        0        0       1          3
    #>  4           1     2     1        1        1       1          1
    #>  5           1     3     1        0        0       1          3
    #>  6           1     3     1        1        0       0          1
    #>  7           1     4     1        0        0       1          2
    #>  8           1     4     1        1        1       1          3
    #>  9           1     5     1        0        0       1          1
    #> 10           1     5     1        1        1       1          3
    #> # ℹ 2,490 more rows
    #> # ℹ 9 more variables: dprime <dbl>, c <dbl>, meta_dprime <dbl>,
    #> #   M <dbl>, meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>,
    #> #   theta_1 <dbl>, theta_2 <dbl>

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
#>    participant  item   N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"]
#>          <int> <int> <int> <int>       <int>      <int>      <int>
#>  1           1     1     1     1           0          0          0
#>  2           1     2     1     1           1          0          0
#>  3           1     3     1     1           1          0          0
#>  4           1     4     1     1           0          1          0
#>  5           1     5     1     1           0          0          1
#>  6           1     6     1     1           1          0          0
#>  7           1     7     1     1           0          0          0
#>  8           1     8     1     1           1          0          0
#>  9           1     9     1     1           0          0          1
#> 10           1    10     1     1           1          0          0
#> # ℹ 1,240 more rows
#> # ℹ 1 more variable: N[4:12] <int>
```

As you can see, the aggregated data set has 1250 rows (with two
observations per row), which is not much smaller than the trial-level
data that we started with! So, in this case, it will probably be easier
*not* to aggregate our data. Nevertheless, there is nothing stopping us
from fitting the model like normal:[¹](#fn1)

``` r
# Priors are chosen arbitrarily for this example.
# Please choose your own wisely!
priors <- prior(normal(0, .25), class = Intercept) +
  set_prior(
    "normal(0, .25)",
    class = "Intercept",
    dpar = c("dprime", "c")
  ) +
  set_prior("normal(-0.5, .1)",
    class = "Intercept",
    dpar = c(
      "metac2zero1diff", "metac2zero2diff",
      "metac2one1diff", "metac2one2diff"
    )
  ) +
  prior(normal(0, 1), class = sd) +
  set_prior("normal(0, 1)",
    class = "sd",
    dpar = c(
      "dprime", "c",
      "metac2zero1diff", "metac2zero2diff",
      "metac2one1diff", "metac2one2diff"
    )
  )

m.multinomial <- fit_metad(
  bf(
    N ~ 1 + (1 | participant) + (1 | item),
    dprime + c +
      metac2zero1diff + metac2zero2diff +
      metac2one1diff + metac2one2diff ~
      1 + (1 | participant) + (1 | item)
  ),
  data = d, init = 0, cores = 4, prior = priors
)
#> Compiling Stan program...
#> Start sampling
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
    #> sd(Intercept)                     0.12      0.09     0.00
    #> sd(dprime_Intercept)              0.63      0.16     0.39
    #> sd(c_Intercept)                   0.71      0.11     0.53
    #> sd(metac2zero1diff_Intercept)     0.10      0.07     0.00
    #> sd(metac2zero2diff_Intercept)     0.25      0.12     0.03
    #> sd(metac2one1diff_Intercept)      0.13      0.10     0.01
    #> sd(metac2one2diff_Intercept)      0.14      0.09     0.01
    #>                               u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.34 1.00     1530     1820
    #> sd(dprime_Intercept)              1.02 1.00      765     1402
    #> sd(c_Intercept)                   0.97 1.00      854     1733
    #> sd(metac2zero1diff_Intercept)     0.27 1.00     1371     1515
    #> sd(metac2zero2diff_Intercept)     0.49 1.02      701      866
    #> sd(metac2one1diff_Intercept)      0.38 1.00      984     1708
    #> sd(metac2one2diff_Intercept)      0.34 1.00     1030     1356
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI
    #> sd(Intercept)                     0.14      0.10     0.01
    #> sd(dprime_Intercept)              0.46      0.09     0.29
    #> sd(c_Intercept)                   0.29      0.04     0.21
    #> sd(metac2zero1diff_Intercept)     0.30      0.10     0.10
    #> sd(metac2zero2diff_Intercept)     0.20      0.10     0.02
    #> sd(metac2one1diff_Intercept)      0.30      0.11     0.06
    #> sd(metac2one2diff_Intercept)      0.35      0.10     0.16
    #>                               u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.36 1.01     1221     1493
    #> sd(dprime_Intercept)              0.65 1.00     1151     1723
    #> sd(c_Intercept)                   0.38 1.00     1341     2619
    #> sd(metac2zero1diff_Intercept)     0.50 1.00      900     1052
    #> sd(metac2zero2diff_Intercept)     0.40 1.00      803      886
    #> sd(metac2one1diff_Intercept)      0.53 1.01      527      555
    #> sd(metac2one2diff_Intercept)      0.57 1.01     1060     1230
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI
    #> Intercept                    -0.30      0.09    -0.48    -0.14
    #> dprime_Intercept              1.16      0.17     0.76     1.44
    #> c_Intercept                  -0.02      0.13    -0.28     0.23
    #> metac2zero1diff_Intercept    -0.85      0.06    -0.97    -0.72
    #> metac2zero2diff_Intercept    -0.80      0.07    -0.93    -0.66
    #> metac2one1diff_Intercept     -0.91      0.07    -1.04    -0.75
    #> metac2one2diff_Intercept     -0.80      0.07    -0.93    -0.67
    #>                           Rhat Bulk_ESS Tail_ESS
    #> Intercept                 1.00     3800     3038
    #> dprime_Intercept          1.01      963     1385
    #> c_Intercept               1.01      578     1071
    #> metac2zero1diff_Intercept 1.00     2515     2824
    #> metac2zero2diff_Intercept 1.00     2177     2606
    #> metac2one1diff_Intercept  1.00     1825     2413
    #> metac2one2diff_Intercept  1.00     2306     2375
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
    #>    participant  item trial stimulus joint_response response
    #>          <int> <int> <int>    <int>          <int>    <int>
    #>  1           1     1     1        0              4        1
    #>  2           1     1     1        1              5        1
    #>  3           1     2     1        0              1        0
    #>  4           1     2     1        1              4        1
    #>  5           1     3     1        0              1        0
    #>  6           1     3     1        1              3        0
    #>  7           1     4     1        0              2        0
    #>  8           1     4     1        1              6        1
    #>  9           1     5     1        0              3        0
    #> 10           1     5     1        1              6        1
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
  data = d, categorical = TRUE, init = 0, cores = 4, prior = priors
)
#> Compiling Stan program...
#> Start sampling
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
    #> sd(Intercept)                     0.13      0.09     0.01
    #> sd(dprime_Intercept)              0.63      0.15     0.40
    #> sd(c_Intercept)                   0.71      0.11     0.53
    #> sd(metac2zero1diff_Intercept)     0.09      0.07     0.00
    #> sd(metac2zero2diff_Intercept)     0.25      0.11     0.05
    #> sd(metac2one1diff_Intercept)      0.13      0.10     0.01
    #> sd(metac2one2diff_Intercept)      0.15      0.10     0.01
    #>                               u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.35 1.00     1101     1894
    #> sd(dprime_Intercept)              0.97 1.00      848     1588
    #> sd(c_Intercept)                   0.95 1.00      782     1424
    #> sd(metac2zero1diff_Intercept)     0.27 1.00     1422     1889
    #> sd(metac2zero2diff_Intercept)     0.49 1.00      927      963
    #> sd(metac2one1diff_Intercept)      0.38 1.01      878     1201
    #> sd(metac2one2diff_Intercept)      0.37 1.00      898     1365
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI
    #> sd(Intercept)                     0.14      0.10     0.01
    #> sd(dprime_Intercept)              0.45      0.09     0.30
    #> sd(c_Intercept)                   0.29      0.04     0.21
    #> sd(metac2zero1diff_Intercept)     0.31      0.10     0.12
    #> sd(metac2zero2diff_Intercept)     0.21      0.10     0.02
    #> sd(metac2one1diff_Intercept)      0.31      0.12     0.06
    #> sd(metac2one2diff_Intercept)      0.34      0.11     0.12
    #>                               u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.36 1.00      947     1467
    #> sd(dprime_Intercept)              0.63 1.00     1649     2279
    #> sd(c_Intercept)                   0.38 1.00     1505     2375
    #> sd(metac2zero1diff_Intercept)     0.51 1.00     1124     1142
    #> sd(metac2zero2diff_Intercept)     0.41 1.00      786      839
    #> sd(metac2one1diff_Intercept)      0.54 1.00      685      609
    #> sd(metac2one2diff_Intercept)      0.55 1.00      894      817
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI
    #> Intercept                    -0.30      0.09    -0.48    -0.14
    #> dprime_Intercept              1.16      0.17     0.79     1.45
    #> c_Intercept                  -0.02      0.13    -0.27     0.25
    #> metac2zero1diff_Intercept    -0.85      0.06    -0.97    -0.72
    #> metac2zero2diff_Intercept    -0.80      0.07    -0.94    -0.65
    #> metac2one1diff_Intercept     -0.91      0.07    -1.05    -0.75
    #> metac2one2diff_Intercept     -0.80      0.07    -0.93    -0.66
    #>                           Rhat Bulk_ESS Tail_ESS
    #> Intercept                 1.00     3517     3090
    #> dprime_Intercept          1.00      963     1829
    #> c_Intercept               1.00      424      912
    #> metac2zero1diff_Intercept 1.00     3253     3034
    #> metac2zero2diff_Intercept 1.00     2152     2509
    #> metac2one1diff_Intercept  1.00     2037     2299
    #> metac2one2diff_Intercept  1.00     2434     2963
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
#>    .row joint_response response confidence           p_fa
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>
#> 1     1              1        0          3  0.654 ± 0.055
#> 2     1              2        0          2  0.470 ± 0.060
#> 3     1              3        0          1  0.291 ± 0.053
#> 4     1              4        1          1  0.179 ± 0.041
#> 5     1              5        1          2  0.091 ± 0.027
#> # ℹ 1 more variable: p_hit <rvar[1d]>
```

The process is exactly the same for the categorical model:

``` r
roc1_rvars(m.categorical, tibble(.row = 1), re_formula = NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence          p_fa
#>   <int>          <int>    <int>      <dbl>    <rvar[1d]>
#> 1     1              1        0          3  0.65 ± 0.056
#> 2     1              2        0          2  0.47 ± 0.060
#> 3     1              3        0          1  0.29 ± 0.054
#> 4     1              4        1          1  0.18 ± 0.042
#> 5     1              5        1          2  0.09 ± 0.026
#> # ℹ 1 more variable: p_hit <rvar[1d]>
```

Next, to get participant-level ROCs (averaging over items), we can use a
dataset with one row per participant and only the participant-level
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
#>     .row  item joint_response response confidence          p_fa
#>    <int> <int>          <int>    <int>      <dbl>    <rvar[1d]>
#>  1     1     1              1        0          3  0.82 ± 0.044
#>  2     2     2              1        0          3  0.38 ± 0.066
#>  3     3     3              1        0          3  0.33 ± 0.063
#>  4     4     4              1        0          3  0.71 ± 0.057
#>  5     5     5              1        0          3  0.76 ± 0.051
#>  6     6     6              1        0          3  0.57 ± 0.066
#>  7     7     7              1        0          3  0.73 ± 0.056
#>  8     8     8              1        0          3  0.54 ± 0.067
#>  9     9     9              1        0          3  0.59 ± 0.064
#> 10    10    10              1        0          3  0.74 ± 0.053
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
