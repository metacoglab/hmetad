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
K <- 3   ## number of confidence levels
mu_log_M <- -0.5
mu_dprime <- 1.5
mu_c <- 0
mu_c2_0 <- rep(-1, K-1)
mu_c2_1 <- rep(-1, K-1)

## participant-level standard deviations
sd_log_M_participant <- 0.25
sd_dprime_participant <- 0.5
sd_c_participant <- 0.33
sd_c2_0_participant <- cov_matrix(rep(0.25, K-1), diag(K-1))
sd_c2_1_participant <- cov_matrix(rep(0.25, K-1), diag(K-1))

## item-level standard deviations
sd_log_M_item <- 0.1
sd_dprime_item <- 0.5
sd_c_item <- 0.75
sd_c2_0_item <- cov_matrix(rep(0.1, K-1), diag(K-1))
sd_c2_1_item <- cov_matrix(rep(0.1, K-1), diag(K-1))



## simulate data
d <- expand_grid(participant=1:50,
            item=1:10) |>
  ## simulate participant-level differences
  group_by(participant) |>
  mutate(z_log_M_participant=rnorm(1, sd=sd_log_M_participant),
         z_dprime_participant=rnorm(1, sd=sd_dprime_participant),
         z_c_participant=rnorm(1, sd=sd_c_participant),
         z_c2_0_participant=list(rmulti_normal(1, mu=rep(0,K-1), Sigma=sd_c2_0_participant)),
         z_c2_1_participant=list(rmulti_normal(1, mu=rep(0,K-1), Sigma=sd_c2_1_participant)))|>
  ## simulate item-level differences
  group_by(item) |>
  mutate(z_log_M_item=rnorm(1, sd=sd_log_M_item),
         z_dprime_item=rnorm(1, sd=sd_dprime_item),
         z_c_item=rnorm(1, sd=sd_c_item),
         z_c2_0_item=list(rmulti_normal(1, mu=rep(0,K-1), Sigma=sd_c2_0_item)),
         z_c2_1_item=list(rmulti_normal(1, mu=rep(0,K-1), Sigma=sd_c2_1_item))) |>
  ungroup() |>
  ## compute model parameters
  mutate(log_M = mu_log_M + z_log_M_participant + z_log_M_item,
         dprime = mu_dprime + z_dprime_participant + z_dprime_item,
         c = mu_c + z_c_participant + z_c_item,
         c2_0_diff = map2(z_c2_0_participant, z_c2_0_item, 
                          ~ exp(mu_c2_0 + .x + .y)),
         c2_1_diff = map2(z_c2_1_participant, z_c2_1_item,
                          ~ exp(mu_c2_1 + .x + .y))) |>
  ## simulate two trials per participant/item (stimulus = 0 and stimulus = 1)
  mutate(trial=pmap(list(dprime, c, log_M, c2_0_diff, c2_1_diff), sim_metad, N_trials=2)) |>
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
from fitting the model like normal:

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
  file = "models/multinomial.rds"
)
```

    #> Warning: There were 37 divergent transitions after warmup. Increasing
    #> adapt_delta above 0.8 may help. See
    #> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
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
    #>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    #>          total post-warmup draws = 4000
    #> 
    #> Multilevel Hyperparameters:
    #> ~item (Number of levels: 10) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.21      0.17     0.01     0.65 1.00
    #> sd(dprime_Intercept)              0.68      0.23     0.35     1.25 1.00
    #> sd(c_Intercept)                   0.75      0.22     0.45     1.31 1.00
    #> sd(metac2zero1diff_Intercept)     0.20      0.15     0.01     0.57 1.00
    #> sd(metac2zero2diff_Intercept)     0.26      0.16     0.02     0.63 1.00
    #> sd(metac2one1diff_Intercept)      0.15      0.12     0.01     0.44 1.00
    #> sd(metac2one2diff_Intercept)      0.31      0.18     0.03     0.72 1.00
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                     1342     1942
    #> sd(dprime_Intercept)              1498     2072
    #> sd(c_Intercept)                   1226     1701
    #> sd(metac2zero1diff_Intercept)     1530     1860
    #> sd(metac2zero2diff_Intercept)     1285     1961
    #> sd(metac2one1diff_Intercept)      1926     2318
    #> sd(metac2one2diff_Intercept)      1258     1574
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.35      0.18     0.03     0.72 1.00
    #> sd(dprime_Intercept)              0.55      0.14     0.28     0.83 1.00
    #> sd(c_Intercept)                   0.36      0.07     0.25     0.50 1.00
    #> sd(metac2zero1diff_Intercept)     0.25      0.15     0.02     0.57 1.00
    #> sd(metac2zero2diff_Intercept)     0.29      0.15     0.02     0.59 1.00
    #> sd(metac2one1diff_Intercept)      0.29      0.16     0.02     0.62 1.00
    #> sd(metac2one2diff_Intercept)      0.51      0.14     0.24     0.79 1.00
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                      916     1347
    #> sd(dprime_Intercept)              1629     1738
    #> sd(c_Intercept)                   1595     2383
    #> sd(metac2zero1diff_Intercept)      967     1234
    #> sd(metac2zero2diff_Intercept)      776     1176
    #> sd(metac2one1diff_Intercept)      1049     1597
    #> sd(metac2one2diff_Intercept)      1434     1759
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                    -0.37      0.18    -0.80    -0.05 1.00     2412
    #> dprime_Intercept              1.77      0.27     1.25     2.30 1.00     1435
    #> c_Intercept                  -0.10      0.27    -0.63     0.42 1.00      808
    #> metac2zero1diff_Intercept    -1.05      0.14    -1.34    -0.78 1.00     3336
    #> metac2zero2diff_Intercept    -0.76      0.14    -1.05    -0.50 1.00     2484
    #> metac2one1diff_Intercept     -1.03      0.13    -1.31    -0.78 1.00     2901
    #> metac2one2diff_Intercept     -1.09      0.18    -1.48    -0.76 1.00     2147
    #>                           Tail_ESS
    #> Intercept                     2151
    #> dprime_Intercept              1820
    #> c_Intercept                   1285
    #> metac2zero1diff_Intercept     2472
    #> metac2zero2diff_Intercept     2510
    #> metac2one1diff_Intercept      3131
    #> metac2one2diff_Intercept      2244
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
  relocate(joint_response, .after="stimulus")
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
  data = d, categorical=TRUE, init = 0,
  file = "models/categorical.rds"
)
```

    #> Warning: There were 24 divergent transitions after warmup. Increasing
    #> adapt_delta above 0.8 may help. See
    #> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
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
    #>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    #>          total post-warmup draws = 4000
    #> 
    #> Multilevel Hyperparameters:
    #> ~item (Number of levels: 10) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.21      0.18     0.01     0.64 1.00
    #> sd(dprime_Intercept)              0.67      0.23     0.35     1.22 1.00
    #> sd(c_Intercept)                   0.76      0.22     0.45     1.31 1.00
    #> sd(metac2zero1diff_Intercept)     0.19      0.14     0.01     0.53 1.00
    #> sd(metac2zero2diff_Intercept)     0.26      0.15     0.02     0.62 1.00
    #> sd(metac2one1diff_Intercept)      0.15      0.12     0.01     0.45 1.00
    #> sd(metac2one2diff_Intercept)      0.32      0.18     0.03     0.72 1.01
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                     1395     2061
    #> sd(dprime_Intercept)              1675     2347
    #> sd(c_Intercept)                   1424     2120
    #> sd(metac2zero1diff_Intercept)     1361     1735
    #> sd(metac2zero2diff_Intercept)     1216     1557
    #> sd(metac2one1diff_Intercept)      1752     2299
    #> sd(metac2one2diff_Intercept)       976     1333
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat
    #> sd(Intercept)                     0.34      0.18     0.03     0.72 1.01
    #> sd(dprime_Intercept)              0.55      0.15     0.25     0.85 1.00
    #> sd(c_Intercept)                   0.37      0.06     0.25     0.50 1.00
    #> sd(metac2zero1diff_Intercept)     0.25      0.15     0.01     0.58 1.00
    #> sd(metac2zero2diff_Intercept)     0.28      0.15     0.02     0.58 1.01
    #> sd(metac2one1diff_Intercept)      0.29      0.16     0.02     0.61 1.00
    #> sd(metac2one2diff_Intercept)      0.51      0.14     0.22     0.80 1.00
    #>                               Bulk_ESS Tail_ESS
    #> sd(Intercept)                      926     1363
    #> sd(dprime_Intercept)              1126     1245
    #> sd(c_Intercept)                   1573     2690
    #> sd(metac2zero1diff_Intercept)      952     1570
    #> sd(metac2zero2diff_Intercept)      832     1419
    #> sd(metac2one1diff_Intercept)       738     1047
    #> sd(metac2one2diff_Intercept)      1200      887
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    #> Intercept                    -0.37      0.19    -0.79    -0.04 1.00     2533
    #> dprime_Intercept              1.76      0.26     1.27     2.26 1.00     1625
    #> c_Intercept                  -0.08      0.26    -0.60     0.42 1.00      954
    #> metac2zero1diff_Intercept    -1.04      0.14    -1.35    -0.80 1.00     2738
    #> metac2zero2diff_Intercept    -0.76      0.14    -1.04    -0.49 1.00     2572
    #> metac2one1diff_Intercept     -1.03      0.13    -1.31    -0.79 1.00     3293
    #> metac2one2diff_Intercept     -1.10      0.19    -1.48    -0.76 1.00     2107
    #>                           Tail_ESS
    #> Intercept                     1740
    #> dprime_Intercept              1838
    #> c_Intercept                   1633
    #> metac2zero1diff_Intercept     2544
    #> metac2zero2diff_Intercept     2462
    #> metac2one1diff_Intercept      2871
    #> metac2one2diff_Intercept      1817
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
roc1_rvars(m.multinomial, tibble(.row=1), re_formula=NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence           p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.573 ± 0.107  0.95 ± 0.034
#> 2     1              2        0          2  0.372 ± 0.101  0.89 ± 0.056
#> 3     1              3        0          1  0.227 ± 0.085  0.83 ± 0.075
#> 4     1              4        1          1  0.147 ± 0.067  0.69 ± 0.097
#> 5     1              5        1          2  0.089 ± 0.049  0.55 ± 0.109
```

The process is exactly the same for the categorical model:

``` r
roc1_rvars(m.categorical, tibble(.row=1), re_formula=NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence           p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.567 ± 0.105  0.95 ± 0.032
#> 2     1              2        0          2  0.365 ± 0.100  0.89 ± 0.055
#> 3     1              3        0          1  0.221 ± 0.082  0.82 ± 0.074
#> 4     1              4        1          1  0.142 ± 0.065  0.69 ± 0.094
#> 5     1              5        1          2  0.086 ± 0.048  0.55 ± 0.106
```

Next, to get participant-level ROCs (averaging over items), we can use a
data set with one row per participant and only the participant-level
random effects:

``` r
roc1_rvars(m.categorical, distinct(d, participant), re_formula=~ (1 | participant))
#> # A tibble: 250 × 7
#> # Groups:   .row, participant, joint_response, response, confidence [250]
#>     .row participant joint_response response confidence         p_fa
#>    <int>       <int>          <int>    <int>      <dbl>   <rvar[1d]>
#>  1     1           1              1        0          3  0.50 ± 0.15
#>  2     2           2              1        0          3  0.57 ± 0.14
#>  3     3           3              1        0          3  0.78 ± 0.12
#>  4     4           4              1        0          3  0.74 ± 0.13
#>  5     5           5              1        0          3  0.76 ± 0.12
#>  6     6           6              1        0          3  0.49 ± 0.15
#>  7     7           7              1        0          3  0.81 ± 0.11
#>  8     8           8              1        0          3  0.43 ± 0.15
#>  9     9           9              1        0          3  0.54 ± 0.15
#> 10    10          10              1        0          3  0.60 ± 0.14
#> # ℹ 240 more rows
#> # ℹ 1 more variable: p_hit <rvar[1d]>
```

We can use a similar process to get item-level ROCs (averaging over
participants):

``` r
roc1_rvars(m.categorical, distinct(d, item), re_formula=~ (1 | item))
#> # A tibble: 50 × 7
#> # Groups:   .row, item, joint_response, response, confidence [50]
#>     .row  item joint_response response confidence          p_fa          p_hit
#>    <int> <int>          <int>    <int>      <dbl>    <rvar[1d]>     <rvar[1d]>
#>  1     1     1              1        0          3  0.44 ± 0.065  0.86 ± 0.0403
#>  2     2     2              1        0          3  0.63 ± 0.068  0.99 ± 0.0056
#>  3     3     3              1        0          3  0.82 ± 0.048  0.99 ± 0.0075
#>  4     4     4              1        0          3  0.75 ± 0.063  1.00 ± 0.0022
#>  5     5     5              1        0          3  0.66 ± 0.064  0.98 ± 0.0108
#>  6     6     6              1        0          3  0.21 ± 0.057  0.77 ± 0.0549
#>  7     7     7              1        0          3  0.67 ± 0.063  0.83 ± 0.0463
#>  8     8     8              1        0          3  0.77 ± 0.058  0.99 ± 0.0071
#>  9     9     9              1        0          3  0.55 ± 0.068  0.96 ± 0.0195
#> 10    10    10              1        0          3  0.20 ± 0.054  0.68 ± 0.0633
#> # ℹ 40 more rows
```

## Other benefits

Aside from representing the data in a more convenient format, the
trial-level model should be more useful for things like model comparison
using the `loo` package, multivariate models, and mediation models.
These features should mostly work out the box but they are still under
active development, so stay tuned!
