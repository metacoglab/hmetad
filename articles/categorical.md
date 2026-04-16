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
    #>    participant  item trial stimulus response correct confidence dprime       c meta_dprime     M
    #>          <int> <int> <int>    <int>    <int>   <int>      <int>  <dbl>   <dbl>       <dbl> <dbl>
    #>  1           1     1     1        0        0       1          3  1.16   0.585        0.895 0.774
    #>  2           1     1     1        1        0       0          1  1.16   0.585        0.895 0.774
    #>  3           1     2     1        0        1       0          3  0.633 -0.0996       0.466 0.736
    #>  4           1     2     1        1        1       1          1  0.633 -0.0996       0.466 0.736
    #>  5           1     3     1        0        0       1          3  0.548  0.788        0.431 0.787
    #>  6           1     3     1        1        0       0          3  0.548  0.788        0.431 0.787
    #>  7           1     4     1        0        1       0          1  0.686  0.497        0.485 0.707
    #>  8           1     4     1        1        1       1          2  0.686  0.497        0.485 0.707
    #>  9           1     5     1        0        0       1          2  0.487 -0.182        0.383 0.788
    #> 10           1     5     1        1        1       1          3  0.487 -0.182        0.383 0.788
    #> # ℹ 2,490 more rows
    #> # ℹ 5 more variables: meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>, theta_1 <dbl>, theta_2 <dbl>

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
#> `hmetad` has inferred that there are K=3 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> # A tibble: 1,250 × 5
#>    participant  item   N_0   N_1 N[,"N_0_1"] [,"N_0_2"] [,"N_0_3"] [,"N_0_4"] [,"N_0_5"] [,"N_0_6"]
#>          <int> <int> <int> <int>       <int>      <int>      <int>      <int>      <int>      <int>
#>  1           1     1     1     1           1          0          0          0          0          0
#>  2           1     2     1     1           0          0          0          0          0          1
#>  3           1     3     1     1           1          0          0          0          0          0
#>  4           1     4     1     1           0          0          0          1          0          0
#>  5           1     5     1     1           0          1          0          0          0          0
#>  6           1     6     1     1           0          0          1          0          0          0
#>  7           1     7     1     1           1          0          0          0          0          0
#>  8           1     8     1     1           0          0          1          0          0          0
#>  9           1     9     1     1           0          0          1          0          0          0
#> 10           1    10     1     1           1          0          0          0          0          0
#> # ℹ 1,240 more rows
#> # ℹ 1 more variable: N[7:12] <int>
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
#> `hmetad` has inferred that there are K=3 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
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
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.09      0.07     0.00     0.26 1.00     1686     1558
    #> sd(dprime_Intercept)              0.74      0.19     0.45     1.18 1.01      631     1424
    #> sd(c_Intercept)                   0.81      0.13     0.60     1.11 1.01      628     1218
    #> sd(metac2zero1diff_Intercept)     0.16      0.10     0.01     0.37 1.00      930     1186
    #> sd(metac2zero2diff_Intercept)     0.11      0.08     0.00     0.30 1.00     1008     1218
    #> sd(metac2one1diff_Intercept)      0.31      0.11     0.12     0.55 1.00     1151     1478
    #> sd(metac2one2diff_Intercept)      0.13      0.09     0.01     0.32 1.00     1006     1581
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.14      0.09     0.01     0.34 1.00      991     1483
    #> sd(dprime_Intercept)              0.50      0.09     0.34     0.69 1.00     1203     2152
    #> sd(c_Intercept)                   0.34      0.05     0.26     0.44 1.00     1397     2103
    #> sd(metac2zero1diff_Intercept)     0.17      0.09     0.01     0.36 1.00      836     1386
    #> sd(metac2zero2diff_Intercept)     0.18      0.10     0.01     0.38 1.01      658     1015
    #> sd(metac2one1diff_Intercept)      0.29      0.09     0.10     0.48 1.00     1031     1191
    #> sd(metac2one2diff_Intercept)      0.16      0.10     0.01     0.36 1.01      722      951
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> Intercept                    -0.30      0.08    -0.46    -0.14 1.00     3087     2868
    #> dprime_Intercept              1.10      0.19     0.69     1.43 1.01      728     1186
    #> c_Intercept                   0.05      0.14    -0.22     0.34 1.02      505      915
    #> metac2zero1diff_Intercept    -0.81      0.06    -0.93    -0.68 1.00     2382     2520
    #> metac2zero2diff_Intercept    -0.79      0.06    -0.90    -0.67 1.00     2739     2570
    #> metac2one1diff_Intercept     -0.77      0.07    -0.92    -0.62 1.00     2093     2708
    #> metac2one2diff_Intercept     -0.79      0.06    -0.91    -0.67 1.00     2987     2527
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
    #>    participant  item trial stimulus joint_response response correct confidence dprime       c
    #>          <int> <int> <int>    <int>          <int>    <int>   <int>      <int>  <dbl>   <dbl>
    #>  1           1     1     1        0              1        0       1          3  1.16   0.585 
    #>  2           1     1     1        1              3        0       0          1  1.16   0.585 
    #>  3           1     2     1        0              6        1       0          3  0.633 -0.0996
    #>  4           1     2     1        1              4        1       1          1  0.633 -0.0996
    #>  5           1     3     1        0              1        0       1          3  0.548  0.788 
    #>  6           1     3     1        1              1        0       0          3  0.548  0.788 
    #>  7           1     4     1        0              4        1       0          1  0.686  0.497 
    #>  8           1     4     1        1              5        1       1          2  0.686  0.497 
    #>  9           1     5     1        0              2        0       1          2  0.487 -0.182 
    #> 10           1     5     1        1              6        1       1          3  0.487 -0.182 
    #> # ℹ 2,490 more rows
    #> # ℹ 7 more variables: meta_dprime <dbl>, M <dbl>, meta_c2_0 <list>, meta_c2_1 <list>, theta <dbl>,
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
#> `hmetad` has inferred that there are K=3 confidence levels in the data. If this is incorrect, please set this manually using the argument `K=<K>`
#> Compiling Stan program...
#> Start sampling
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
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
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.09      0.07     0.00     0.25 1.00     1386     1107
    #> sd(dprime_Intercept)              0.75      0.18     0.46     1.18 1.00      742     1584
    #> sd(c_Intercept)                   0.82      0.13     0.61     1.12 1.00      647     1330
    #> sd(metac2zero1diff_Intercept)     0.15      0.10     0.01     0.36 1.01      792      657
    #> sd(metac2zero2diff_Intercept)     0.12      0.08     0.01     0.30 1.00     1310     1638
    #> sd(metac2one1diff_Intercept)      0.31      0.11     0.09     0.54 1.01      684      438
    #> sd(metac2one2diff_Intercept)      0.13      0.09     0.01     0.33 1.00     1023     1497
    #> 
    #> ~participant (Number of levels: 50) 
    #>                               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> sd(Intercept)                     0.14      0.09     0.01     0.34 1.01     1081     1547
    #> sd(dprime_Intercept)              0.50      0.09     0.34     0.68 1.00     1268     2400
    #> sd(c_Intercept)                   0.34      0.05     0.26     0.44 1.00     1419     2662
    #> sd(metac2zero1diff_Intercept)     0.17      0.10     0.01     0.37 1.00      767     1164
    #> sd(metac2zero2diff_Intercept)     0.18      0.10     0.01     0.39 1.00      698      986
    #> sd(metac2one1diff_Intercept)      0.30      0.09     0.12     0.48 1.00     1312     1522
    #> sd(metac2one2diff_Intercept)      0.16      0.09     0.01     0.36 1.00      751      856
    #> 
    #> Regression Coefficients:
    #>                           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    #> Intercept                    -0.30      0.08    -0.47    -0.14 1.00     3183     2891
    #> dprime_Intercept              1.11      0.19     0.68     1.44 1.00      718     1468
    #> c_Intercept                   0.07      0.14    -0.22     0.33 1.01      388      856
    #> metac2zero1diff_Intercept    -0.81      0.06    -0.94    -0.69 1.00     2547     2277
    #> metac2zero2diff_Intercept    -0.79      0.06    -0.90    -0.67 1.00     2723     2413
    #> metac2one1diff_Intercept     -0.77      0.07    -0.91    -0.62 1.00     1745     2359
    #> metac2one2diff_Intercept     -0.78      0.06    -0.90    -0.67 1.00     3046     2716
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
#>    .row joint_response response confidence           p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.643 ± 0.060  0.91 ± 0.029
#> 2     1              2        0          2  0.457 ± 0.064  0.82 ± 0.045
#> 3     1              3        0          1  0.276 ± 0.056  0.69 ± 0.060
#> 4     1              4        1          1  0.154 ± 0.040  0.50 ± 0.066
#> 5     1              5        1          2  0.074 ± 0.024  0.31 ± 0.058
```

The process is exactly the same for the categorical model:

``` r
roc1_rvars(m.categorical, tibble(.row = 1), re_formula = NA)
#> # A tibble: 5 × 6
#> # Groups:   .row, joint_response, response, confidence [5]
#>    .row joint_response response confidence           p_fa         p_hit
#>   <int>          <int>    <int>      <dbl>     <rvar[1d]>    <rvar[1d]>
#> 1     1              1        0          3  0.638 ± 0.060  0.91 ± 0.028
#> 2     1              2        0          2  0.451 ± 0.064  0.81 ± 0.044
#> 3     1              3        0          1  0.271 ± 0.056  0.69 ± 0.058
#> 4     1              4        1          1  0.150 ± 0.040  0.49 ± 0.063
#> 5     1              5        1          2  0.071 ± 0.023  0.31 ± 0.055
```

Next, to get participant-level ROCs (averaging over items), we can use a
dataset with one row per participant and only the participant-level
random effects:

``` r
roc1_rvars(m.categorical, distinct(d, participant), re_formula = ~ (1 | participant))
#> # A tibble: 250 × 7
#> # Groups:   .row, participant, joint_response, response, confidence [250]
#>     .row participant joint_response response confidence          p_fa         p_hit
#>    <int>       <int>          <int>    <int>      <dbl>    <rvar[1d]>    <rvar[1d]>
#>  1     1           1              1        0          3  0.68 ± 0.089  0.82 ± 0.068
#>  2     2           2              1        0          3  0.59 ± 0.099  0.94 ± 0.032
#>  3     3           3              1        0          3  0.71 ± 0.086  0.90 ± 0.047
#>  4     4           4              1        0          3  0.62 ± 0.093  0.81 ± 0.069
#>  5     5           5              1        0          3  0.83 ± 0.065  0.97 ± 0.021
#>  6     6           6              1        0          3  0.39 ± 0.095  0.76 ± 0.080
#>  7     7           7              1        0          3  0.77 ± 0.078  0.89 ± 0.049
#>  8     8           8              1        0          3  0.55 ± 0.099  0.91 ± 0.043
#>  9     9           9              1        0          3  0.58 ± 0.098  0.93 ± 0.038
#> 10    10          10              1        0          3  0.69 ± 0.089  0.95 ± 0.030
#> # ℹ 240 more rows
```

We can use a similar process to get item-level ROCs (averaging over
participants):

``` r
roc1_rvars(m.categorical, distinct(d, item), re_formula = ~ (1 | item))
#> # A tibble: 125 × 7
#> # Groups:   .row, item, joint_response, response, confidence [125]
#>     .row  item joint_response response confidence          p_fa         p_hit
#>    <int> <int>          <int>    <int>      <dbl>    <rvar[1d]>    <rvar[1d]>
#>  1     1     1              1        0          3  0.42 ± 0.065  0.96 ± 0.016
#>  2     2     2              1        0          3  0.72 ± 0.055  0.96 ± 0.017
#>  3     3     3              1        0          3  0.47 ± 0.065  0.87 ± 0.037
#>  4     4     4              1        0          3  0.45 ± 0.068  0.91 ± 0.029
#>  5     5     5              1        0          3  0.82 ± 0.043  0.97 ± 0.013
#>  6     6     6              1        0          3  0.81 ± 0.045  0.98 ± 0.010
#>  7     7     7              1        0          3  0.48 ± 0.067  0.83 ± 0.044
#>  8     8     8              1        0          3  0.59 ± 0.063  0.97 ± 0.015
#>  9     9     9              1        0          3  0.42 ± 0.068  0.90 ± 0.031
#> 10    10    10              1        0          3  0.56 ± 0.065  0.94 ± 0.023
#> # ℹ 115 more rows
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
